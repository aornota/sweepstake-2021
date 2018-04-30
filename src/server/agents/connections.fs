module Aornota.Sweepstake2018.Server.Agents.Connections

open Aornota.Common.Json

open Aornota.Server.Common.Helpers
open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.Event

open System
open System.Net.WebSockets
open System.Text
open System.Threading

open FSharp.Control.Tasks.ContextInsensitive

type private ConnectionsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | AddConnection of connectionId : ConnectionId * ws : WebSocket
    | RemoveConnection of connectionId : ConnectionId
    | OnReceiveUiMsgError of connectionId : ConnectionId * exn : exn
    | OnDeserializeUiMsgError of connectionId : ConnectionId * exn : exn
    | HandleUiMsg of connectionId : ConnectionId * uiMsg : UiMsg

type private AuthUserSession = {
    UserId : UserId
    SessionId : SessionId
    UserName : string
    LastApi : DateTime }

type private Connection = {
    ConnectionId : ConnectionId
    Ws : WebSocket
    AuthUserSession : AuthUserSession option }

type private SendFilter =
    | OnlyConnectionId of connectionId : ConnectionId
    | AllAuthExceptConnectionId of connectionId : ConnectionId
    | AllAuthExceptUserId of userId : UserId
    | SameUserSessionExceptConnectionId of userId : UserId * sessionId : SessionId * connectionId : ConnectionId

#if DEBUG
let private random = Random ()
let private fakeErrorFrequency = 0.02
#endif

let private log category = consoleLogger.Log (Connections, category)

type Connections () = // TODO-NMB-HIGH: More detailed logging (including Verbose stuff?)...
    // TODO-NMB-HIGH: As part of rework, move these functions to be [private] module-level ones...
    let remove connectionIds connections = connections |> List.filter (fun connection -> not (connectionIds |> List.contains connection.ConnectionId))
    let sendMsg (serverMsg:ServerMsg) sendFilter connections = async {
        let (Json json) = serverMsg |> toJson
        let buffer = Encoding.UTF8.GetBytes json
        let segment = new ArraySegment<byte> (buffer)
        let rec send (recipients:(ConnectionId * WebSocket) list) failures = async {
            let trySendMessage (ws:WebSocket) = task {
                if ws.State = WebSocketState.Open then
                    try
                        do! ws.SendAsync (segment, WebSocketMessageType.Text, true, CancellationToken.None)
                        return true
                    with _ -> return false
                else return false }
            match recipients with
            | (connectionId, ws) :: t ->
                let! success = trySendMessage ws |> Async.AwaitTask
                return! send t (if success then failures else connectionId :: failures)
            | [] -> return failures }
        let recipients =
            match sendFilter with
            | OnlyConnectionId connectionId ->
                connections |> List.filter (fun connection -> connection.ConnectionId = connectionId)
            | AllAuthExceptConnectionId connectionId ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthUserSession with
                    | Some _ -> connection.ConnectionId <> connectionId
                    | None -> false)
            | AllAuthExceptUserId userId ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthUserSession with
                    | Some authUserSession -> authUserSession.UserId <> userId
                    | None -> false)
            | SameUserSessionExceptConnectionId (userId, sessionId, connectionId) ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthUserSession with
                    | Some authUserSession ->
                        connection.ConnectionId <> connectionId && authUserSession.UserId = userId && authUserSession.SessionId = sessionId
                    | None -> false)
        let recipients = recipients |> List.map (fun connection -> connection.ConnectionId, connection.Ws)
        let! failures = send recipients []
        return connections |> remove failures }
    let signIn (connectionId, userId, sessionId, userName) connections =
        let authUserSession = { UserId = userId ; SessionId = sessionId ; UserName = userName ; LastApi = DateTime.Now }
        connections
        |> List.map (fun connection -> if connection.ConnectionId = connectionId then { connection with AuthUserSession = Some authUserSession } else connection)
    let signOut (connectionId, userId, sessionId) connections =
        connections
        |> List.map (fun connection -> 
            match connection.AuthUserSession with
            | Some authUserSession when connection.ConnectionId = connectionId && authUserSession.UserId = userId && authUserSession.SessionId = sessionId ->
                { connection with AuthUserSession = None }
            | Some _ | None -> connection)
    let autoSignOut (userId, sessionId) connections =
        connections
        |> List.map (fun connection -> 
            match connection.AuthUserSession with
            | Some authUserSession when authUserSession.UserId = userId && authUserSession.SessionId = sessionId ->
                { connection with AuthUserSession = None }
            | Some _ | None -> connection)
    let userIdAndNameForConnectionId connectionId connections =
        connections
        |> List.choose (fun connection -> 
            match connection.AuthUserSession with
            | Some authUserSession when connection.ConnectionId = connectionId -> Some (authUserSession.UserId, authUserSession.UserName)
            | Some _ | None -> None)
        |> List.tryHead // note: expect at most one Connection per connectionId
    // TEMP-NMB?: Might not be needed once SignInMsg handled properly, e.g. by using real data...
    let userIdForUserName userName connections =
        connections
        |> List.choose (fun connection -> 
            match connection.AuthUserSession with
            | Some authUserSession when authUserSession.UserName = userName -> Some authUserSession.UserId
            | Some _ | None -> None)
        |> List.tryHead // note: expect at most one UserId per userName
    // ...NMB-TEMP?
    let countForUserId userId connections =
        connections
        |> List.filter (fun connection ->
            match connection.AuthUserSession with
            | Some authUserSession when authUserSession.UserId = userId -> true
            | Some _ | None -> false)
        |> List.length
    let updateLastApi(connectionId, userId, sessionId) connections =
        connections
        |> List.map (fun connection ->
            if connection.ConnectionId = connectionId then
                match connection.AuthUserSession with
                | Some authUserSession when authUserSession.UserId = userId && authUserSession.SessionId = sessionId ->
                    let authUserSession = { authUserSession with LastApi = DateTime.Now }
                    { connection with AuthUserSession = Some authUserSession }
                | Some _ | None -> connection
            else connection)
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                log (Info "Start when awaitingStart -> managingConnections (0 connections)")
                () |> reply.Reply
                return! managingConnections []
            | AddConnection _ -> log (Agent (IgnoredInput "AddConnection when awaitingStart")) ; return! awaitingStart ()
            | RemoveConnection _ -> log (Agent (IgnoredInput "RemoveConnection when awaitingStart")) ; return! awaitingStart ()
            | OnReceiveUiMsgError _ -> log (Agent (IgnoredInput "OnReceiveUiMsgError when awaitingStart")) ; return! awaitingStart ()
            | OnDeserializeUiMsgError _ -> log (Agent (IgnoredInput "OnDeserializeUiMsgError when awaitingStart")) ; return! awaitingStart ()
            | HandleUiMsg _ -> log (Agent (IgnoredInput "HandleUiMsg when awaitingStart")) ; return! awaitingStart () }
        and managingConnections (connections:Connection list) = async {
            let! input = inbox.Receive ()
#if DEBUG
            do! Async.Sleep (random.Next (100, 500))
#endif
            match input with
            | Start _ -> log (Agent (IgnoredInput "Start when managingConnections")) ; return! managingConnections []
            | AddConnection (connectionId, ws) ->
                // SNH-NMB: What if connectionId already in connections? (&c.)...
                let otherConnections = connections |> List.length
                let signedIn =
                    connections
                    |> List.choose (fun connection -> connection.AuthUserSession |> Option.map (fun authUserSession -> authUserSession.UserId))
                    |> List.distinct
                    |> List.length
                let connections = { ConnectionId = connectionId ; Ws = ws ; AuthUserSession = None } :: connections              
                let! connections = sendMsg (ConnectedMsg (otherConnections, signedIn) |> ServerAppMsg) (OnlyConnectionId connectionId) connections
                return! managingConnections connections
            | RemoveConnection connectionId ->
                // SNH-NMB: What if connectionId not in connections? (&c.)...
                let userIdAndName = connections |> userIdAndNameForConnectionId connectionId
                let connections = connections |> remove [ connectionId ]
                let isFullySignedOut = match userIdAndName with | Some (userId, _) -> connections |> countForUserId userId = 0 | None -> false
                let! connections =
                    if isFullySignedOut then
                        let userId, userName = match userIdAndName with | Some (userId, userName) -> (userId, userName) | None -> UserId.Create (), "yves strop" // SNH-NMB
                        sendMsg (OtherUserSignedOutMsgOLD userName |> ServerAppMsg) (AllAuthExceptUserId userId) connections
                    else thingAsync connections
                return! managingConnections connections
            | OnReceiveUiMsgError (connectionId, exn) ->
                let! connections = sendMsg (ServerUiMsgErrorMsg (ReceiveUiMsgError exn.Message) |> ServerAppMsg) (OnlyConnectionId connectionId) connections
                return! managingConnections connections
            | OnDeserializeUiMsgError (connectionId, exn) ->                
                let! connections = sendMsg (ServerUiMsgErrorMsg (DeserializeUiMsgError exn.Message) |> ServerAppMsg) (OnlyConnectionId connectionId) connections  
                return! managingConnections connections
            | HandleUiMsg (connectionId, uiMsg) ->
                match uiMsg with
                | UiUnauthMsg (SignInMsgOLD (sessionId, userName, _password)) ->
                    // SNH-NMB: What if connectionId not in connections? What if already have AuthUserSession for sessionId? (&c.)...
                    // TODO-NMB-MEDIUM: Handle properly, e.g. by checking userName and _password against real data...
                    let userId, alreadySignedIn = match connections |> userIdForUserName userName with | Some userId -> userId, true | None -> UserId.Create (), false
                    let authUser = { UserId = userId ; SessionId = sessionId ; UserName = userName }
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake SignInApi error -> %A" authUser)
                        else if userName = "ann ewity" then Error ("Username is reserved")
                        else Ok authUser
#else
                        Ok authUser
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signIn (connectionId, authUser.UserId, authUser.SessionId, authUser.UserName) else connections
                    let! connections = sendMsg (SignInResultMsg result |> ServerAppMsg) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk && not alreadySignedIn then
                            sendMsg (OtherUserSignedInMsgOLD authUser.UserName |> ServerAppMsg) (AllAuthExceptUserId authUser.UserId) connections
                        else thingAsync connections
                    return! managingConnections connections
                | UiUnauthMsg (AutoSignInMsgOLD (Jwt jwt)) ->
                    // SNH-NMB: What if connectionId not in connections? (&c.)...
                    // TODO-NMB-MEDIUM: Handle properly, e.g. by verifying jwt (i.e. can decrypt | details match [inc. permissions] | &c.)...
                    let authUser = jwt
                    let alreadySignedIn = connections |> countForUserId jwt.UserId > 0
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake AutoSignInApi error -> %A" authUser) else Ok authUser
#else
                        Ok authUser
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signIn (connectionId, authUser.UserId, authUser.SessionId, authUser.UserName) else connections
                    let! connections = sendMsg (AutoSignInResultMsg result |> ServerAppMsg) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk && not alreadySignedIn then
                            sendMsg (OtherUserSignedInMsgOLD authUser.UserName |> ServerAppMsg) (AllAuthExceptUserId authUser.UserId) connections
                        else thingAsync connections
                    return! managingConnections connections
                | UiAuthMsg (Jwt jwt, SignOutMsgOLD) ->       
                    // SNH-NMB: What if connectionId not in connections? What if no [or mismatched?] AuthUserSession for connectionId? (&c.)...
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake SignOutApi error -> %A" jwt) else Ok jwt.SessionId
#else
                        Ok jwt.SessionId
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    // Note: Okay to sign out connection-for-connectionId before sending SignOutResultMsg (since will always match OnlyConnectedId, even if signed-out).
                    let connections = if isOk then connections |> signOut (connectionId, jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendMsg (SignOutResultMsg result |> ServerAppMsg) (OnlyConnectionId connectionId) connections
                    let! connections = 
                        if isOk then sendMsg (AutoSignOutMsgOLD jwt.SessionId |> ServerAppMsg) (SameUserSessionExceptConnectionId (jwt.UserId, jwt.SessionId, connectionId)) connections
                        else thingAsync connections
                    // Note: Auto-sign out connections-for-same-user-session-except-ConnectionId after sending AutoSignOutMsg (else would not match SameUserSessionExceptConnectionId once signed-out).
                    let connections = if isOk then connections |> autoSignOut (jwt.UserId, jwt.SessionId) else connections
                    let isFullySignedOut = if isOk then connections |> countForUserId jwt.UserId = 0 else false                    
                    let! connections =
                        if isFullySignedOut then sendMsg (OtherUserSignedOutMsgOLD jwt.UserName |> ServerAppMsg) (AllAuthExceptUserId jwt.UserId) connections
                        else thingAsync connections
                    return! managingConnections connections
                | UiAuthMsg (Jwt jwt, (SendChatMessageMsgOLD chatMessage)) ->
                    // SNH-NMB: What if connectionId not in connections? What if no AuthUserSession for connectionId? (&c.)...
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (chatMessage.ChatMessageId, (sprintf "Fake SendChatMessageApi error -> %A -> %A" jwt chatMessage)) else Ok chatMessage
#else
                        Ok chatMessage
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> updateLastApi (connectionId, jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendMsg (SendChatMessageResultMsgOLD result |> ServerChatMsg) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk then sendMsg (OtherUserChatMessageMsgOLD chatMessage |> ServerChatMsg) (AllAuthExceptConnectionId connectionId) connections
                        else thingAsync connections
                    return! managingConnections connections }
        log (Info "agent instantiated -> awaitingStart")
        awaitingStart ())
    do agent.Error.Add (logAgentException Source.Connections) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member _self.Start () =
        // TODO-NMB-LOW: Subscribe to Tick (e.g. to auto-sign out "expired" sessions)?...
        // TODO-NMB-HIGH: Subscribe to UserSignedIn? UserSignedOut? - or will Connections agent cause these in the first place?...
        let onEvent = (fun event ->
            match event with
            | SendMsg (_serverMsg, _recipients) -> () // TODO-NMB-HIGH... (serverMsg, recipients) |> self.SendMsg
            | _ -> ())
        let subscriberId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        log (Info (sprintf "agent subscribed to SendMsg broadcasts -> %A" subscriberId))
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AddConnection (connectionId, ws) = AddConnection (connectionId, ws) |> agent.Post
    member __.RemoveConnection connectionId = RemoveConnection connectionId |> agent.Post
    member __.OnReceiveUiMsgError (connectionId, exn) = OnReceiveUiMsgError (connectionId, exn) |> agent.Post
    member __.OnDeserializeUiMsgError (connectionId, exn) = OnDeserializeUiMsgError (connectionId, exn) |> agent.Post
    member __.HandleUiMsg (connectionId, uiMsg) = HandleUiMsg (connectionId, uiMsg) |> agent.Post

let connections = Connections ()
