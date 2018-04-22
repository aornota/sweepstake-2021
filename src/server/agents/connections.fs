module Aornota.Sweepstake2018.Server.Agents.Connections

open Aornota.Sweepstake2018.Server.Json
open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui

open System
open System.Net.WebSockets
open System.Text
open System.Threading

open FSharp.Control.Tasks.ContextInsensitive

type ConnectionId = | ConnectionId of guid : Guid with static member Create () = Guid.NewGuid () |> ConnectionId

type private ConnectionsInput =
    | AddConnection of connectionId : ConnectionId * ws : WebSocket
    | RemoveConnection of connectionId : ConnectionId
    | OnReceiveError of connectionId : ConnectionId * exn : exn
    | OnDeserializeUiWsApiError of connectionId : ConnectionId * exn : exn
    | HandleUiWsApi of connectionId : ConnectionId * uiWsApi : UiWsApi

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
let private fakeErrorFrequency = 0.2
#endif

type ConnectionsAgent () =
    let remove connectionIds connections = connections |> List.filter (fun connection -> not (connectionIds |> List.contains connection.ConnectionId))
    let sendWs (serverWsApi:ServerWsApi) sendFilter connections = async {
        let json = toJson serverWsApi
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
    // TEMP-NMB?: Might not be needed once SignInWs handled properly, e.g. by using real data...
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
    let idAsync x = async { return x }
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec managing (connections:Connection list) = async {
            let! input = inbox.Receive ()
#if DEBUG
            do! Async.Sleep (random.Next (100, 500))
#endif
            match input with
            | AddConnection (connectionId, ws) ->
                // SNH-NMB: What if connectionId already in connections? (&c.)...
                let otherConnections = connections |> List.length
                let signedIn =
                    connections
                    |> List.choose (fun connection -> connection.AuthUserSession |> Option.map (fun authUserSession -> authUserSession.UserId))
                    |> List.distinct
                    |> List.length
                let connections = { ConnectionId = connectionId ; Ws = ws ; AuthUserSession = None } :: connections              
                let! connections = sendWs (ConnectedWs (otherConnections, signedIn) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                return! managing connections
            | RemoveConnection connectionId ->
                // SNH-NMB: What if connectionId not in connections? (&c.)...
                let userIdAndName = connections |> userIdAndNameForConnectionId connectionId
                let connections = connections |> remove [ connectionId ]
                let isFullySignedOut = match userIdAndName with | Some (userId, _) -> connections |> countForUserId userId = 0 | None -> false
                let! connections =
                    if isFullySignedOut then
                        let userId, userName = match userIdAndName with | Some (userId, userName) -> (userId, userName) | None -> UserId.Create (), "yves strop" // SNH-NMB
                        sendWs (OtherUserSignedOut userName |> ServerAppWsApi) (AllAuthExceptUserId userId) connections
                    else idAsync connections
                return! managing connections
            | OnReceiveError (connectionId, exn) ->
                let! connections = sendWs (ServerWsErrorWs(ReceiveError exn.Message) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                return! managing connections
            | OnDeserializeUiWsApiError (connectionId, exn) ->                
                let! connections = sendWs (ServerWsErrorWs(DeserializeUiWsApiError exn.Message) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections  
                return! managing connections
            | HandleUiWsApi (connectionId, uiWsApi) ->
                match uiWsApi with
                | UiUnauthWsApi (SignInWs (sessionId, userName, _password)) ->
                    // SNH-NMB: What if connectionId not in connections? What if already have AuthUserSession for sessionId? (&c.)...
                    // TODO-NMB-MEDIUM: Handle properly, e.g. by checking userName and _password against real data...
                    let userId, alreadySignedIn = match connections |> userIdForUserName userName with | Some userId -> userId, true | None -> UserId.Create (), false
                    let authUser = { UserId = userId ; SessionId = sessionId ; UserName = userName }
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake SignInWs error -> %A" authUser)
                        else if userName = "ann ewity" then Error ("Username is reserved")
                        else Ok authUser
#else
                        Ok authUser
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signIn (connectionId, authUser.UserId, authUser.SessionId, authUser.UserName) else connections
                    let! connections = sendWs (SignInResultWs result |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk && not alreadySignedIn then
                            sendWs (OtherUserSignedIn authUser.UserName |> ServerAppWsApi) (AllAuthExceptUserId authUser.UserId) connections
                        else idAsync connections
                    return! managing connections
                | UiUnauthWsApi (AutoSignInWs (Jwt jwt)) ->
                    // SNH-NMB: What if connectionId not in connections? (&c.)...
                    // TODO-NMB-MEDIUM: Handle properly, e.g. by verifying jwt (i.e. can decrypt | details match [inc. permissions] | &c.)...
                    let authUser = jwt
                    let alreadySignedIn = connections |> countForUserId jwt.UserId > 0
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake AutoSignInWs error -> %A" authUser) else Ok authUser
#else
                        Ok authUser
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signIn (connectionId, authUser.UserId, authUser.SessionId, authUser.UserName) else connections
                    let! connections = sendWs (AutoSignInResultWs result |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk && not alreadySignedIn then
                            sendWs (OtherUserSignedIn authUser.UserName |> ServerAppWsApi) (AllAuthExceptUserId authUser.UserId) connections
                        else idAsync connections
                    return! managing connections
                | UiAuthWsApi (Jwt jwt, SignOutWs) ->       
                    // SNH-NMB: What if connectionId not in connections? What if no [or mismatched?] AuthUserSession for connectionId? (&c.)...
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (sprintf "Fake SignOutWs error -> %A" jwt) else Ok jwt.SessionId
#else
                        Ok jwt.SessionId
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    // Note: Okay to sign out connection-for-connectionId before sending SignOutResultWs (since will always match OnlyConnectedId, even if signed-out).
                    let connections = if isOk then connections |> signOut (connectionId, jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendWs (SignOutResultWs result |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                    let! connections = 
                        if isOk then sendWs (AutoSignOutWs jwt.SessionId |> ServerAppWsApi) (SameUserSessionExceptConnectionId (jwt.UserId, jwt.SessionId, connectionId)) connections
                        else idAsync connections
                    // Note: Auto-sign out connections-for-same-user-session-except-ConnectionId after sending AutoSignOutWs (else would not match SameUserSessionExceptConnectionId once signed-out).
                    let connections = if isOk then connections |> autoSignOut (jwt.UserId, jwt.SessionId) else connections
                    let isFullySignedOut = if isOk then connections |> countForUserId jwt.UserId = 0 else false                    
                    let! connections =
                        if isFullySignedOut then sendWs (OtherUserSignedOut jwt.UserName |> ServerAppWsApi) (AllAuthExceptUserId jwt.UserId) connections
                        else idAsync connections
                    return! managing connections
                | UiAuthWsApi (Jwt jwt, (SendChatMessageWs chatMessage)) ->
                    // SNH-NMB: What if connectionId not in connections? What if no AuthUserSession for connectionId? (&c.)...
                    let result =
#if DEBUG
                        if random.NextDouble () < fakeErrorFrequency then Error (chatMessage.ChatMessageId, (sprintf "Fake SendChatMessageWs error -> %A -> %A" jwt chatMessage)) else Ok chatMessage
#else
                        Ok chatMessage
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> updateLastApi (connectionId, jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendWs (SendChatMessageResultWs result |> ServerChatWsApi) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk then sendWs (OtherUserChatMessageWs chatMessage |> ServerChatWsApi) (AllAuthExceptConnectionId connectionId) connections
                        else idAsync connections
                    return! managing connections }
        managing [])
    member __.AddConnection (connectionId, ws) = agent.Post (AddConnection (connectionId, ws))
    member __.RemoveConnection connectionId = agent.Post (RemoveConnection connectionId)
    member __.OnReceiveError (connectionId, exn) = agent.Post (OnReceiveError (connectionId, exn))
    member __.OnDeserializeUiWsApiError (connectionId, exn) = agent.Post (OnDeserializeUiWsApiError (connectionId, exn))
    member __.HandleUiWsApi (connectionId, uiWs) = agent.Post (HandleUiWsApi (connectionId, uiWs))

let connectionsAgent = ConnectionsAgent ()
