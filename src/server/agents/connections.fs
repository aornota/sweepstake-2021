module Aornota.Sweepstake2018.Server.Agents.Connections

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers
open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Jwt

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

let private log category = (Connections, category) |> consoleLogger.Log

let private logResult resultSource successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" resultSource successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" resultSource error |> Danger |> log

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
        |> List.map (fun connection -> if connection.ConnectionId = connectionId then { connection with AuthUserSession = authUserSession |> Some } else connection)
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
                    { connection with AuthUserSession = authUserSession |> Some }
                | Some _ | None -> connection
            else connection)
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> managingConnections (0 connections)" |> Info |> log
                () |> reply.Reply
                return! managingConnections []
            | AddConnection _ -> "AddConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnReceiveUiMsgError _ -> "OnReceiveUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDeserializeUiMsgError _ -> "OnDeserializeUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleUiMsg _ -> "HandleUiMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and managingConnections (connections:Connection list) = async {
            let! input = inbox.Receive ()
            do! ifDebugSleepAsync 100 500
            match input with
            | Start _ -> "Start when managingConnections" |> IgnoredInput |> Agent |> log ; return! managingConnections []
            | AddConnection (connectionId, ws) ->
                // SNH-NMB: What if connectionId already in connections? (&c.)...
                sprintf "AddConnection %A when managingConnections (%i connection/s)" connectionId connections.Length |> Verbose |> log
                let otherConnections = connections |> List.length
                let signedIn =
                    connections
                    |> List.choose (fun connection -> connection.AuthUserSession |> Option.map (fun authUserSession -> authUserSession.UserId))
                    |> List.distinct
                    |> List.length
                let connections = { ConnectionId = connectionId ; Ws = ws ; AuthUserSession = None } :: connections              
                let! connections = connections |> sendMsg ((otherConnections, signedIn) |> ConnectedMsg |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                return! managingConnections connections
            | RemoveConnection connectionId ->
                // SNH-NMB: What if connectionId not in connections? (&c.)...
                sprintf "RemoveConnection %A when managingConnections (%i connection/s)" connectionId connections.Length |> Verbose |> log
                let userIdAndName = connections |> userIdAndNameForConnectionId connectionId
                let connections = connections |> remove [ connectionId ]
                connectionId |> Disconnected |> broadcaster.Broadcast
                let isFullySignedOut = match userIdAndName with | Some (userId, _) -> connections |> countForUserId userId = 0 | None -> false
                let! connections =
                    if isFullySignedOut then
                        let userId, userName = match userIdAndName with | Some (userId, userName) -> (userId, userName) | None -> UserId.Create (), "yves strop" // SNH-NMB
                        userId |> UserSignedOut |> broadcaster.Broadcast
                        // TODO-NMB-HIGH: Remove use of OtherUserSignedInMsgOLD once handled by Chat projection...
                        connections |> sendMsg (userName |> OtherUserSignedOutMsgOLD |> ServerAppMsg) (userId |> AllAuthExceptUserId)
                    else thingAsync connections
                return! managingConnections connections
            | OnReceiveUiMsgError (connectionId, exn) ->
                sprintf "OnReceiveUiMsgError for %A when managingConnections (%i connection/s) -> %s" connectionId connections.Length exn.Message |> Danger |> log
                let! connections = connections |> sendMsg (exn.Message |> ReceiveUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                return! managingConnections connections
            | OnDeserializeUiMsgError (connectionId, exn) ->                
                sprintf "OnDeserializeUiMsgError for %A when managingConnections (%i connection/s) -> %s" connectionId connections.Length exn.Message |> Danger |> log
                let! connections = connections |> sendMsg (exn.Message |> DeserializeUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                return! managingConnections connections
            | HandleUiMsg (connectionId, uiMsg) ->
                match uiMsg with
                | UiUnauthMsg (UiUnauthAppMsg (SignInCmd (sessionId, userName, password))) ->
                    // SNH-NMB: What if connectionId not in connections? What if already have AuthUserSession for sessionId? (&c.)...
                    sprintf "SignInCmd for %A (%A) when managingConnections (%i connection/s)" userName sessionId connections.Length |> Verbose |> log
                    let alreadySignedIn =
                        let (UserName userName) = userName
                        match connections |> userIdForUserName userName with | Some _ -> true | None -> false
                    let! result =
                        if debugFakeError () then sprintf "Fake SignInCmd error -> %A (%A)" userName sessionId |> OtherError |> OtherSignInCmdError |> Error |> thingAsync
                        else if userName = UserName "ann ewity" then "Username is reserved" |> OtherError |> OtherSignInCmdError |> Error |> thingAsync
                        else (sessionId, userName, password) |> users.HandleSignInCmdAsync
                    let connections =
                        match result with
                        | Ok authUser ->
                            let (UserName userName) = authUser.UserName
                            connections |> signIn (connectionId, authUser.UserId, sessionId, userName)
                        | Error _ -> connections
                    let! connections = connections |> sendMsg (result |> SignInCmdResult |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                    let! connections =
                        match result, alreadySignedIn with
                        | Ok authUser, false ->
                            let (UserName userName) = authUser.UserName
                            authUser.UserId |> UserSignedIn |> broadcaster.Broadcast
                            // TODO-NMB-HIGH: Remove use of OtherUserSignedInMsgOLD once handled by Chat projection...
                            connections |> sendMsg (userName |> OtherUserSignedInMsgOLD |> ServerAppMsg) (authUser.UserId |> AllAuthExceptUserId)
                        | Ok _, true | Error _, _ -> connections |> thingAsync
                    result |> logResult "SignInCmd" (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                    return! managingConnections connections
                | UiUnauthMsg (UiUnauthAppMsg (AutoSignInCmd (sessionId, jwt))) ->
                    // SNH-NMB: What if connectionId not in connections? (&c.)...
                    sprintf "AutoSignInCmd for %A (%A) when managingConnections (%i connection/s)" sessionId jwt connections.Length |> Verbose |> log
                    let result =
                        if debugFakeError () then sprintf "Fake AutoSignInCmd error -> %A" jwt |> OtherError |> OtherAutoSignInCmdError |> Error
                        else
                            match jwt |> fromJwt with
                            | Ok (sessionId, userId, userName, permissions) -> (sessionId, userId, userName, permissions) |> Ok
                            | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> AutoSignInCmdJwtError |> Error
                    let! result =
                        match result with
                        | Ok (sessionId, userId, _, permissions) -> (sessionId, userId, permissions) |> users.HandleAutoSignInCmdAsync
                        | Error error -> error |> Error |> thingAsync
                    // TODO-NMB-HIGH: If Ok, verify userId | permissions | (&c.) against cache...
                    let alreadySignedIn = match result with | Ok authUser -> connections |> countForUserId authUser.UserId > 0 | Error _ -> false
                    let connections =
                        match result with
                        | Ok authUser ->
                            let (UserName userName) = authUser.UserName
                            connections |> signIn (connectionId, authUser.UserId, sessionId, userName)
                        | Error _ -> connections
                    let! connections = connections |> sendMsg (result |> AutoSignInCmdResult |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                    let! connections =
                        match result, alreadySignedIn with
                        | Ok authUser, false ->
                            let (UserName userName) = authUser.UserName
                            authUser.UserId |> UserSignedIn |> broadcaster.Broadcast
                            // TODO-NMB-HIGH: Remove use of OtherUserSignedInMsgOLD once handled by Chat projection...
                            connections |> sendMsg (userName |> OtherUserSignedInMsgOLD |> ServerAppMsg) (authUser.UserId |> AllAuthExceptUserId)
                        | Ok _, true | Error _, _ -> connections |> thingAsync
                    result |> logResult "AutoSignInCmd" (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                    return! managingConnections connections
                | UiAuthMsg (jwt, UiAuthAppMsg (ChangePasswordCmd (currentRvn, password))) ->     
                    // SNH-NMB: What if connectionId not in connections? What if no [or mismatched?] AuthUserSession for connectionId? (&c.)...
                    sprintf "ChangePasswordCmd (%A) for %A when managingConnections (%i connection/s)" currentRvn jwt connections.Length |> Verbose |> log
                    let result =
                        if debugFakeError () then sprintf "Fake ChangePasswordCmd error -> %A" jwt |> OtherError |> OtherCmdError |> Error
                        else
                            match jwt |> fromJwt with
                            | Ok (sessionId, userId, userName, permissions) -> (sessionId, userId, userName, permissions) |> Ok
                            | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> CmdJwtError |> Error
                    // TODO-NMB-HIGH: If Ok, verify userId | permissions | (&c.) against cache - and retrieve UserTokens...
                    let! result =
                        match result with
                        | Ok (_, userId, _, _) ->
                            // TEMP-NMB: Provide ChangePasswordToken once have access to it...
                            (None, userId, currentRvn, password) |> users.HandleChangePasswordCmdAsync
                            //(userTokens.ChangePasswordToken, userId, currentRvn, password) |> users.HandleChangePasswordCmdAsync
                            // ...NMB-TEMP
                        | Error error -> error |> Error |> thingAsync
                    let! connections = connections |> sendMsg (result |> ChangePasswordCmdResult |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                    result |> logResult "ChangePasswordCmd" (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                    return! managingConnections connections
                | UiAuthMsg (jwt, UiAuthAppMsg SignOutCmd) ->     
                    // SNH-NMB: What if connectionId not in connections? What if no [or mismatched?] AuthUserSession for connectionId? (&c.)...
                    sprintf "SignOutCmd for %A when managingConnections (%i connection/s)" jwt connections.Length |> Verbose |> log
                    let result =
                        if debugFakeError () then sprintf "Fake SignOutCmd error -> %A" jwt |> OtherError |> OtherCmdError |> Error
                        else
                            match jwt |> fromJwt with
                            | Ok (sessionId, userId, userName, permissions) -> (sessionId, userId, userName, permissions) |> Ok
                            | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> CmdJwtError |> Error
                    // TODO-NMB-HIGH: If Ok, verify userId | permissions | (&c.) against cache - and retrieve UserTokens...
                    // Note: Okay to sign out connection-for-connectionId before sending SignOutResultMsgOLD (since will always match OnlyConnectedId, even if signed-out).
                    let connections =
                        match result with
                        | Ok (sessionId, userId, _, _) -> connections |> signOut (connectionId, userId, sessionId)
                        | Error _ -> connections
                    let! connections = connections |> sendMsg (result |> discardOk |> SignOutCmdResult |> ServerAppMsg) (connectionId |> OnlyConnectionId)
                    let! connections =
                        match result with
                        | Ok (sessionId, userId, _, _) ->
                            connections |> sendMsg (None |> AutoSignOutMsg |> ServerAppMsg) ((userId, sessionId, connectionId) |> SameUserSessionExceptConnectionId)
                        | Error _ -> connections |> thingAsync
                    // Note: Auto-sign out connections-for-same-user-session-except-ConnectionId after sending AutoSignOutMsgOLD (else would not match SameUserSessionExceptConnectionId once signed-out).
                    let connections =
                        match result with
                        | Ok (sessionId, userId, _, _) -> connections |> autoSignOut (userId, sessionId)
                        | Error _ -> connections
                    let! connections =
                        match result with
                        | Ok (_, userId, userName, _) ->
                            if connections |> countForUserId userId = 0 then
                                let (UserName userName) = userName
                                userId |> UserSignedOut |> broadcaster.Broadcast
                                // TODO-NMB-HIGH: Remove use of OtherUserSignedOutMsgOLD once handled by Chat projection...
                                connections |> sendMsg (userName |> OtherUserSignedOutMsgOLD |> ServerAppMsg) (userId |> AllAuthExceptUserId)
                            else thingAsync connections
                        | Error _ -> thingAsync connections
                    result |> logResult "SignOutCmd" (fun (_, userId, userName, _) -> sprintf "%A %A" userName userId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                    return! managingConnections connections
                | UiAuthMsg (jwt, UiAuthChatMsg (SendChatMessageCmd chatMessage)) ->
                    // SNH-NMB: What if connectionId not in connections? What if no AuthUserSession for connectionId? (&c.)...
                    sprintf "SendChatMessageCmd for %A when managingConnections (%i connection/s) -> %A" jwt connections.Length chatMessage |> Verbose |> log
                    let result =
                        if debugFakeError () then sprintf "Fake SendChatMessageCmd error -> %A -> %A" jwt chatMessage |> OtherError |> OtherCmdError |> Error
                        else
                            match jwt |> fromJwt with
                            | Ok (sessionId, userId, userName, permissions) -> (sessionId, userId, userName, permissions) |> Ok
                            | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> CmdJwtError |> Error
                    let result = match result with | Ok ok -> ok |> Ok | Error error -> (chatMessage.ChatMessageId, sprintf "%A" error) |> Error
                    // TODO-NMB-HIGH: If Ok, verify userId | permissions | (&c.) against cache - and retrieve UserTokens...
                    // TODO-NMB-HIGH: Handle via Chat [projection] agent?...
                    let connections =
                        match result with
                        | Ok (sessionId, userId, _, _) ->
                            userId |> UserApi |> broadcaster.Broadcast
                            connections |> updateLastApi (connectionId, userId, sessionId)
                        | Error _ -> connections
                    let result = match result with | Ok _ -> chatMessage |> Ok | Error error -> error |> Error
                    let! connections = connections |> sendMsg (result |> SendChatMessageResultMsgOLD |> ServerChatMsg) (connectionId |> OnlyConnectionId)
                    let! connections =
                        match result with
                        | Ok chatMessage ->
                            connections |> sendMsg (chatMessage |> OtherUserChatMessageMsgOLD |> ServerChatMsg) (connectionId |> AllAuthExceptConnectionId)
                        | Error _ -> connections |> thingAsync
                    result |> logResult "SendChatMessageCmd" (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                    return! managingConnections connections }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Connections |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member _self.Start () =
        // TODO-NMB-LOW: Subscribe to Tick (e.g. to auto-sign out "expired" sessions)?...
        let onEvent = (fun event ->
            match event with
            | SendMsg (_serverMsg, _recipients) -> () // TODO-NMB-HIGH... (serverMsg, recipients) |> self.SendMsg
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to SendMsg broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AddConnection (connectionId, ws) = (connectionId, ws) |> AddConnection |> agent.Post
    member __.RemoveConnection connectionId = connectionId |> RemoveConnection |> agent.Post
    member __.OnReceiveUiMsgError (connectionId, exn) = (connectionId, exn) |> OnReceiveUiMsgError |> agent.Post
    member __.OnDeserializeUiMsgError (connectionId, exn) = (connectionId, exn) |> OnDeserializeUiMsgError |> agent.Post
    member __.HandleUiMsg (connectionId, uiMsg) = (connectionId, uiMsg) |> HandleUiMsg |> agent.Post

let connections = Connections ()
