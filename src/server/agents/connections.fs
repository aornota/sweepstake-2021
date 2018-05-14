module Aornota.Sweepstake2018.Server.Agents.Connections

// Note: Connections agent broadcasts UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnect - and subscribes to SendMsg.

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
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Jwt

open System
open System.Collections.Generic
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
    | SendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list

type private SignedInUser = { UserName : UserName ; Permissions : Permissions ; UserTokens : UserTokens ; LastApi : DateTime }

type private SignedInSession = UserId * SessionId

type private Connection = WebSocket * SignedInSession option

type private Recipients =
    | ConnectionIds of connectionIds : ConnectionId list
    | AllSignedInExceptConnectionId of connectionId : ConnectionId // TODO-REMOVE: Once no longer using OtherUserChatMessageMsgOLD...
    | AllSignedInExceptUserId of userId : UserId // TODO-REMOVE: Once no longer using OtherUserSignedInMsgOLD | OtherUserSignedOutMsgOLD...

let private log category = (Connections, category) |> consoleLogger.Log

let private logResult resultSource successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" resultSource successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" resultSource error |> Danger |> log

let private encoding = Encoding.UTF8

let private hasConnections userId (connections:Dictionary<ConnectionId, Connection>) =
    let forUserId =
        connections
        |> List.ofSeq
        |> List.filter (fun (KeyValue (_, (_, signedInSession))) -> match signedInSession with | Some (otherUserId, _) when otherUserId = userId -> true | Some _ | None -> false)
    forUserId.Length > 0

let private removeSignedInUser userId (signedInUsers:Dictionary<UserId, SignedInUser>) =
    if userId |> signedInUsers.Remove then userId |> UserSignedOut |> broadcaster.Broadcast
    else // note: should never happen
        sprintf "removeSignedInUser -> %A not found in signedInUsers" userId |> Danger |> log

let private removeConnection connectionId (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) =
    match connectionId |> connections.TryGetValue with
    | true, (_, signedInSession) ->
        connectionId |> connections.Remove |> ignore
        connectionId |> Disconnected |> broadcaster.Broadcast
        match signedInSession with
        | Some (userId, _) -> if connections |> hasConnections userId |> not then signedInUsers |> removeSignedInUser userId
        | None -> ()
    | false, _ -> // note: should never happen
        sprintf "removeConnection -> %A not found in connections" connectionId |> Danger |> log

let private sendMsg (serverMsg:ServerMsg) recipients (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) = async {
    let (Json json) = serverMsg |> toJson
    let buffer = encoding.GetBytes json
    let segment = new ArraySegment<byte> (buffer)
    let rec send (recipients:(ConnectionId * WebSocket) list) failedConnectionIds = async {
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
            return! send t (if success then failedConnectionIds else connectionId :: failedConnectionIds)
        | [] -> return failedConnectionIds }
    let recipients =
        match recipients with
        | ConnectionIds connectionIds -> // note: silently ignore unknown connectionIds
            connectionIds |> List.choose (fun connectionId -> match connectionId |> connections.TryGetValue with | true, (ws, _) -> (connectionId, ws) |> Some | false, _ -> None)
        | AllSignedInExceptConnectionId connectionId -> // note: silently ignore unknown connectionId
            connections
            |> List.ofSeq
            |> List.choose (fun (KeyValue (otherConnectionId, (ws, signedInSession))) ->
                if otherConnectionId <> connectionId then match signedInSession with | Some _ -> (otherConnectionId, ws) |> Some | None -> None else None)
        | AllSignedInExceptUserId userId -> // note: silently ignore unknown userId
            connections
            |> List.ofSeq
            |> List.choose (fun (KeyValue (connectionId, (ws, signedInSession))) ->
                match signedInSession with
                | Some (userId', _) -> if userId' <> userId then (connectionId, ws) |> Some else None
                | None -> None)
    let! failedConnectionIds = [] |> send recipients
    failedConnectionIds |> List.iter (fun connectionId -> (connections, signedInUsers) |> removeConnection connectionId)
    return () }

let private addSignedInUser authUser (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) = async {
    if authUser.UserId |> signedInUsers.ContainsKey |> not then // note: silently ignore if already in signedInUsers
        let signedInUser = { UserName = authUser.UserName ; Permissions = authUser.Permissions ; UserTokens = authUser.Permissions |> UserTokens ; LastApi = DateTime.Now }
        (authUser.UserId, signedInUser) |> signedInUsers.Add
        authUser.UserId |> UserSignedIn |> broadcaster.Broadcast
        // TODO-NMB-HIGH: Remove use of OtherUserSignedInMsgOLD once replaced by Chat [projection] agent - and can then move function *above* sendMsg (e.g. above removeSignedInUser)...
        let (UserName userName) = authUser.UserName
        let serverMsg = userName |> OtherUserSignedInMsgOLD |> ServerAppMsg
        do! (connections, signedInUsers) |> sendMsg serverMsg (authUser.UserId |> AllSignedInExceptUserId) }

let private ifNoSignedInSession source connectionId fWithWs (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) = async {
    match connectionId |> connections.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some _ -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A already has a SignedInSession" source connections.Count signedInUsers.Count connectionId |> Danger |> log
        | None -> do! ws |> fWithWs
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connections.Count signedInUsers.Count connectionId |> Danger |> log }

let private ifSignedInSession source connectionId fWithConnection (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) = async {
    match connectionId |> connections.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some signedInSession -> do! (ws, signedInSession) |> fWithConnection
        | None -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A does not have a signed-in session" source connections.Count signedInUsers.Count connectionId |> Danger |> log
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connections.Count signedInUsers.Count connectionId |> Danger |> log }

let private tokensForAuthApi source (otherError, jwtError) userId jwt (signedInUsers:Dictionary<UserId, SignedInUser>) =
    // Note: If successful, updates SignedInUser.LastApi and broadcasts UserApi.
    match jwt |> fromJwt with
    | Ok (userIdFromJwt, permissionsFromJwt) ->
        if userIdFromJwt <> userId then // note: should never happen
            let errorText = sprintf "%s -> UserId mismatch -> SignedInSession %A vs. userIdFromJwt %A" source userId userIdFromJwt
            ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
        else                                            
            match userIdFromJwt |> signedInUsers.TryGetValue with
            | true, signedInUser ->
                if signedInUser.Permissions <> permissionsFromJwt then // note: should never happen
                    let errorText = sprintf "%s -> Permissions mismatch -> SignedInUser %A vs. permissionsFromJwt %A" source signedInUser.Permissions permissionsFromJwt
                    ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
                else
                    let signedInUser = { signedInUser with LastApi = DateTime.Now }
                    signedInUsers.[userIdFromJwt] <- signedInUser
                    userIdFromJwt |> UserApi |> broadcaster.Broadcast
                    signedInUser.UserTokens |> Ok
            | false, _ -> // note: should never happen
                let errorText = sprintf "%s -> No SignedInUser for %A" source userIdFromJwt
                ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
    | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> jwtError |> Error

let private tokensForAuthCmdApi source userId jwt signedInUsers = tokensForAuthApi source (OtherAuthCmdError, AuthCmdJwtError) userId jwt signedInUsers
let private tokensForAuthQryApi source userId jwt signedInUsers = tokensForAuthApi source (OtherAuthQryError, AuthQryJwtError) userId jwt signedInUsers

// TODO-REMOVE: Once OtherUserSignedOutMsgOLD replaced by Chat [projection] agent...
let private tempIfFullySignedOut (userId, UserName userName) (connections:Dictionary<ConnectionId, Connection>, signedInUsers:Dictionary<UserId, SignedInUser>) = async {
    if userId |> signedInUsers.ContainsKey |> not then
        let serverMsg = userName |> OtherUserSignedOutMsgOLD |> ServerAppMsg
        do! (connections, signedInUsers) |> sendMsg serverMsg (userId |> AllSignedInExceptUserId) }

type Connections () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> managingConnections (0 connections)" |> Info |> log
                () |> reply.Reply
                return! managingConnections (new Dictionary<ConnectionId, Connection> (), new Dictionary<UserId, SignedInUser> ())
            | AddConnection _ -> "AddConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnReceiveUiMsgError _ -> "OnReceiveUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDeserializeUiMsgError _ -> "OnDeserializeUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleUiMsg _ -> "HandleUiMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | SendMsg _ -> "SendMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and managingConnections (connections, signedInUsers) = async {
            let! input = inbox.Receive ()
            do! ifDebugSleepAsync 100 500
            match input with
            | Start _ -> "Start when managingConnections" |> IgnoredInput |> Agent |> log ; return! managingConnections (connections, signedInUsers)
            | AddConnection (connectionId, ws) ->
                sprintf "AddConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connections.Count signedInUsers.Count |> Verbose |> log
                let otherConnectionCount, signedInUserCount = connections.Count, signedInUsers.Count
                if connections.ContainsKey connectionId then // note: should never happen
                    sprintf "AddConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (already in use)" connections.Count signedInUsers.Count connectionId |> Danger |> log
                else
                    (connectionId, (ws, None)) |> connections.Add
                    let serverMsg = (otherConnectionCount, signedInUserCount) |> ConnectedMsg |> ServerAppMsg
                    do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                return! managingConnections (connections, signedInUsers)
            | RemoveConnection connectionId ->
                sprintf "RemoveConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connections.Count signedInUsers.Count |> Verbose |> log
                match connectionId |> connections.TryGetValue with
                | true, (_, signedInSession) ->
                    // Note: No need to broadcast Disconnected - and UserSignedOut (if appropriate) - since handled by removeConnection.
                    // TODO-REMOVE: Use of tempUserIdAndUserName | tempIfFullySignedOut once OtherUserSignedOutMsgOLD replaced by Chat [projection] agent...
                    let tempUserIdAndUserName =
                        match signedInSession with
                        | Some (userId, _) -> match userId |> signedInUsers.TryGetValue with | true, signedInUser -> (userId, signedInUser.UserName) |> Some | false, _ -> None
                        | None -> None
                    (connections, signedInUsers) |> removeConnection connectionId
                    match tempUserIdAndUserName with
                    | Some (userId, userName) -> do! (connections, signedInUsers) |> tempIfFullySignedOut (userId, userName)
                    | None -> ()
                | false, _ -> // note: should never happen
                    sprintf "RemoveConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" connections.Count signedInUsers.Count connectionId |> Danger |> log
                return! managingConnections (connections, signedInUsers)
            | OnReceiveUiMsgError (connectionId, exn) ->
                sprintf "OnReceiveUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connections.Count signedInUsers.Count |> Danger |> log
                let serverMsg = exn.Message |> ReceiveUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                return! managingConnections (connections, signedInUsers)
            | OnDeserializeUiMsgError (connectionId, exn) ->                
                sprintf "OnDeserializeUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connections.Count signedInUsers.Count |> Danger |> log
                let serverMsg = exn.Message |> DeserializeUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                return! managingConnections (connections, signedInUsers)
            | HandleUiMsg (connectionId, uiMsg) ->
                match uiMsg with
                | Ping -> // note: logged - but otherwise ignored
                    sprintf "Ping for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connections.Count signedInUsers.Count |> Verbose |> log
                    return! managingConnections (connections, signedInUsers)
                | UiUnauthMsg (UiUnauthAppMsg (SignInCmd (sessionId, userName, password))) ->
                    let source = "SignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userName connectionId connections.Count signedInUsers.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let! result =
                            if debugFakeError () then sprintf "Fake %s error -> %A (%A)" source userName sessionId |> OtherError |> OtherSignInCmdError |> Error |> thingAsync
                            else (userName, password) |> users.HandleSignInCmdAsync
                        let result =
                            result
                            |> Result.bind (fun authUser ->
                                // Note: No need to broadcast UserSignedIn (if appropriate) since handled by addSignedInUser.
                                (connections, signedInUsers) |> addSignedInUser authUser |> Async.RunSynchronously
                                connections.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> SignInCmdResult |> ServerAppMsg
                        do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connections, signedInUsers) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections (connections, signedInUsers)
                | UiUnauthMsg (UiUnauthAppMsg (AutoSignInCmd (sessionId, jwt))) ->
                    let source = "AutoSignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source sessionId jwt connections.Count signedInUsers.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAutoSignInCmdError |> Error
                            else
                                // Note: Similar logic to tokensForAuthApi.
                                match jwt |> fromJwt with
                                | Ok (userIdFromJwt, permissionsFromJwt) ->
                                    match userIdFromJwt |> signedInUsers.TryGetValue with
                                    | true, signedInUser ->
                                        if signedInUser.Permissions <> permissionsFromJwt then
                                            let errorText = sprintf "%s -> Permissions mismatch -> SignedInUser %A vs. permissionsFromJwt %A" source signedInUser.Permissions permissionsFromJwt
                                            ifDebug errorText UNEXPECTED_ERROR |> OtherError |> OtherAutoSignInCmdError |> Error
                                        else (userIdFromJwt, permissionsFromJwt) |> Ok
                                    | false, _ -> (userIdFromJwt, permissionsFromJwt) |> Ok
                                | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> AutoSignInCmdJwtError |> Error
                        let! result =
                            match result with
                            | Ok (userIdFromJwt, permissionsFromJwt) -> (userIdFromJwt, permissionsFromJwt) |> users.HandleAutoSignInCmdAsync
                            | Error error -> error |> Error |> thingAsync
                        let result =
                            result
                            |> Result.bind (fun authUser ->
                                // Note: No need to broadcast UserSignedIn (if appropriate) since handled by addSignedInUser.
                                (connections, signedInUsers) |> addSignedInUser authUser |> Async.RunSynchronously
                                connections.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> AutoSignInCmdResult |> ServerAppMsg
                        do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connections, signedInUsers) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections (connections, signedInUsers)
                | UiAuthMsg (jwt, UiAuthAppMsg (ChangePasswordCmd (currentRvn, password))) ->     
                    let source = "ChangePasswordCmd"
                    sprintf "%s (%A) for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source currentRvn jwt connections.Count signedInUsers.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUsers |> tokensForAuthCmdApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserApi)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChangePasswordToken with
                                | Some changePasswordToken -> (changePasswordToken, userId, currentRvn, password) |> users.HandleChangePasswordCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePasswordCmdResult |> ServerAppMsg
                        do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connections, signedInUsers) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (connections, signedInUsers)
                | UiAuthMsg (jwt, UiAuthAppMsg SignOutCmd) ->     
                    let source = "SignOutCmd"
                    sprintf "%s for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source jwt connections.Count signedInUsers.Count |> Verbose |> log
                    let fWithConnection = (fun (ws, (userId, sessionId)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUsers |> tokensForAuthCmdApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserApi)
                            |> Result.bind (fun _ ->
                                connections.[connectionId] <- (ws, None) // note: connectionId will be in connections (otherwise ifSignedInSession would bypass fWithConnection)
                                [ connectionId ] |> ConnectionsSignedOut |> broadcaster.Broadcast
                                () |> Ok)
                        let serverMsg = result |> SignOutCmdResult |> ServerAppMsg
                        do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                        result |> logResult source (fun _ -> sprintf "%A" userId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                        match result with
                        | Ok _ ->
                            // TODO-REMOVE: Use of tempUserName | tempIfFullySignedOut once OtherUserSignedOutMsgOLD replaced by Chat [projection] agent...
                            let tempUserName = match userId |> signedInUsers.TryGetValue with | true, signedInUser -> signedInUser.UserName |> Some | false, _ -> None
                            let otherConnectionsForSignedInSession =
                                connections
                                |> List.ofSeq
                                |> List.choose (fun (KeyValue (otherConnectionId, (ws, signedInSession))) ->
                                if otherConnectionId <> connectionId then // note: this check should be superfluous as connectionId no longer has a SignedInSession
                                    match signedInSession with | Some signedInSession when signedInSession = (userId, sessionId) -> (ws, otherConnectionId) |> Some | Some _ | None -> None
                                else None)
                            match otherConnectionsForSignedInSession with
                            | _ :: _ ->
                                otherConnectionsForSignedInSession |> List.iter (fun (ws, otherConnectionId) -> connections.[otherConnectionId] <- (ws, None))
                                let otherConnectionIds = otherConnectionsForSignedInSession |> List.map snd
                                otherConnectionIds |> ConnectionsSignedOut |> broadcaster.Broadcast
                                let serverMsg = None |> AutoSignOutMsg |> ServerAppMsg
                                do! (connections, signedInUsers) |> sendMsg serverMsg (otherConnectionIds |> ConnectionIds)
                            | [] -> ()
                            // Note: No need to broadcast UserSignedOut (if appropriate) since handled by removeSignedInUser.
                            if connections |> hasConnections userId |> not then signedInUsers |> removeSignedInUser userId
                            match tempUserName with
                            | Some userName -> do! (connections, signedInUsers) |> tempIfFullySignedOut (userId, userName)
                            | None -> () // note: should never happen
                        | Error _ -> () })
                    do! (connections, signedInUsers) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (connections, signedInUsers)
                | UiAuthMsg (jwt, UiAuthChatMsg (SendChatMessageCmd chatMessage)) ->
                    let source = "SendChatMessageCmd"
                    sprintf "%s %A for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source chatMessage jwt connections.Count signedInUsers.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUsers |> tokensForAuthCmdApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserApi)
                        // TODO-NMB-HIGH: Check has appropriate token for Chat [projection] agent...
                        // TODO-REMOVE: No need to hack Ok | Error once using SendChatMessageCmdResult (rather than SendChatMessageResultMsgOLD)...
                        let result = match result with | Ok _ -> chatMessage |> Ok | Error error -> (chatMessage.ChatMessageId, sprintf "%A" error) |> Error
                        // TODO-REMOVE: Use of OtherUserChatMessageMsgOLD once handled by Chat [projection] agent...
                        match result with
                        | Ok chatMessage ->
                            let serverMsg = chatMessage |> OtherUserChatMessageMsgOLD |> ServerChatMsg
                            do! (connections, signedInUsers) |> sendMsg serverMsg (connectionId |> AllSignedInExceptConnectionId)
                        | Error _ -> ()
                        let serverMsg = result |> SendChatMessageResultMsgOLD |> ServerChatMsg
                        do! (connections, signedInUsers) |> sendMsg serverMsg ([ connectionId ] |> ConnectionIds)
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connections, signedInUsers) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (connections, signedInUsers)
            | SendMsg (serverMsg, connectionIds) ->
                sprintf "SendMsg %A (%i ConnectionId/s) when managingConnections (%i connection/s) (%i signed-in user/s)" serverMsg connectionIds.Length connections.Count signedInUsers.Count |> Verbose |> log
                do! (connections, signedInUsers) |> sendMsg serverMsg (connectionIds |> ConnectionIds)
                return! managingConnections (connections, signedInUsers) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Connections |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        // TODO-NMB-LOW: Subscribe to Tick (e.g. to auto-sign out "expired" sessions)?...
        let onEvent = (fun event ->
            match event with
            | Event.SendMsg (serverMsg, connectionIds) -> (serverMsg, connectionIds) |> SendMsg |> agent.Post
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
