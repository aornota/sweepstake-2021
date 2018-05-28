module Aornota.Sweepstake2018.Server.Agents.Connections

// Note: Connections agent broadcasts UserSignedIn | UserActivity | UserSignedOut | ConnectionsSignedOut | Disconnected - and subscribes to SendMsg.

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers
open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.Server.Agents
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Projections.Chat
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Jwt
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text
open System.Threading

open FSharp.Control.Tasks.ContextInsensitive

type private ConnectionsInput =
    | Start of serverStarted : DateTimeOffset * reply : AsyncReplyChannel<unit>
    | OnSendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list
    | AddConnection of connectionId : ConnectionId * ws : WebSocket
    | RemoveConnection of connectionId : ConnectionId
    | OnReceiveUiMsgError of connectionId : ConnectionId * exn : exn
    | OnDeserializeUiMsgError of connectionId : ConnectionId * exn : exn
    | HandleUiMsg of connectionId : ConnectionId * uiMsg : UiMsg

type private SignedInUser = { UserName : UserName ; Permissions : Permissions ; UserTokens : UserTokens ; LastApi : DateTimeOffset }

type private SignedInUserDic = Dictionary<UserId, SignedInUser>

type private SignedInSession = UserId * SessionId

type private Connection = WebSocket * SignedInSession option

type private ConnectionDic = Dictionary<ConnectionId, Connection>

let private log category = (Connections, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private encoding = Encoding.UTF8

let private hasConnections userId (connectionDic:ConnectionDic) =
    let forUserId =
        connectionDic
        |> List.ofSeq
        |> List.filter (fun (KeyValue (_, (_, signedInSession))) -> match signedInSession with | Some (otherUserId, _) when otherUserId = userId -> true | Some _ | None -> false)
    forUserId.Length > 0

let private addSignedInUser (authUser:AuthUser) (signedInUserDic:SignedInUserDic) = async {
    if authUser.UserId |> signedInUserDic.ContainsKey |> not then // note: silently ignore if already in signedInUsers
        let signedInUser = { UserName = authUser.UserName ; Permissions = authUser.Permissions ; UserTokens = authUser.Permissions |> UserTokens ; LastApi = DateTimeOffset.UtcNow }
        (authUser.UserId, signedInUser) |> signedInUserDic.Add
        authUser.UserId |> UserSignedIn |> broadcaster.Broadcast }

let private removeSignedInUser userId (signedInUserDic:SignedInUserDic) =
    if userId |> signedInUserDic.Remove then userId |> UserSignedOut |> broadcaster.Broadcast
    else // note: should never happen
        sprintf "removeSignedInUser -> %A not found in signedInUsers" userId |> Danger |> log

let private removeConnection connectionId (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) =
    match connectionId |> connectionDic.TryGetValue with
    | true, (_, signedInSession) ->
        connectionId |> connectionDic.Remove |> ignore
        connectionId |> Disconnected |> broadcaster.Broadcast
        match signedInSession with
        | Some (userId, _) -> if connectionDic |> hasConnections userId |> not then signedInUserDic |> removeSignedInUser userId
        | None -> ()
    | false, _ -> // note: should never happen
        sprintf "removeConnection -> %A not found in connections" connectionId |> Danger |> log

let private sendMsg (serverMsg:ServerMsg) connectionIds (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    let (Json json) = serverMsg |> toJson
    let buffer = encoding.GetBytes json
    let segment = ArraySegment<byte> (buffer)
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
        connectionIds |> List.choose (fun connectionId -> match connectionId |> connectionDic.TryGetValue with | true, (ws, _) -> (connectionId, ws) |> Some | false, _ -> None)
    let! failedConnectionIds = [] |> send recipients
    failedConnectionIds |> List.iter (fun connectionId -> (connectionDic, signedInUserDic) |> removeConnection connectionId)
    return () }

let private autoSignOut autoSignOutReason userId onlySessionId exceptConnectionId (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    let otherConnections =
        connectionDic |> List.ofSeq |> List.choose (fun (KeyValue (connectionId, (ws, (signedInSession)))) ->
            let signOut =
                match signedInSession with
                | Some (otherUserId, sessionId) ->
                    if otherUserId = userId then
                        match onlySessionId with
                        | Some onlySessionId when onlySessionId = sessionId -> true
                        | Some _ -> false
                        | None -> true
                    else false
                | None -> false
            let signOut =
                if signOut then
                    match exceptConnectionId with
                    | Some exceptConnectionId when exceptConnectionId <> connectionId -> true // note: this check should be superfluous (as calling code will have ensured that exceptConnectionId no longer has a SignedInSession)
                    | Some _ -> false
                    | None -> true
                else signOut
            if signOut then (ws, connectionId) |> Some
            else None)
    match otherConnections with
    | _ :: _ ->
        otherConnections |> List.iter (fun (ws, otherConnectionId) -> connectionDic.[otherConnectionId] <- (ws, None))
        let otherConnectionIds = otherConnections |> List.map snd
        otherConnectionIds |> ConnectionsSignedOut |> broadcaster.Broadcast
        do! (connectionDic, signedInUserDic) |> sendMsg (autoSignOutReason |> AutoSignOutMsg |> ServerAppMsg) otherConnectionIds
    | [] -> ()
    // Note: No need to broadcast UserSignedOut (if appropriate) since handled by removeSignedInUser.
    if connectionDic |> hasConnections userId |> not then signedInUserDic |> removeSignedInUser userId
    return () }

let private ifConnection source connectionId fWithWs (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, _) ->
        do! ws |> fWithWs
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private ifNoSignedInSession source connectionId fWithWs (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some _ -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A already has a signed-in session" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
        | None -> do! ws |> fWithWs
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private ifSignedInSession source connectionId fWithConnection (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some signedInSession -> do! (ws, signedInSession) |> fWithConnection
        | None -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A does not have a signed-in session" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private tokensForAuthApi source (otherError, jwtError) updateLastApi userId jwt (signedInUserDic:SignedInUserDic) =
    // Note: If successful [and if requested], updates SignedInUser.LastApi and broadcasts UserActivity.
    match jwt |> fromJwt with
    | Ok (userIdFromJwt, permissionsFromJwt) ->
        if userIdFromJwt <> userId then // note: should never happen
            let errorText = sprintf "%s -> UserId mismatch -> SignedInSession %A vs. userIdFromJwt %A" source userId userIdFromJwt
            ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
        else                                            
            match userIdFromJwt |> signedInUserDic.TryGetValue with
            | true, signedInUser ->
                if signedInUser.Permissions <> permissionsFromJwt then // note: should never happen
                    let errorText = sprintf "%s -> Permissions mismatch -> SignedInUser %A vs. permissionsFromJwt %A" source signedInUser.Permissions permissionsFromJwt
                    ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
                else
                    if updateLastApi then
                        let signedInUser = { signedInUser with LastApi = DateTimeOffset.UtcNow }
                        signedInUserDic.[userIdFromJwt] <- signedInUser
                        userIdFromJwt |> UserActivity |> broadcaster.Broadcast
                    signedInUser.UserTokens |> Ok
            | false, _ -> // note: should never happen
                let errorText = sprintf "%s -> No SignedInUser for %A" source userIdFromJwt
                ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
    | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> jwtError |> Error

let private tokensForAuthCmdApi source updateLastApi userId jwt signedInUsers = tokensForAuthApi source (OtherAuthCmdError, AuthCmdJwtError) updateLastApi userId jwt signedInUsers
let private tokensForAuthQryApi source userId jwt signedInUsers = tokensForAuthApi source (OtherAuthQryError, AuthQryJwtError) true userId jwt signedInUsers

type Connections () =
    let agent = MailboxProcessor.Start (fun inbox ->
        // #region: awaitingStart
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start (serverStarted, reply) ->
                "Start when awaitingStart -> managingConnections (0 connections)" |> Info |> log
                () |> reply.Reply
                return! managingConnections (serverStarted, ConnectionDic (), SignedInUserDic ())
            | AddConnection _ -> "AddConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnReceiveUiMsgError _ -> "OnReceiveUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDeserializeUiMsgError _ -> "OnDeserializeUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleUiMsg _ -> "HandleUiMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSendMsg _ -> "SendMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        // #endregion
        // #region: managingConnections
        and managingConnections (serverStarted, connectionDic, signedInUserDic) = async {
            let! input = inbox.Receive ()
            do! ifDebugSleepAsync 100 500
            match input with
            | Start _ -> "Start when managingConnections" |> IgnoredInput |> Agent |> log ; return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | OnSendMsg (serverMsg, connectionIds) ->
                sprintf "SendMsg %A (%i ConnectionId/s) when managingConnections (%i connection/s) (%i signed-in user/s)" serverMsg connectionIds.Length connectionDic.Count signedInUserDic.Count |> Verbose |> log
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg connectionIds
                return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | AddConnection (connectionId, ws) ->
                sprintf "AddConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                let otherConnectionCount, signedInUserCount = connectionDic.Count, signedInUserDic.Count
                if connectionId |> connectionDic.ContainsKey then // note: should never happen
                    sprintf "AddConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (already in use)" connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
                else
                    (connectionId, (ws, None)) |> connectionDic.Add
                    let serverMsg = (serverStarted, otherConnectionCount, signedInUserCount) |> ConnectedMsg |> ServerAppMsg
                    do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | RemoveConnection connectionId ->
                sprintf "RemoveConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                match connectionId |> connectionDic.TryGetValue with
                | true, _ ->
                    // Note: No need to broadcast Disconnected - and UserSignedOut (if appropriate) - since handled by removeConnection.
                    (connectionDic, signedInUserDic) |> removeConnection connectionId
                | false, _ -> // note: should never happen
                    sprintf "RemoveConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
                return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | OnReceiveUiMsgError (connectionId, exn) ->
                sprintf "OnReceiveUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Danger |> log
                let serverMsg = exn.Message |> ReceiveUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | OnDeserializeUiMsgError (connectionId, exn) ->                
                sprintf "OnDeserializeUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Danger |> log
                let serverMsg = exn.Message |> DeserializeUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections (serverStarted, connectionDic, signedInUserDic)
            | HandleUiMsg (connectionId, uiMsg) ->
                match uiMsg with
                | Wiff -> // note: logged - but otherwise ignored
                    sprintf "Wiff for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #region: UiUnauthMsg UiUnauthAppMsg SignInCmd
                | UiUnauthMsg (UiUnauthAppMsg (SignInCmd (sessionId, userName, password))) ->
                    let source = "SignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userName connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let! result =
                            if debugFakeError () then sprintf "Fake %s error -> %A (%A)" source userName sessionId |> OtherError |> OtherSignInCmdError |> Error |> thingAsync
                            else (userName, password) |> users.HandleSignInCmdAsync
                        let result =
                            result
                            |> Result.bind (fun authUser ->
                                // Note: No need to broadcast UserSignedIn (if appropriate) since handled by addSignedInUser.
                                signedInUserDic |> addSignedInUser authUser |> Async.RunSynchronously
                                connectionDic.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> SignInCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiUnauthMsg UiUnauthAppMsg AutoSignInCmd
                | UiUnauthMsg (UiUnauthAppMsg (AutoSignInCmd (sessionId, jwt))) ->
                    let source = "AutoSignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source sessionId jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAutoSignInCmdError |> Error
                            else
                                // Note: Similar logic to tokensForAuthApi.
                                match jwt |> fromJwt with
                                | Ok (userIdFromJwt, permissionsFromJwt) ->
                                    match userIdFromJwt |> signedInUserDic.TryGetValue with
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
                                signedInUserDic |> addSignedInUser authUser |> Async.RunSynchronously
                                connectionDic.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> AutoSignInCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiUnauthMsg UiUnauthSquadsMsg InitializeSquadsProjectionQry
                | UiUnauthMsg (UiUnauthSquadsMsg InitializeSquadsProjectionQry) ->
                    let source = "InitializeSquadsProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log               
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ ->

                                // TODO-NEXT... connectionId |> Projections.Squads.squads.HandleInitializeSquadsProjectionQry

                                () |> Ok |> thingAsync // TEMP-NMB...
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeSquadsProjectionQryResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                    })
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UserNonApiActivity
                | UiAuthMsg (jwt, UserNonApiActivity) ->
                    let source = "UserNonApiActivity"
                    sprintf "%s for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async { userId |> UserActivity |> broadcaster.Broadcast })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UiAuthAppMsg SignOutCmd
                | UiAuthMsg (jwt, UiAuthAppMsg SignOutCmd) ->     
                    let source = "SignOutCmd"
                    sprintf "%s for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (ws, (userId, sessionId)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source false userId jwt // note: if successful, does *not* update SignedInUser.LastApi (nor broadcast UserActivity)
                            |> Result.bind (fun _ ->
                                connectionDic.[connectionId] <- (ws, None) // note: connectionId will be in connections (otherwise ifSignedInSession would bypass fWithConnection)
                                [ connectionId ] |> ConnectionsSignedOut |> broadcaster.Broadcast
                                () |> Ok)
                        let serverMsg = result |> SignOutCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun _ -> sprintf "%A" userId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                        match result with
                        | Ok _ -> do! (connectionDic, signedInUserDic) |> autoSignOut None userId (sessionId |> Some) (connectionId |> Some)
                        | Error _ -> () })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UiAuthAppMsg ChangePasswordCmd
                | UiAuthMsg (jwt, UiAuthAppMsg (ChangePasswordCmd (currentRvn, password))) ->
                    let source = "ChangePasswordCmd"
                    sprintf "%s (%A) for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source currentRvn jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChangePasswordToken with
                                | Some changePasswordToken -> (changePasswordToken, userId, currentRvn, password) |> users.HandleChangePasswordCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePasswordCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UiAuthSquadsMsg AddPlayerCmd
                | UiAuthMsg (jwt, UiAuthSquadsMsg (AddPlayerCmd (squadId, currentRvn, playerId, playerName, playerType))) ->
                    let source = "AddPlayerCmd"
                    sprintf "%s (%A %A %A) for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source playerId playerName playerType squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.AddOrEditPlayerToken with
                                | Some addOrEditPlayerToken ->
                                    (addOrEditPlayerToken, userId, squadId, currentRvn, playerId, playerName, playerType) |> Entities.Squads.squads.HandleAddPlayerCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> AddPlayerCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion

                (* TODO-NMB-HIGH:
                    - UiAuthUserAdminMsg InitializeUserAdminProjectionQry...
                    - UiAuthUserAdminMsg CreateUserCmd...
                    - UiAuthUserAdminMsg ResetPasswordCmd [remember to autoSignOut PasswordReset [any SessionId / any ConnectionId], cf. SignOutCmd]...
                    - UiAuthUserAdminMsg ChangeUserTypeCmd [remember to autoSignOut PermissionsChanged [any SessionId / any ConnectionId], cf. SignOutCmd]... *)

                (* TODO-NMB-HIGH:
                    - UiAuthSquadsMsg ChangePlayerNameCmd... 
                    - UiAuthSquadsMsg ChangePlayerTypeCmd... 
                    - UiAuthSquadsMsg WithdrawPlayerCmd... 
                    - UiAuthSquadsMsg EliminateSquadCmd... *)

                // #region: UiAuthMsg UiAuthChatMsg InitializeChatProjectionQry
                | UiAuthMsg (jwt, UiAuthChatMsg InitializeChatProjectionQry) ->
                    let source = "InitializeChatProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChatProjectionQryToken with
                                | Some chatProjectionQryToken -> (chatProjectionQryToken, connectionId) |> chat.HandleInitializeChatProjectionQry
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeChatProjectionQryResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UiAuthChatMsg MoreChatMessagesQry
                | UiAuthMsg (jwt, UiAuthChatMsg MoreChatMessagesQry) ->
                    let source = "MoreChatMessagesQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChatProjectionQryToken with
                                | Some chatProjectionQryToken -> (chatProjectionQryToken, connectionId) |> chat.HandleMoreChatMessagesQry
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> MoreChatMessagesQryResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic)
                // #endregion
                // #region: UiAuthMsg UiAuthChatMsg SendChatMessageCmd
                | UiAuthMsg (jwt, UiAuthChatMsg (SendChatMessageCmd (chatMessageId, messageText))) ->
                    let source = "SendChatMessageCmd"
                    sprintf "%s %A for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source chatMessageId jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.SendChatMessageToken with
                                | Some sendChatMessageToken -> (sendChatMessageToken, userId, chatMessageId, messageText) |> chat.HandleSendChatMessageCmd
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> tupleError chatMessageId |> SendChatMessageCmdResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)                   
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections (serverStarted, connectionDic, signedInUserDic) }
                // #endregion
        // #endregion
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Connections |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start (serverStarted) =
        // TODO-NMB-LOW: Subscribe to Tick, e.g. to auto-sign out "expired" sessions? and/or to "purge" connections (i.e. via sending Waff to all connections)?...
        let onEvent = (fun event ->
            match event with
            | Signal.SendMsg (serverMsg, connectionIds) -> (serverMsg, connectionIds) |> OnSendMsg |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to SendMsg broadcasts -> %A" subscriptionId |> Info |> log
        (fun reply -> (serverStarted, reply) |>  Start) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AddConnection (connectionId, ws) = (connectionId, ws) |> AddConnection |> agent.Post
    member __.RemoveConnection connectionId = connectionId |> RemoveConnection |> agent.Post
    member __.OnReceiveUiMsgError (connectionId, exn) = (connectionId, exn) |> OnReceiveUiMsgError |> agent.Post
    member __.OnDeserializeUiMsgError (connectionId, exn) = (connectionId, exn) |> OnDeserializeUiMsgError |> agent.Post
    member __.HandleUiMsg (connectionId, uiMsg) = (connectionId, uiMsg) |> HandleUiMsg |> agent.Post

let connections = Connections ()
