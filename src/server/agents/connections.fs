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

type private AuthenticatedUserSession = {
    UserId : UserId
    SessionId : SessionId
    LastApi : DateTime }

type private Connection = {
    ConnectionId : ConnectionId
    Ws : WebSocket
    AuthenticatedUserSession : AuthenticatedUserSession option }

type private SendFilter =
    | OnlyConnectionId of connectionId : ConnectionId
    | AllAuthenticatedExceptConnectionId of connectionId : ConnectionId
    | AllAuthenticatedExceptUserId of userId : UserId
    | SameUserSessionExceptConnectionId of userId : UserId * sessionId : SessionId * connectionId : ConnectionId

#if DEBUG
let private random = Random ()
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
            | AllAuthenticatedExceptConnectionId connectionId ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthenticatedUserSession with
                    | Some _ -> connection.ConnectionId <> connectionId
                    | None -> false)
            | AllAuthenticatedExceptUserId userId ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthenticatedUserSession with
                    | Some authenticatedUserSession -> authenticatedUserSession.UserId <> userId
                    | None -> false)
            | SameUserSessionExceptConnectionId (userId, sessionId, connectionId) ->
                connections
                |> List.filter (fun connection ->
                    match connection.AuthenticatedUserSession with
                    | Some authenticatedUserSession -> connection.ConnectionId <> connectionId && authenticatedUserSession.UserId = userId && authenticatedUserSession.SessionId = sessionId
                    | None -> false)
        let recipients = recipients |> List.map (fun connection -> connection.ConnectionId, connection.Ws)
        let! failures = send recipients []
        return connections |> remove failures }
    let signIn (connectionId, userId, sessionId) connections =
        let authenticatedUserSession = { UserId = userId ; SessionId = sessionId ; LastApi = DateTime.Now }
        connections
        |> List.map (fun connection -> if connection.ConnectionId = connectionId then { connection with AuthenticatedUserSession = Some authenticatedUserSession } else connection)
    let signOut (userId, sessionId) connections =
        connections
        |> List.map (fun connection -> 
            match connection.AuthenticatedUserSession with
            | Some authenticatedUserSession when authenticatedUserSession.UserId = userId && authenticatedUserSession.SessionId = sessionId ->
                { connection with AuthenticatedUserSession = None }
            | Some _ | None -> connection)
    let updateLastApi(connectionId, userId, sessionId) connections =
        connections
        |> List.map (fun connection ->
            if connection.ConnectionId = connectionId then
                match connection.AuthenticatedUserSession with
                | Some authenticatedUserSession when authenticatedUserSession.UserId = userId && authenticatedUserSession.SessionId = sessionId ->
                    let authenticatedUserSession = { authenticatedUserSession with LastApi = DateTime.Now }
                    { connection with AuthenticatedUserSession = Some authenticatedUserSession }
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
                // SNH-NMB: What if connectionId already in connections?...
                let otherConnections = connections |> List.length
                let signedIn =
                    connections
                    |> List.choose (fun connection -> connection.AuthenticatedUserSession |> Option.map (fun authenticatedUserSession -> authenticatedUserSession.UserId))
                    |> List.distinct
                    |> List.length
                let connections = { ConnectionId = connectionId ; Ws = ws ; AuthenticatedUserSession = None } :: connections              
                let! connections = sendWs (ConnectedWs (otherConnections, signedIn) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                return! managing connections
            | RemoveConnection connectionId ->
                // SNH-NMB: What if connectionId not in connections?...
                // TODO-NMB: What if last connection for [userId], e.g. send (OtherUserSignedOut [userName] |> ServerAppWs) (AllAuthenticatedExceptUserId [userId]) connections?...
                let connections = connections |> remove [ connectionId ]
                return! managing connections
            | OnReceiveError (connectionId, exn) ->
                let! connections = sendWs (ServerWsErrorWs(ReceiveError exn.Message) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                return! managing connections
            | OnDeserializeUiWsApiError (connectionId, exn) ->                
                let! connections = sendWs (ServerWsErrorWs(DeserializeUiWsApiError exn.Message) |> ServerAppWsApi) (OnlyConnectionId connectionId) connections  
                return! managing connections
            | HandleUiWsApi (connectionId, uiWsApi) ->
                match uiWsApi with
                | UiUnauthenticatedWsApi (SignInWs (sessionId, userName, _password)) ->
                    // SNH-NMB: What if connectionId not in connections [SNH]? What if already have AuthenticatedUserSession for sessionId?...
                    // TODO-NMB: Check userName and _password...
                    // TODO-NMB: If userName already in connections, re-use userId?...
                    let authenticatedUser = { UserId = UserId.Create () ; SessionId = sessionId ; UserName = userName }
                    let result =
#if DEBUG
                        if random.NextDouble () < 0.02 then (Error (sprintf "Fake SignInWs error -> %A" authenticatedUser)) else (Ok authenticatedUser)
#else
                        (Ok authenticatedUser)
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signIn (connectionId, authenticatedUser.UserId, authenticatedUser.SessionId) else connections
                    let! connections = sendWs (SignInResultWs result |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk then sendWs (OtherUserSignedIn userName |> ServerAppWsApi) (AllAuthenticatedExceptUserId authenticatedUser.UserId) connections
                        else idAsync connections
                    return! managing connections
                | UiAuthenticatedWsApi (Jwt jwt, SignOutWs) ->       
                    // SNH-NMB: What if connectionId not in connections? What if no AuthenticatedUserSession for userId+sessionId?...
                    let result =
#if DEBUG
                        if random.NextDouble () < 0.02 then (Error (sprintf "Fake SignOutWs error -> %A" jwt)) else (Ok jwt.SessionId)
#else
                        (Ok authenticatedUser)
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> signOut (jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendWs (SignOutResultWs result |> ServerAppWsApi) (OnlyConnectionId connectionId) connections
                    let! connections = 
                        if isOk then sendWs (AutoSignOutWs jwt.SessionId |> ServerAppWsApi) (SameUserSessionExceptConnectionId (jwt.UserId, jwt.SessionId, connectionId)) connections
                        else idAsync connections
                    let! connections =
                        if isOk then sendWs (OtherUserSignedOut jwt.UserName |> ServerAppWsApi) (AllAuthenticatedExceptUserId jwt.UserId) connections
                        else idAsync connections
                    return! managing connections
                | UiAuthenticatedWsApi (Jwt jwt, (SendChatMessageWs chatMessage)) ->
                    // SNH-NMB: What if connectionId not in connections? What if no AuthenticatedUserSession for connectionId?...
                    let result =
#if DEBUG
                        if random.NextDouble () < 0.02 then (Error (chatMessage.ChatMessageId, (sprintf "Fake SendChatMessageWs error -> %A -> %A" jwt chatMessage))) else (Ok chatMessage)
#else
                        (Ok authenticatedUser)
#endif
                    let isOk = match result with | Ok _ -> true | Error _ -> false
                    let connections = if isOk then connections |> updateLastApi (connectionId, jwt.UserId, jwt.SessionId) else connections
                    let! connections = sendWs (SendChatMessageResultWs result |> ServerChatWsApi) (OnlyConnectionId connectionId) connections
                    let! connections =
                        if isOk then sendWs (OtherUserChatMessageWs chatMessage |> ServerChatWsApi) (AllAuthenticatedExceptConnectionId connectionId) connections
                        else idAsync connections
                    return! managing connections }
        managing [])
    member __.AddConnection (connectionId, ws) = agent.Post (AddConnection (connectionId, ws))
    member __.RemoveConnection connectionId = agent.Post (RemoveConnection connectionId)
    member __.OnReceiveError (connectionId, exn) = agent.Post (OnReceiveError (connectionId, exn))
    member __.OnDeserializeUiWsApiError (connectionId, exn) = agent.Post (OnDeserializeUiWsApiError (connectionId, exn))
    member __.HandleUiWsApi (connectionId, uiWs) = agent.Post (HandleUiWsApi (connectionId, uiWs))

let connectionsAgent = ConnectionsAgent ()
