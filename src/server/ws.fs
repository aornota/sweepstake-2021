module Aornota.Sweepstake2018.Server.Ws

open Aornota.Sweepstake2018.Shared

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open FSharp.Control.Tasks.ContextInsensitive

open Microsoft.AspNetCore.Http
    
open Newtonsoft.Json

// TODO-NMB: Move Json stuff to a separate file?...

let private jsonConverter = Fable.JsonConverter () :> JsonConverter

let private toJson value = JsonConvert.SerializeObject (value, [|jsonConverter|])

let private ofJson<'a> (json:string) : 'a = JsonConvert.DeserializeObject<'a> (json, [|jsonConverter|])

#if DEBUG
let private random = Random ()
#endif

type private WsAgentMessage =
    | Add of ws : WebSocket
    | Remove of ws : WebSocket
    | UiWs of ws : WebSocket * uiWs : UiWs
    | OnReceiveError of ws : WebSocket * exn : exn

type private WsState = {
    Ws : WebSocket
    Connection : Connection option }

let private sendMessage (serverWs:ServerWs) (ws:WebSocket) = task {
    let json = toJson serverWs
    let buffer = Encoding.UTF8.GetBytes json
    let segment = new ArraySegment<byte> (buffer)
    if ws.State = WebSocketState.Open then
        do! ws.SendAsync (segment, WebSocketMessageType.Text, true, CancellationToken.None)
        return true
    else return false }
    
(*let sendMessageToSockets =
    fun message ->
        task {
            for socket in sockets do
                try
                    do! sendMessage socket message
                with
                    | _ -> sockets <- removeSocket sockets socket }*)
    
type private WsAgent () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let removeIf remove ws wsStates = if remove then wsStates |> List.filter (fun wsState -> wsState.Ws <> ws) else wsStates
        let rec running (wsStates:WsState list) = async {
            let! message = inbox.Receive ()
#if DEBUG
            do! Async.Sleep (random.Next (250, 1250))
#endif
            match message with
            | Add ws ->
                // TODO-NMB: What if already in wsStates?...
                let wsStates = { Ws = ws ; Connection = None } :: wsStates
                return! running wsStates
            | Remove ws ->
                // TODO-NMB: What if not in wsStates?...
                let wsStates = removeIf true ws wsStates
                return! running wsStates
            | UiWs (ws, uiWs) ->
                match uiWs with
                | ConnectWs connection ->
                    // TODO-NMB: Fake error [#if DEBUG], e.g. if connection.Nickname = "satan" then failwith "'satan' is a reserved nickname"?...
                    // TODO-NMB: What if not in wsStates? Or already connected? Or nickname mismatch?...
                    let wsStates = wsStates |> List.map (fun wsState -> if wsState.Ws = ws then { wsState with Connection = Some connection } else wsState)
                    // TODO-NMB: Send more message/s...
                    let! isOpen = sendMessage (ConnectResultWs (Ok connection)) ws |> Async.AwaitTask
                    let wsStates = removeIf (not isOpen) ws wsStates
                    return! running wsStates
                | SendMessageWs (connectionId, message) ->
                    // TODO-NMB: Fake error [#if DEBUG], e.g. //if (random.NextDouble () < 0.1) then failwith (sprintf "Message '%s' is inappropriate" message.Contents)
                    // TODO-NMB: Send more message/s...
                    let! isOpen = sendMessage (SendMessageResultWs (Ok message)) ws |> Async.AwaitTask
                    let wsStates = removeIf (not isOpen) ws wsStates
                    return! running wsStates
                | DisconnectWs connection ->                
                    // TODO-NMB: Fake error [#if DEBUG], e.g. //if connection.Nickname = "god" then failwith "'god' cannot disconnect"
                    // TODO-NMB: What if not in wsStates? Or not connected? Or nickname mismatch?...
                    let wsStates = wsStates |> List.map (fun wsState -> if wsState.Ws = ws then { wsState with Connection = None } else wsState) 
                    // TODO-NMB: Send more message/s...
                    let! isOpen = sendMessage (DisconnectResultWs (Ok connection)) ws |> Async.AwaitTask
                    let wsStates = removeIf (not isOpen) ws wsStates
                    return! running wsStates
            | OnReceiveError (ws, exn) ->
                // TODO-NMB: What if not in wsStates? Or not connected?...
                let! isOpen = sendMessage (OnReceiveErrorWs exn.Message) ws |> Async.AwaitTask
                let wsStates = removeIf (not isOpen) ws wsStates
                return! running wsStates }
        running [])
    member __.Add ws = agent.Post (Add ws)
    member __.Remove ws = agent.Post (Remove ws)
    member __.UiWs (ws, uiWs) = agent.Post (UiWs (ws, uiWs))
    member __.OnReceiveError (ws, exn) = agent.Post (OnReceiveError (ws, exn))

let private wsAgent = WsAgent ()

type WsMiddleware (next:RequestDelegate) =
    member __.Invoke (ctx:HttpContext) = 
        async {
            if ctx.Request.Path = PathString (WS_API) then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
#if DEBUG
                    do! Async.Sleep (random.Next (100, 500))
#endif
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    wsAgent.Add ws
                    let mutable (closed:WebSocketReceiveResult) = null
                    let mutable loop = true
                    while loop do
                        // TODO-NMB: Consider whether buffer size is adequate...
                        let buffer : byte [] = Array.zeroCreate 4096
                        let! receive = ws.ReceiveAsync (new ArraySegment<byte> (buffer), CancellationToken.None) |> Async.AwaitTask
                        if receive.CloseStatus.HasValue then
                            closed <- receive
                            loop <- false
                        else
                            try
                                // Note: Expect buffer to be deserializable to UiWs.
                                let uiWs = ofJson<UiWs> (Encoding.UTF8.GetString buffer)
                                wsAgent.UiWs (ws, uiWs)
                            with exn -> wsAgent.OnReceiveError (ws, exn)
                    ws.CloseAsync (closed.CloseStatus.Value, closed.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    wsAgent.Remove ws
                | false -> ctx.Response.StatusCode <- 400
            else next.Invoke(ctx) |> ignore
        } |> Async.StartAsTask :> Task
