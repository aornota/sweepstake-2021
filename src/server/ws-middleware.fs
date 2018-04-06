module Aornota.Sweepstake2018.Server.WsMiddleware

open Aornota.Sweepstake2018.Server.Agents.Ws
open Aornota.Sweepstake2018.Server.Json
open Aornota.Sweepstake2018.Shared.TODO
open Aornota.Sweepstake2018.Shared.Literals

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Http
    
#if DEBUG
let private random = Random ()
#endif

type WsMiddleware (next:RequestDelegate) =
    let rec receiving (ws:WebSocket) = async {
        // TODO-NMB: Consider whether buffer size is adequate (albeit messages from ui to server should be small)...
        let buffer : byte [] = Array.zeroCreate 4096
        let! receiveResult = ws.ReceiveAsync (new ArraySegment<byte> (buffer), CancellationToken.None) |> Async.AwaitTask
        if receiveResult.CloseStatus.HasValue then return receiveResult
        else
            try // note: Expect buffer to be deserializable to UiWs
                let uiWs = ofJson<UiWs> (Encoding.UTF8.GetString buffer)
                wsAgent.UiWs (ws, uiWs)
            with exn -> wsAgent.OnReceiveError (ws, exn)
            return! receiving ws }
    member __.Invoke (ctx:HttpContext) = 
        async {
            if ctx.Request.Path = PathString WS_API_PATH then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
#if DEBUG
                    do! Async.Sleep (random.Next (100, 500))
#endif
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    wsAgent.Add ws
                    let! closedReceiveResult = receiving ws
                    ws.CloseAsync (closedReceiveResult.CloseStatus.Value, closedReceiveResult.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    wsAgent.Remove ws
                | false -> ctx.Response.StatusCode <- 400
            else next.Invoke (ctx) |> ignore 
        } |> Async.StartAsTask :> Task
