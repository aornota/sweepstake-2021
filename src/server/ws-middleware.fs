module Aornota.Sweepstake2018.Server.WsMiddleware

open Aornota.Sweepstake2018.Server.Agents.Connections
open Aornota.Sweepstake2018.Server.Json
open Aornota.Sweepstake2018.Shared.Literals
open Aornota.Sweepstake2018.Shared.Ws.Ui

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Http
    
#if DEBUG
//let private random = Random ()
#endif

type WsMiddleware (next:RequestDelegate) =
    let rec receiving (connectionId, ws:WebSocket) = async {
        let buffer : byte [] = Array.zeroCreate 4096
        try // note: buffer size should be adequate (as messages from ui to server should be small)
            let! receiveResult = ws.ReceiveAsync (new ArraySegment<byte> (buffer), CancellationToken.None) |> Async.AwaitTask
#if DEBUG
            //if random.NextDouble () < 0.75 then failwith (sprintf "Fake error receiving message for %A" connectionId)
#endif
            if receiveResult.CloseStatus.HasValue then return receiveResult
            else
                try // note: expect buffer to be deserializable to UiWsApi               
                    let uiWs = ofJson<UiWsApi> (Encoding.UTF8.GetString buffer)
#if DEBUG
                    //if random.NextDouble () < 0.75 then failwith (sprintf "Fake error deserializing %A for %A" uiWs connectionId)
#endif
                    connectionsAgent.HandleUiWsApi (connectionId, uiWs)
                    return! receiving (connectionId, ws)
                with exn ->
                    connectionsAgent.OnDeserializeUiWsApiError (connectionId, exn)
                    return! receiving (connectionId, ws)
        with exn ->
            connectionsAgent.OnReceiveError (connectionId, exn)
            return! receiving (connectionId, ws) }
    member __.Invoke (ctx:HttpContext) = 
        async {
            if ctx.Request.Path = PathString WS_API_PATH then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
#if DEBUG
                    //do! Async.Sleep (random.Next (100, 500))
#endif
                    let connectionId = ConnectionId.Create ()
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    connectionsAgent.AddConnection (connectionId, ws)
                    let! closedReceiveResult = receiving (connectionId, ws)
                    ws.CloseAsync (closedReceiveResult.CloseStatus.Value, closedReceiveResult.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    connectionsAgent.RemoveConnection connectionId
                | false -> ctx.Response.StatusCode <- 400
            else next.Invoke (ctx) |> ignore 
        } |> Async.StartAsTask :> Task
