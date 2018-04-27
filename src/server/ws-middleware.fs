module Aornota.Sweepstake2018.Server.WsMiddleware

open Aornota.Common.Json

open Aornota.Server.Common.Json

open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.Server.Agents.Connections
open Aornota.Sweepstake2018.Server.Connection

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Http
    
#if DEBUG
let private random = Random ()
let private fakeErrorFrequency = 0.02
#endif

type WsMiddleware (next:RequestDelegate) =
    let rec receiving (connectionId, ws:WebSocket) = async {
        let buffer : byte [] = Array.zeroCreate 4096
        try // note: buffer size should be adequate (as serialized UiMsg data should be relatively small)
            let! receiveResult = ws.ReceiveAsync (new ArraySegment<byte> (buffer), CancellationToken.None) |> Async.AwaitTask
#if DEBUG
            if random.NextDouble () < fakeErrorFrequency then failwith (sprintf "Fake error receiving message for %A" connectionId)
#endif
            if receiveResult.CloseStatus.HasValue then return receiveResult
            else
                try // note: expect buffer to be deserializable to UiMsg              
                    let uiApi = Json (Encoding.UTF8.GetString buffer) |> ofJson<UiMsg>
#if DEBUG
                    if random.NextDouble () < fakeErrorFrequency then failwith (sprintf "Fake error deserializing %A for %A" uiApi connectionId)
#endif
                    connections.HandleUiMsg (connectionId, uiApi)
                    return! receiving (connectionId, ws)
                with exn ->
                    connections.OnDeserializeUiMsgError (connectionId, exn)
                    return! receiving (connectionId, ws)
        with exn ->
            connections.OnReceiveUiMsgError (connectionId, exn)
            return! receiving (connectionId, ws) }
    member __.Invoke (ctx:HttpContext) = 
        async {
            if ctx.Request.Path = PathString WS_API_PATH then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
#if DEBUG
                    do! Async.Sleep (random.Next (25, 125))
#endif
                    let connectionId = ConnectionId.Create ()
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    connections.AddConnection (connectionId, ws)
                    let! closedReceiveResult = receiving (connectionId, ws)
                    ws.CloseAsync (closedReceiveResult.CloseStatus.Value, closedReceiveResult.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    connections.RemoveConnection connectionId
                | false -> ctx.Response.StatusCode <- 400
            else next.Invoke (ctx) |> ignore 
        } |> Async.StartAsTask :> Task
