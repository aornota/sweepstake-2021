module Aornota.Sweepstake2018.Server.WsMiddleware

open Aornota.Common.IfDebug
open Aornota.Common.Json

open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.Server.Agents.Connections
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Connection

open System
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Http
    
let private log category = consoleLogger.Log (WsMiddleware, category)

type WsMiddleware (next:RequestDelegate) =
    let rec receiving (connectionId, ws:WebSocket) receiveFailureCount = async {
        let buffer : byte [] = Array.zeroCreate 4096
        try // note: buffer size should be adequate (as serialized UiMsg data should be relatively small)
            let! receiveResult = ws.ReceiveAsync (new ArraySegment<byte> (buffer), CancellationToken.None) |> Async.AwaitTask
            log (Verbose (sprintf "receiving message for %A" connectionId))
            ifDebugFakeErrorFailWith (sprintf "Fake error receiving message for %A" connectionId)
            if receiveResult.CloseStatus.HasValue then return Some receiveResult
            else
                try // note: expect buffer to be deserializable to UiMsg              
                    log (Verbose (sprintf "deserializing message for %A" connectionId))
                    let uiMsg = Json (Encoding.UTF8.GetString buffer) |> ofJson<UiMsg>
                    ifDebugFakeErrorFailWith (sprintf "Fake error deserializing %A for %A" uiMsg connectionId)
                    log (Verbose (sprintf "message deserialized for %A -> %A" connectionId uiMsg))
                    connections.HandleUiMsg (connectionId, uiMsg)
                    return! receiving (connectionId, ws) receiveFailureCount
                with exn ->
                    log (Danger (sprintf "deserializing message failed for %A -> %A" connectionId exn.Message))
                    connections.OnDeserializeUiMsgError (connectionId, exn)
                    return! receiving (connectionId, ws) receiveFailureCount
        with exn ->
            let receiveFailureCount = receiveFailureCount + 1u
            log (Danger (sprintf "receiving message failed for %A -> receive failure count %i -> %A" connectionId receiveFailureCount exn.Message))
            if ws.State = WebSocketState.Open then connections.OnReceiveUiMsgError (connectionId, exn) // note: attempt to send message
            // Note: Try to avoid infinite loop, e.g. of exceptions from ws.ReceiveAsync (...) calls.
            if ws.State = WebSocketState.Open && receiveFailureCount < 3u then
                do! Async.Sleep 1000 // note: just in case it helps
                return! receiving (connectionId, ws) receiveFailureCount
            else return None }
    member __.Invoke (ctx:HttpContext) = 
        async {
            if ctx.Request.Path = PathString WS_API_PATH then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
                    log (Verbose "new web socket request")
                    do! ifDebugSleepAsync 25 125
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    let connectionId = ConnectionId.Create ()
                    log (Verbose (sprintf "new web socket accepted -> %A" connectionId))
                    connections.AddConnection (connectionId, ws)
                    let! receiveResult = receiving (connectionId, ws) 0u
                    log (Verbose (sprintf "web socket closing -> %A" connectionId))
                    match receiveResult with
                    | Some receiveResult -> ws.CloseAsync (receiveResult.CloseStatus.Value, receiveResult.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    | None -> ()
                    connections.RemoveConnection connectionId
                    log (Verbose (sprintf "web socket closed -> %A" connectionId))
                | false -> ctx.Response.StatusCode <- 400
            else next.Invoke ctx |> ignore 
        } |> Async.StartAsTask :> Task
