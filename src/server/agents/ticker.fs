module Aornota.Sweepstake2018.Server.Agents.Ticker

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event

type private TickerInput =
    | Start of secondsPerTick : int<second/tick> * reply : AsyncReplyChannel<unit>
    | Stop of reply : AsyncReplyChannel<unit>

let private log category = consoleLogger.Log (Ticker, category)

type Ticker () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start (secondsPerTick, reply) ->
                let secondsPerTick = if secondsPerTick > 0<second/tick> then secondsPerTick else 1<second/tick>
                log (Info (sprintf "Start when awaitingStart -> ticking (%i<second/tick>)" (int secondsPerTick)))
                () |> reply.Reply
                return! ticking (secondsPerTick, None)
            | Stop _ -> log (Agent (IgnoredInput "Stop when awaitingStart")) ; return! awaitingStart () }
        and ticking (secondsPerTick, ticks) = async {
            let millisecondsPerTick = ((float secondsPerTick) * 1.<second/tick>) * MILLISECONDS_PER_SECOND
            do! Async.Sleep (int millisecondsPerTick)
            let ticks = match ticks with | Some ticks -> ticks + 1<tick> | None -> 1<tick>
            Tick (ticks, secondsPerTick) |> broadcaster.Broadcast
            // Note: Only call inbox.Receive () if queue is non-empty (since would otherwise block until non-empty, thus preventing further Tick broadcasts).
            if inbox.CurrentQueueLength > 0 then
                let! input = inbox.Receive ()
                match input with
                | Start _ -> log (Agent (IgnoredInput "Start when ticking")) ; return! ticking (secondsPerTick, Some ticks)
                | Stop reply ->
                    log (Info (sprintf "Stop when ticking (%i<tick>) -> awaitingStart" (int ticks)))
                    () |> reply.Reply
                    return! awaitingStart ()
            else return! ticking (secondsPerTick, Some ticks) }
        log (Info "agent instantiated -> awaitingStart")
        awaitingStart ())
    do agent.Error.Add (logAgentException Source.Ticker) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start secondsPerTick = (fun reply -> Start (secondsPerTick, reply)) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.Stop () = Stop |> agent.PostAndReply

let isEveryNSeconds (everyN:int<second>) (ticks, secondsPerTick) =
    let seconds = ticks * secondsPerTick
    if seconds < everyN then false
    else seconds % everyN < secondsPerTick * 1<tick>

let ticker = Ticker ()
