module Aornota.Sweepstake2018.Server.Agents.Ticker

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event

type private TickerInput = | Start | Stop

let [<Literal>] private SECONDS_PER_TICK = 1<second/tick>

let private log category = consoleLogger.Log (Ticker, category)

type Ticker (secondsPerTick) =
    let secondsPerTick = if secondsPerTick > 0<second/tick> then secondsPerTick else 1<second/tick>
    let millisecondsPerTick = ((float secondsPerTick) * 1.<second/tick>) * MILLISECONDS_PER_SECOND
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec idle () = async {
            let! input = inbox.Receive () // note: not Scan (...) in order to consume "ignored" inputs (rather than leaving them on the queue)
            match input with
            | Start ->
                log (Info (sprintf "Start when idle -> started ticking -> %i<second/tick>" (int secondsPerTick)))
                return! ticking None
            | Stop ->
                log (IgnoredInput "Stop when idle")
                return! idle () }
        and ticking ticks = async {
            do! Async.Sleep (int millisecondsPerTick)
            let ticks = match ticks with | Some ticks -> ticks + 1<tick> | None -> 1<tick>
            Tick (ticks, secondsPerTick) |> broadcaster.Broadcast
            // Note: Only call inbox.Receive () if queue is non-empty (since would otherwise block until non-empty, thus preventing further Tick broadcasts).
            if inbox.CurrentQueueLength > 0 then
                let! input = inbox.Receive () // note: not Scan (...) in order to consume "ignored" inputs (rather than leaving them on the queue)
                match input with
                | Start ->
                    log (IgnoredInput "Start when ticking")
                    return! ticking (Some ticks)
                | Stop ->
                    log (Info (sprintf "Stop when ticking -> %i<tick> -> idle" (int ticks)))
                    return! idle ()
            else return! ticking (Some ticks) }
        log (Info "agent instantiated -> idle")
        idle ())
    do agent.Error.Add (logAgentExn Source.Ticker) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () = Start |> agent.Post
    member __.Stop () = Stop |> agent.Post

let isEveryNSeconds (everyN:int<second>) (ticks, secondsPerTick) =
    let seconds = ticks * secondsPerTick
    if seconds < everyN then false
    else seconds % everyN < secondsPerTick * 1<tick>

let ticker = Ticker SECONDS_PER_TICK

// Note: No ensureInstantiated function since host.fs has explicit call to ticker.Start.
