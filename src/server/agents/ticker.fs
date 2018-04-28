module Aornota.Sweepstake2018.Server.Agents.Ticker

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Events.Event

type private TickerInput = 
    | Start
    | Stop

type Ticker (secondsPerTick) =
    let secondsPerTick = if secondsPerTick > 0<second/tick> then secondsPerTick else 1<second/tick>
    let millisecondsPerTick = ((float secondsPerTick) * 1.<second/tick>) * MILLISECONDS_PER_SECOND
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec stopped () = async {
            let! message = inbox.Receive ()
            match message with
            | Start -> return! running None
            | Stop -> return! stopped () }
        and running ticks = async {
            do! Async.Sleep (int millisecondsPerTick)
            let ticks = match ticks with | Some ticks -> ticks + 1<tick> | None -> 1<tick>
            Tick (ticks, secondsPerTick) |> broadcaster.Broadcast
            let! message = inbox.Receive ()
            match message with
            | Start -> return! running (Some ticks)
            | Stop -> return! stopped () }
        stopped ())
    member __.Start () = Start |> agent.Post
    member __.Stop () = Stop |> agent.Post

let [<Literal>] private SECONDS_PER_TICK = 1<second/tick>

let isEveryNSeconds (everyN:int<second>) (ticks, secondsPerTick) =
    let seconds = ticks * secondsPerTick
    if seconds < everyN then false
    else seconds % everyN < secondsPerTick * 1<tick>

let ticker = Ticker SECONDS_PER_TICK

// Note: No ensureInstantiated function since host.fs has explicit call to ticker.Start.
