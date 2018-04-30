module Aornota.Sweepstake2018.Server.Agents.Broadcaster

open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event

open System
open System.Collections.Generic

type SubscriberId = private | SubscriberId of guid : Guid

type EventFilter = Event -> bool
type LogEventFilter = string * EventFilter

type private BroadcasterInput =
    | Start of logEventFilter : LogEventFilter * reply : AsyncReplyChannel<unit>
    | Broadcast of event : Event
    | Subscribe of onEvent : (Event -> unit) * reply : AsyncReplyChannel<SubscriberId>
    | Unsubscribe of subscriberId : SubscriberId
    | CurrentLogEventFilter of reply : AsyncReplyChannel<LogEventFilter>
    | ChangeLogEventFilter of logEventFilter : LogEventFilter * reply : AsyncReplyChannel<unit>

let private log category = consoleLogger.Log (Broadcaster, category)

let private allEvents : EventFilter = function | _ -> true
let private allExceptTick : EventFilter = function | Tick _ -> false | _ -> true
let private noEvents : EventFilter = function | _ -> false

let logAllEvents : LogEventFilter = "all events", allEvents
let logAllEventsExceptTick : LogEventFilter = "all events except Tick", allExceptTick
let logNoEvents : LogEventFilter = "no events", noEvents

type Broadcaster () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start ((filterName, logEventFilter), reply) ->
                log (Info (sprintf "Start when awaitingStart -> broadcasting (log event filter: '%s')" filterName))
                () |> reply.Reply
                return! broadcasting (new Dictionary<SubscriberId, Event -> unit> (), (filterName, logEventFilter))
            | Broadcast _ -> log (Agent (IgnoredInput "Broadcast when awaitingStart")) ; return! awaitingStart ()
            | Subscribe _ -> log (Agent (IgnoredInput "Subscribe when awaitingStart")) ; return! awaitingStart ()
            | Unsubscribe _ -> log (Agent (IgnoredInput "Unsubscribe when awaitingStart")) ; return! awaitingStart ()
            | CurrentLogEventFilter _ -> log (Agent (IgnoredInput "CurrentLogEventFilter when awaitingStart")) ; return! awaitingStart ()
            | ChangeLogEventFilter _ -> log (Agent (IgnoredInput "ChangeLogEventFilter when awaitingStart")) ; return! awaitingStart () }
        and broadcasting (subscriptions, (filterName, eventFilter)) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> log (Agent (IgnoredInput "Start when broadcasting")) ; return! broadcasting (subscriptions, (filterName, eventFilter))
            | Broadcast event ->
                if eventFilter event then log (Info (sprintf "Broadcast -> %i subscriber/s -> %A" subscriptions.Count event))
                subscriptions |> List.ofSeq |> List.iter (fun (KeyValue (_, onEvent)) -> onEvent event)
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | Subscribe (onEvent, reply) ->
                let subscriberId = Guid.NewGuid () |> SubscriberId
                subscriptions.Add (subscriberId, onEvent)
                log (Info (sprintf "Subscribe when broadcasting -> added %A -> %i subscriber/s" subscriberId subscriptions.Count))
                subscriberId |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | Unsubscribe subscriberId ->
                if subscriptions.ContainsKey subscriberId then
                    subscriptions.Remove subscriberId |> ignore
                    log (Info (sprintf "Unsubscribe when broadcasting -> removed %A -> %i subscriber/s" subscriberId subscriptions.Count))
                else log (Agent (IgnoredInput (sprintf "Unsubscribe when broadcasting -> unknown %A" subscriberId)))
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | CurrentLogEventFilter reply ->
                (filterName, eventFilter) |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | ChangeLogEventFilter ((filterName, logEventFilter), reply) ->
                log (Info (sprintf "ChangeLogEventFilter when broadcasting -> broadcasting (log event filter: '%s')" filterName))
                () |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter)) }
        log (Info "agent instantiated -> awaitingStart")
        awaitingStart ())
    do agent.Error.Add (logAgentException Source.Broadcaster) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start logEventFilter = (fun reply -> Start (logEventFilter, reply)) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.Broadcast event = Broadcast event |> agent.Post
    member __.SubscribeAsync onEvent = (fun reply -> Subscribe (onEvent, reply)) |> agent.PostAndAsyncReply
    member __.Unsubscribe subscriberId = Unsubscribe subscriberId |> agent.Post
    member __.CurrentLogFilter () = CurrentLogEventFilter |> agent.PostAndReply
    member __.ChangeLogEventFilter logEventFilter = (fun reply -> ChangeLogEventFilter (logEventFilter, reply)) |> agent.PostAndReply

let broadcaster = Broadcaster ()
