module Aornota.Sweepstake2018.Server.Agents.Broadcaster

open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event

open System
open System.Collections.Generic

type SubscriptionId = private | SubscriptionId of guid : Guid

type EventFilter = Event -> bool
type LogEventFilter = string * EventFilter

type private BroadcasterInput =
    | Start of logEventFilter : LogEventFilter * reply : AsyncReplyChannel<unit>
    | Broadcast of event : Event
    | Subscribe of onEvent : (Event -> unit) * reply : AsyncReplyChannel<SubscriptionId>
    | Unsubscribe of subscriptionId : SubscriptionId
    | CurrentLogEventFilter of reply : AsyncReplyChannel<LogEventFilter>
    | ChangeLogEventFilter of logEventFilter : LogEventFilter * reply : AsyncReplyChannel<unit>

let private log category = (Broadcaster, category) |> consoleLogger.Log

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
                sprintf "Start when awaitingStart -> broadcasting (log event filter: '%s')" filterName |> Info |> log
                () |> reply.Reply
                return! broadcasting (new Dictionary<SubscriptionId, Event -> unit> (), (filterName, logEventFilter))
            | Broadcast _ -> "Broadcast when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | Subscribe _ -> "Subscribe when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | Unsubscribe _ -> "Unsubscribe when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | CurrentLogEventFilter _ -> "CurrentLogEventFilter when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ChangeLogEventFilter _ -> "ChangeLogEventFilter when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and broadcasting (subscriptions, (filterName, eventFilter)) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when broadcasting" |> IgnoredInput |> Agent |> log ; return! broadcasting (subscriptions, (filterName, eventFilter))
            | Broadcast event ->
                if eventFilter event then sprintf "Broadcast -> %i subscription/s -> %A" subscriptions.Count event |> Info |> log
                subscriptions |> List.ofSeq |> List.iter (fun (KeyValue (_, onEvent)) -> onEvent event)
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | Subscribe (onEvent, reply) ->
                let subscriptionId = Guid.NewGuid () |> SubscriptionId
                (subscriptionId, onEvent) |> subscriptions.Add
                sprintf "Subscribe when broadcasting -> added %A -> %i subscription/s" subscriptionId subscriptions.Count |> Info |> log
                subscriptionId |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | Unsubscribe subscriptionId ->
                if subscriptions.ContainsKey subscriptionId then
                    subscriptionId |> subscriptions.Remove |> ignore
                    sprintf "Unsubscribe when broadcasting -> removed %A -> %i subscription/s" subscriptionId subscriptions.Count |> Info |> log
                else sprintf "Unsubscribe when broadcasting -> unknown %A" subscriptionId |> IgnoredInput |> Agent |> log
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | CurrentLogEventFilter reply ->
                (filterName, eventFilter) |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter))
            | ChangeLogEventFilter ((filterName, logEventFilter), reply) ->
                sprintf "ChangeLogEventFilter when broadcasting -> broadcasting (log event filter: '%s')" filterName |> Info |> log
                () |> reply.Reply
                return! broadcasting (subscriptions, (filterName, eventFilter)) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Broadcaster |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start logEventFilter = (fun reply -> (logEventFilter, reply) |> Start) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.Broadcast event = event |> Broadcast |> agent.Post
    member __.SubscribeAsync onEvent = (fun reply -> (onEvent, reply) |> Subscribe) |> agent.PostAndAsyncReply
    member __.Unsubscribe subscriptionId = subscriptionId |> Unsubscribe |> agent.Post
    member __.CurrentLogFilter () = CurrentLogEventFilter |> agent.PostAndReply
    member __.ChangeLogEventFilter logEventFilter = (fun reply -> (logEventFilter, reply) |> ChangeLogEventFilter) |> agent.PostAndReply

let broadcaster = Broadcaster ()
