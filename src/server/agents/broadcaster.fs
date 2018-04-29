module Aornota.Sweepstake2018.Server.Agents.Broadcaster

open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event

open System
open System.Collections.Generic

type SubscriberId = private | SubscriberId of guid : Guid

type private BroadcasterInput =
    | Broadcast of event : Event
    | Subscribe of onEvent : (Event -> unit) * reply : AsyncReplyChannel<SubscriberId>
    | Unsubscribe of subscriberId : SubscriberId

let private log category = consoleLogger.Log (Broadcaster, category)

type Broadcaster () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec receiving (subscriptions:Dictionary<SubscriberId, Event -> unit>) = async {
            let! input = inbox.Receive ()
            match input with
            | Broadcast event ->
                match event with | Tick _ -> () | _ -> log (Info (sprintf "Broadcast -> %i subscriber/s -> %A" subscriptions.Count event)) // note: no point logging Tick events
                subscriptions |> List.ofSeq |> List.iter (fun (KeyValue (_, onEvent)) -> onEvent event)
                return! receiving subscriptions
            | Subscribe (onEvent, reply) ->
                let subscriberId = Guid.NewGuid () |> SubscriberId
                subscriptions.Add (subscriberId, onEvent)
                log (Info (sprintf "Subscribe -> added %A -> %i subscriber/s" subscriberId subscriptions.Count))
                subscriberId |> reply.Reply
                return! receiving subscriptions
            | Unsubscribe subscriberId ->
                if subscriptions.ContainsKey subscriberId then
                    subscriptions.Remove subscriberId |> ignore
                    log (Info (sprintf "Unsubscribe -> removed %A -> %i subscriber/s" subscriberId subscriptions.Count))
                else log (IgnoredInput (sprintf "Unsubscribe -> unknown %A" subscriberId))
                return! receiving subscriptions }
        log (Info "agent instantiated -> receiving")
        receiving (new Dictionary<SubscriberId, Event -> unit> ()))
    do agent.Error.Add (logAgentExn Source.Broadcaster) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Broadcast event = Broadcast event |> agent.Post
    member __.Subscribe onEvent = (fun reply -> Subscribe (onEvent, reply)) |> agent.PostAndReply
    member __.Unsubscribe subscriberId = Unsubscribe subscriberId |> agent.Post

let broadcaster = Broadcaster ()

// Note: No ensureInstantiated function since host.fs has explicit calls to other agents that will then call (and hence instantiate) Broadcaster agent.
