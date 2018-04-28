module Aornota.Sweepstake2018.Server.Agents.Broadcaster

open Aornota.Sweepstake2018.Server.Events.Event

open System
open System.Collections.Generic

type SubscriberId = private | SubscriberId of guid : Guid

type private BroadcasterInput =
    | Broadcast of event : Event
    | Subscribe of onEvent : (Event -> unit) * reply : AsyncReplyChannel<SubscriberId>
    | Unsubscribe of subscriberId : SubscriberId

type Broadcaster () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec running (subscriptions:Dictionary<SubscriberId, Event -> unit>) = async {
            let! message = inbox.Receive ()
            match message with
            | Broadcast event ->
                subscriptions |> List.ofSeq |> List.iter (fun (KeyValue (_, onEvent)) -> onEvent event)
                return! running subscriptions
            | Subscribe (onEvent, reply) ->
                let subscriberId = Guid.NewGuid () |> SubscriberId
                subscriptions.Add (subscriberId, onEvent)
                subscriberId |> reply.Reply
                return! running subscriptions
            | Unsubscribe subscriberId ->
                if subscriptions.ContainsKey subscriberId then subscriptions.Remove subscriberId |> ignore // note: silently ignore unknown subscriberId
                return! running subscriptions }
        running (new Dictionary<SubscriberId, Event -> unit> ()))
    member __.Broadcast event = Broadcast event |> agent.Post
    member __.Subscribe onEvent = (fun reply -> Subscribe (onEvent, reply)) |> agent.PostAndReply
    member __.Unsubscribe subscriberId = Unsubscribe subscriberId |> agent.Post

let broadcaster = Broadcaster ()

// Note: No ensureInstantiated function since host.fs has explicit calls to other agents that will then call Broadcaster agent.
