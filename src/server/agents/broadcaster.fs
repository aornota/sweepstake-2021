module Aornota.Sweepstake2018.Server.Agents.Broadcaster

open Aornota.Sweepstake2018.Server.Events.Event

open System

type SubscriberId = private | SubscriberId of guid : Guid

type private Subscription = {
    SubscriberId : SubscriberId
    OnEvent : Event -> unit }

type private BroadcasterInput =
    | Broadcast of event : Event
    | Subscribe of onEvent : (Event -> unit) * reply : AsyncReplyChannel<SubscriberId>
    | Unsubscribe of subscriberId : SubscriberId

type Broadcaster () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec running subscriptions = async {
            let! message = inbox.Receive ()
            match message with
            | Broadcast event ->
                subscriptions |> List.iter (fun subscription -> subscription.OnEvent event)
                return! running subscriptions
            | Subscribe (onEvent, reply) ->
                let subscriberId = Guid.NewGuid () |> SubscriberId
                let subscriptions = { SubscriberId = subscriberId ; OnEvent = onEvent } :: subscriptions
                subscriberId |> reply.Reply
                return! running subscriptions
            | Unsubscribe subscriberId ->
                let subscriptions = subscriptions |> List.filter (fun subscription -> subscription.SubscriberId <> subscriberId) // note: silently ignore unknown subscriberId
                return! running subscriptions }
        running [])
    member __.Broadcast event = Broadcast event |> agent.Post
    member __.Subscribe onEvent = (fun reply -> Subscribe (onEvent, reply)) |> agent.PostAndReply
    member __.Unsubscribe subscriberId = Unsubscribe subscriberId |> agent.Post

let broadcaster = Broadcaster ()
