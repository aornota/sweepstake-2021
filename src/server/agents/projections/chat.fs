module Aornota.Sweepstake2018.Server.Agents.Projections.Chat

// Note: Chat agent broadcasts SendMsg - and subscribes to Tick | UsersRead | UserEventWritten | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected.

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Ticker
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.UserEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private ChatInput =
    | Start of reply : AsyncReplyChannel<unit>
    | Housekeeping
    | OnUsersRead of usersRead : UserRead list
    | OnUserCreated of userId : UserId * userName : UserName * userType : UserType
    | OnUserTypeChanged of userId : UserId * userType : UserType
    | OnUserSignedInOrOut of userId : UserId * signedIn : bool
    | OnUserApi of userId : UserId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeChatProjectionQry of token : InitializeChatProjectionToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<unit, AuthQryError<string>>> // TODO-NEXT: Something other than unit...
    | HandleSendChatMessageCmd of token : SendChatMessageToken * userId : UserId * chatMessageId : ChatMessageId * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    
type private ChatUser = { UserName : UserName ; LastApi : DateTimeOffset option } // note: list will exclude PersonaNonGrata users

type private ChatMessage = { UserId : UserId ; MessageText : Markdown ; MessageTimestamp : DateTimeOffset ; Expired : bool }

let [<Literal>] private HOUSEKEEPING_INTERVAL = 60<second>

let private log category = (Projection Chat, category) |> consoleLogger.Log

// TODO-NMB-HIGH: "State" will be ChatUsers+ChatMessages+ConnectionIds (?)...

type Chat () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> projectingChat (0 chat users) (0 chat messages) (0 connections)" |> Info |> log
                () |> reply.Reply
                return! projectingChat (new Dictionary<UserId, ChatUser> (), new Dictionary<ChatMessageId, ChatMessage> (), [])
            | Housekeeping _ -> "Housekeeping when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserApi _ -> "OnUserApi when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeChatProjectionQry _ -> "HandleInitializeChatProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleSendChatMessageCmd _ -> "HandleSendChatMessageCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        // TODO-NEXT: pendingOnUsersRead ()...
        and projectingChat (chatUsers, chatMessages, connections) = async {
            do! Async.Sleep 1000
        }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Chat |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | Tick (ticks, secondsPerTick) -> if (ticks, secondsPerTick) |> isEveryNSeconds HOUSEKEEPING_INTERVAL then Housekeeping |> agent.Post
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | UserEventWritten (_, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, userType) -> (userId, userName, userType) |> OnUserCreated |> agent.Post
                | UserTypeChanged (userId, userType) -> (userId, userType) |> OnUserTypeChanged |> agent.Post
                | _ -> ()
            | UserSignedIn userId -> (userId, true) |> OnUserSignedInOrOut |> agent.Post
            | UserSignedOut userId -> (userId, false) |> OnUserSignedInOrOut |> agent.Post
            | UserApi userId -> userId |> OnUserApi |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeChatProjectionQry (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleInitializeChatProjectionQry) |> agent.PostAndAsyncReply
    member __.HandleSendChatMessageCmd (token, userId, chatMessageId, messageText) =
        (fun reply -> (token, userId, chatMessageId, messageText, reply) |> HandleSendChatMessageCmd) |> agent.PostAndAsyncReply

let chat = Chat ()
