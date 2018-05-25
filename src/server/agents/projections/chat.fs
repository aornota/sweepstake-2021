module Aornota.Sweepstake2018.Server.Agents.Projections.Chat

// Note: Chat agent broadcasts SendMsg - and subscribes to Tick | UsersRead | UserEventWritten | UserSignedIn | UserActivity | UserSignedOut | ConnectionsSignedOut | Disconnected.

open Aornota.Common.IfDebug
open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError
open Aornota.Common.UnitsOfMeasure

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.Chat
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
    | OnUserActivity of userId : UserId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeChatProjectionQry of token : InitializeChatProjectionToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<ChatProjectionDto, AuthQryError<string>>>
    | HandleSendChatMessageCmd of token : SendChatMessageToken * userId : UserId * chatMessageId : ChatMessageId * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    
type private ChatUser = { UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }
type private ChatUserDic = Dictionary<UserId, ChatUser>

type private ChatMessage = { UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset }
type private ChatMessageDic = Dictionary<ChatMessageId, ChatMessage>

type private ConnectionSet = HashSet<ConnectionId>

type private ChatUserDtoDic = Dictionary<UserId, ChatUserDto>
type private ChatMessageDtoDic = Dictionary<ChatMessageId, ChatMessageDto>
type private ChatProjection = { Rvn : Rvn ; ChatUserDtoDic : ChatUserDtoDic ; ChatMessageDtoDic : ChatMessageDtoDic }

let [<Literal>] private HOUSEKEEPING_INTERVAL = 1.<minute>
let [<Literal>] private CHAT_MESSAGE_EXPIRES_AFTER = 24.<hour>
let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>

let private log category = (Projection Chat, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private sendMsg (connectionSet:ConnectionSet) serverMsg = (serverMsg, connectionSet |> List.ofSeq) |> SendMsg |> broadcaster.Broadcast

let private chatProjectionDto (chatProjection:ChatProjection) =
    let chatUserDtos = chatProjection.ChatUserDtoDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatUser)) -> chatUser)
    let chatMessageDtos = chatProjection.ChatMessageDtoDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage)
    { Rvn = chatProjection.Rvn ; ChatUserDtos = chatUserDtos ; ChatMessageDtos = chatMessageDtos }

let private updateProjection source (chatUserDic:ChatUserDic) (chatMessageDic:ChatMessageDic) (connectionSet:ConnectionSet) (chatProjection:ChatProjection option) =
    let source = sprintf "%s#updateProjection" source  
    let chatUserDto filter (userId, chatUser:ChatUser) =
        if chatUser |> filter then { UserId = userId ; UserName = chatUser.UserName ; LastActivity = chatUser.LastActivity } |> Some else None
    let chatMessageDto (chatMessageId, chatMessage:ChatMessage) =
        { ChatMessageId = chatMessageId ; UserId = chatMessage.UserId ; MessageText = chatMessage.MessageText ; Timestamp = chatMessage.Timestamp }
    let chatUserDtoDic = ChatUserDtoDic ()
    let filter = (fun chatUser -> match chatUser.UserType with | SuperUser | Administrator | Pleb -> true | PersonaNonGrata -> false)
    chatUserDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, chatUser)) ->
        match (userId, chatUser) |> chatUserDto filter with
        | Some chatUserDto -> (userId, chatUserDto) |> chatUserDtoDic.Add
        | None -> ())
    let chatMessageDtoDic = ChatMessageDtoDic ()
    chatMessageDic |> List.ofSeq |> List.iter (fun (KeyValue (chatMessageId, chatMessage)) ->
        let chatMessageDto = (chatMessageId, chatMessage) |> chatMessageDto
        (chatMessageId, chatMessageDto) |> chatMessageDtoDic.Add)
    let newChatProjection =
        match chatProjection with
        | Some chatProjection ->
            let nextRvn = chatProjection.Rvn
            let chatUserDtoDelta = chatUserDtoDic |> delta chatProjection.ChatUserDtoDic
            let nextRvn =
                if chatUserDtoDelta |> isEmpty then nextRvn
                else
                    let nextRvn = incrementRvn nextRvn
                    (nextRvn, chatUserDtoDelta) |> ChatUsersDeltaMsg |> ChatProjectionMsg |> ServerChatMsg |> sendMsg connectionSet
                    sprintf "%s -> %A %A -> %i connection/s" source nextRvn chatUserDtoDelta connectionSet.Count |> Info |> log
                    nextRvn
            let chatMessageDtoDelta = chatMessageDtoDic |> delta chatProjection.ChatMessageDtoDic
            let nextRvn =
                if chatMessageDtoDelta |> isEmpty then nextRvn
                else
                    let nextRvn = incrementRvn nextRvn
                    (nextRvn, chatMessageDtoDelta) |> ChatMessagesDeltaMsg |> ChatProjectionMsg |> ServerChatMsg |> sendMsg connectionSet
                    sprintf "%s -> %A %A -> %i connection/s" source nextRvn chatMessageDtoDelta connectionSet.Count |> Info |> log
                    nextRvn
            let newChatProjection =
                if nextRvn = chatProjection.Rvn then
                    sprintf "%s -> ChatProjection %A (unchanged)" source chatProjection.Rvn |> Info |> log
                    chatProjection
                else
                    sprintf "%s -> ChatProjection %A" source nextRvn |> Info |> log
                    { Rvn = nextRvn ; ChatUserDtoDic = chatUserDtoDic ; ChatMessageDtoDic = chatMessageDtoDic }
            newChatProjection
        | None ->
            let initialRvn = Rvn 1
            sprintf "%s -> ChatProjection %A" source initialRvn |> Info |> log
            { Rvn = initialRvn ; ChatUserDtoDic = chatUserDtoDic ; ChatMessageDtoDic = chatMessageDtoDic }
    newChatProjection

type Chat () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> projectingChat (0 chat users) (0 chat messages) (0 connections)" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersRead ()
            | Housekeeping -> "Housekeeping when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserActivity _ -> "OnUserActivity when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeChatProjectionQry _ -> "HandleInitializeChatProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleSendChatMessageCmd _ -> "HandleSendChatMessageCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | Housekeeping -> "Housekeeping when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUsersRead usersRead ->
                let source = "OnUsersRead"
                sprintf "%s (%i user/s) when pendingOnUsersRead" source usersRead.Length |> Info |> log
                let chatUserDic = ChatUserDic ()
                usersRead |> List.iter (fun userRead -> (userRead.UserId, { UserName = userRead.UserName ; UserType = userRead.UserType ; LastActivity = None }) |> chatUserDic.Add)           
                let chatMessageDic = ChatMessageDic ()
                let connectionSet = ConnectionSet ()
                let projection = None |> updateProjection source chatUserDic chatMessageDic connectionSet
                return! projectingChat (projection, chatUserDic, chatMessageDic, connectionSet)
            | OnUserCreated _ -> "OnUserCreated when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserActivity _ -> "OnUserActivity when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleInitializeChatProjectionQry _ -> "HandleInitializeChatProjectionQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleSendChatMessageCmd _ -> "HandleSendChatMessageCmd when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead () }
        and projectingChat (chatProjection, chatUserDic, chatMessageDic, connectionSet) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingChat" |> IgnoredInput |> Agent |> log ; return! projectingChat (chatProjection, chatUserDic, chatMessageDic, connectionSet)
            | Housekeeping ->
                let source = "Housekeeping"
                sprintf "%s when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                let expirationCutoff = cutoff (int (CHAT_MESSAGE_EXPIRES_AFTER |> hoursToSeconds) * 1<second>)
                let updatedChatMessageDic = ChatMessageDic ()
                chatMessageDic |> List.ofSeq |> List.iter (fun (KeyValue (chatMessageId, chatMessage)) ->
                    if chatMessage.Timestamp > expirationCutoff then (chatMessageId, chatMessage) |> updatedChatMessageDic.Add)
                let projection, chatMessageDtoDic = chatProjection |> Some |> updateProjection source chatUserDic updatedChatMessageDic connectionSet, updatedChatMessageDic
                return! projectingChat (projection, chatUserDic, chatMessageDtoDic, connectionSet)
            | OnUsersRead _ -> "OnUsersRead when projectingChat" |> IgnoredInput |> Agent |> log ; return! projectingChat (chatProjection, chatUserDic, chatMessageDic, connectionSet)
            | OnUserCreated (userId, userName, userType) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source userId userName userType chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                if userId |> chatUserDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, { UserName = userName ; UserType = userType ; LastActivity = None }) |> chatUserDic.Add
                sprintf "%s when projectingChat -> %i chat users/s)" source chatUserDic.Count |> Info |> log
                let projection = chatProjection |> Some |> updateProjection source chatUserDic chatMessageDic connectionSet
                return! projectingChat (projection, chatUserDic, chatMessageDic, connectionSet)
            | OnUserTypeChanged (userId, userType) ->
                let source = "OnUserTypeChanged"
                sprintf "%s (%A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source userId userType chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                let updatedChatMessageDic =
                    if userId |> chatUserDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                        let chatUser = chatUserDic.[userId]
                        chatUserDic.[userId] <- { chatUser with UserType = userType }
                        let updatedChatMessageDic =
                            match chatUser.UserType with
                            | PersonaNonGrata ->
                                let updatedChatMessageDic = ChatMessageDic ()
                                chatMessageDic |> List.ofSeq |> List.iter (fun (KeyValue (chatMessageId, chatMessage)) ->
                                    if chatMessage.UserId <> userId then (chatMessageId, chatMessage) |> updatedChatMessageDic.Add)
                                updatedChatMessageDic
                            | SuperUser | Administrator | Pleb -> chatMessageDic
                        updatedChatMessageDic
                    else chatMessageDic
                sprintf "%s when projectingChat -> %i chat users/s)" source chatUserDic.Count |> Info |> log
                let projection, chatMessageDtoDic = chatProjection |> Some |> updateProjection source chatUserDic updatedChatMessageDic connectionSet, updatedChatMessageDic
                return! projectingChat (projection, chatUserDic, chatMessageDtoDic, connectionSet)
            | OnUserSignedInOrOut (userId, signedIn) ->
                let source = "OnUserSignedInOrOut"
                sprintf "%s (%A %b) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source userId signedIn chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                if userId |> chatUserDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let chatUser = chatUserDic.[userId]
                    chatUser.UserName |> (if signedIn then UserSignedInMsg else UserSignedOutMsg) |> ChatProjectionMsg |> ServerChatMsg |> sendMsg connectionSet
                    chatUserDic.[userId] <- { chatUser with LastActivity = match signedIn with | true -> DateTimeOffset.UtcNow |> Some | false -> None }
                let projection = chatProjection |> Some |> updateProjection source chatUserDic chatMessageDic connectionSet
                return! projectingChat (projection, chatUserDic, chatMessageDic, connectionSet)
            | OnUserActivity userId ->
                let source = "OnUserActivity"
                sprintf "%s (%A) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source userId chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                let updated =
                    if userId |> chatUserDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                        let chatUser = chatUserDic.[userId]
                        let now = DateTimeOffset.UtcNow
                        let throttled = match chatUser.LastActivity with | Some lastActivity -> (now - lastActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE | None -> false
                        if throttled |> not then chatUserDic.[userId] <- { chatUser with LastActivity = now |> Some }
                        else sprintf "%s throttled for %A" source userId |> Info |> log
                        throttled |> not
                    else false
                let projection = if updated then chatProjection |> Some |> updateProjection source chatUserDic chatMessageDic connectionSet else chatProjection
                return! projectingChat (projection, chatUserDic, chatMessageDic, connectionSet)
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source connectionIds chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                connectionIds // note: silently ignore unknown connectionIds
                |> List.iter (fun connectionId -> if connectionId |> connectionSet.Contains then connectionId |> connectionSet.Remove |> ignore)
                sprintf "%s when projectingChat -> %i connection/s)" source connectionSet.Count |> Info |> log
                return! projectingChat (chatProjection, chatUserDic, chatMessageDic, connectionSet)
            | HandleInitializeChatProjectionQry (_, connectionId, reply) ->
                let source = "HandleInitializeChatProjectionQry"
                sprintf "%s for %A when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source connectionId chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                if connectionId |> connectionSet.Contains |> not then // note: silently ignore already-known connectionId (e.g. re-initialization)
                    connectionId |> connectionSet.Add |> ignore
                sprintf "%s when projectingChat -> %i connection/s)" source connectionSet.Count |> Info |> log
                let result = chatProjection |> chatProjectionDto |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingChat (chatProjection, chatUserDic, chatMessageDic, connectionSet)
            | HandleSendChatMessageCmd (_, userId, chatMessageId, messageText, reply) ->
                let source = "HandleSendChatMessageCmd" 
                sprintf "%s (%A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i connection/s)" source userId chatMessageId chatUserDic.Count chatMessageDic.Count connectionSet.Count |> Info |> log
                let result =
                    if chatMessageId |> chatMessageDic.ContainsKey then ifDebug (sprintf "%A already exists" chatMessageId) UNEXPECTED_ERROR |> OtherError |> OtherAuthCmdError |> Error
                    else
                        if userId |> chatUserDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" userId) UNEXPECTED_ERROR |> OtherError |> OtherAuthCmdError |> Error
                        else (chatMessageId, { UserId = userId ; MessageText = messageText ; Timestamp = DateTimeOffset.UtcNow }) |> chatMessageDic.Add |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                let projection = chatProjection |> Some |> updateProjection source chatUserDic chatMessageDic connectionSet
                return! projectingChat (projection, chatUserDic, chatMessageDic, connectionSet) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Chat |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | Tick (ticks, secondsPerTick) -> if (ticks, secondsPerTick) |> isEveryNSeconds (int (HOUSEKEEPING_INTERVAL |> minutesToSeconds) * 1<second>) then Housekeeping |> agent.Post
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | UserEventWritten (_, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, userType) -> (userId, userName, userType) |> OnUserCreated |> agent.Post
                | UserTypeChanged (userId, userType) -> (userId, userType) |> OnUserTypeChanged |> agent.Post
                | _ -> ()
            | UserSignedIn userId -> (userId, true) |> OnUserSignedInOrOut |> agent.Post
            | UserSignedOut userId -> (userId, false) |> OnUserSignedInOrOut |> agent.Post
            | UserActivity userId -> userId |> OnUserActivity |> agent.Post
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
