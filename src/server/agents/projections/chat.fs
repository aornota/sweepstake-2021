module Aornota.Sweepstake2018.Server.Agents.Projections.Chat

(* Broadcasts: SendMsg
   Subscribes: Tick
               UsersRead
               UserEventWritten (UserCreated | UserTypeChanged)
               UserSignedIn | UserActivity | UserSignedOut
               ConnectionsSignedOut | Disconnected *)

open Aornota.Common.Delta
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
    | HandleInitializeChatProjectionQry of token : ChatProjectionQryToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<ChatProjectionDto * bool, AuthQryError<string>>>
    | HandleMoreChatMessagesQry of token : ChatProjectionQryToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<Rvn * ChatMessageDto list * bool, AuthQryError<string>>>
    | HandleSendChatMessageCmd of token : SendChatMessageToken * userId : UserId * chatMessageId : ChatMessageId * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    
type private ChatUser = { UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }
type private ChatUserDic = Dictionary<UserId, ChatUser>

type private ChatMessage = { Ordinal : int ; UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset }
type private ChatMessageDic = Dictionary<ChatMessageId, ChatMessage>

type private Projectee = { LastRvn : Rvn ; MinChatMessageOrdinal : int option ; LastHasMoreChatMessages : bool }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { ChatUserDic : ChatUserDic ; ChatMessageDic : ChatMessageDic }

type private StateChangeType =
    | Initialization of chatUserDic : ChatUserDic * chatMessageDic : ChatMessageDic
    | ChatUserChange of chatUserDic : ChatUserDic * state : State
    | ChatMessageChange of chatMessageDic : ChatMessageDic * state : State

type private ChatUserDtoDic = Dictionary<UserId, ChatUserDto>

let [<Literal>] private HOUSEKEEPING_INTERVAL = 1.<minute>
let [<Literal>] private CHAT_MESSAGE_EXPIRES_AFTER = 24.<hour>
let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>
let [<Literal>] private CHAT_MESSAGE_BATCH_SIZE = 10

let private log category = (Projection Chat, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private chatUserDto (userId, chatUser:ChatUser) =
    match chatUser.UserType with
    | SuperUser | Administrator | Pleb -> { UserId = userId ; UserName = chatUser.UserName ; LastActivity = chatUser.LastActivity } |> Some
    | PersonaNonGrata -> None

let private chatUserDtoDic (chatUserDic:ChatUserDic) =
    let chatUserDtoDic = ChatUserDtoDic ()
    chatUserDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, chatUser)) ->
        match (userId, chatUser) |> chatUserDto with | Some chatUserDto -> (chatUserDto.UserId, chatUserDto) |> chatUserDtoDic.Add | None -> ())
    chatUserDtoDic

let private chatMessageDto (chatMessageId, chatMessage:ChatMessage) =
    { ChatMessageId = chatMessageId ; UserId = chatMessage.UserId ; MessageText = chatMessage.MessageText ; Timestamp = chatMessage.Timestamp }

let private chatProjectionDto state =
    let chatUserDtos = state.ChatUserDic |> List.ofSeq |> List.choose (fun (KeyValue (userId, chatUser)) -> (userId, chatUser) |> chatUserDto)
    let chatMessageDtos = state.ChatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> (chatMessageId, chatMessage) |> chatMessageDto)
    { ChatUserDtos = chatUserDtos ; ChatMessageDtos = chatMessageDtos }

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendChatUserDtoDelta (projecteeDic:ProjecteeDic) chatUserDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendChatUserDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, chatUserDtoDelta) |> ChatUsersDeltaMsg |> ChatProjectionMsg |> ServerChatMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendChatMessageDelta removedOrdinals minChatMessageOrdinal (projecteeDic:ProjecteeDic) (chatMessageDelta:Delta<ChatMessageId, ChatMessage>) =
    let isRelevant projecteeMinChatMessageOrdinal ordinal =
        match projecteeMinChatMessageOrdinal with
        | Some projecteeMinChatMessageOrdinal when projecteeMinChatMessageOrdinal > ordinal -> false
        | Some _ | None -> true
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let hasMoreChatMessages =
            match projectee.MinChatMessageOrdinal, minChatMessageOrdinal with
            | Some projecteeMinChatMessageOrdinal, Some minChatMessageOrdinal when projecteeMinChatMessageOrdinal > minChatMessageOrdinal -> true
            | Some _, Some _ | Some _, None | None, Some _ | None, None -> false
        let addedChatMessageDtos =
            chatMessageDelta.Added // note: no need to filter based on projectee.MinChatMessageOrdinal
            |> List.map (fun (chatMessageId, chatMessage) -> chatMessageId, (chatMessageId, chatMessage) |> chatMessageDto)
        let changedChatMessageDtos =
            chatMessageDelta.Changed
            |> List.filter (fun (_, chatMessage) -> chatMessage.Ordinal |> isRelevant projectee.MinChatMessageOrdinal)
            |> List.map (fun (chatMessageId, chatMessage) -> chatMessageId, (chatMessageId, chatMessage) |> chatMessageDto)
        let removedChatMessageIds =
            chatMessageDelta.Removed
            |> List.choose (fun chatMessageId ->
                match removedOrdinals |> List.tryFind (fun (removedChatMessageId, _) -> removedChatMessageId = chatMessageId) with
                | Some (_, ordinal) -> (chatMessageId, ordinal) |> Some
                | None -> None) // note: should never happen
            |> List.filter (fun (_, ordinal) -> ordinal |> isRelevant projectee.MinChatMessageOrdinal)
            |> List.map fst
        let chatMessageDtoDelta = { Added = addedChatMessageDtos ; Changed = changedChatMessageDtos ; Removed = removedChatMessageIds }
        if chatMessageDtoDelta |> isEmpty |> not || hasMoreChatMessages <> projectee.LastHasMoreChatMessages then
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; LastHasMoreChatMessages = hasMoreChatMessages }
            sprintf "sendChatMessageDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, chatMessageDtoDelta, hasMoreChatMessages) |> ChatMessagesDeltaMsg |> ChatProjectionMsg |> ServerChatMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization (chatUserDic, chatMessageDic) ->
            sprintf "%s -> initialized" source |> Info |> log
            { ChatUserDic = ChatUserDic chatUserDic ; ChatMessageDic = ChatMessageDic chatMessageDic }
        | ChatUserChange (chatUserDic, state) ->
            let previousChatUserDtoDic = state.ChatUserDic |> chatUserDtoDic
            let chatUserDtoDic = chatUserDic |> chatUserDtoDic
            let chatUserDtoDelta = chatUserDtoDic |> delta previousChatUserDtoDic
            if chatUserDtoDelta |> isEmpty |> not then
                sprintf "%s -> ChatUserDto delta %A -> %i projectee/s" source chatUserDtoDelta projecteeDic.Count |> Info |> log
                chatUserDtoDelta |> sendChatUserDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with ChatUserDic = ChatUserDic chatUserDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
        | ChatMessageChange (chatMessageDic, state) ->
            let chatMessageDelta = chatMessageDic |> delta state.ChatMessageDic
            if chatMessageDelta |> isEmpty |> not then
                let removedOrdinals = chatMessageDelta.Removed |> List.choose (fun chatMessageId ->
                    if chatMessageId |> state.ChatMessageDic.ContainsKey |> not then None // note: ignore unknown ChatMessageId (should never happen)
                    else
                        let chatMessage = state.ChatMessageDic.[chatMessageId]
                        (chatMessageId, chatMessage.Ordinal) |> Some)
                let minChatMessageOrdinal =
                    if chatMessageDic.Count > 0 then chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.min |> Some
                    else None
                sprintf "%s -> ChatMessage delta %A -> %i (potential) projectee/s" source chatMessageDelta projecteeDic.Count |> Info |> log
                chatMessageDelta |> sendChatMessageDelta removedOrdinals minChatMessageOrdinal projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with ChatMessageDic = ChatMessageDic chatMessageDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type Chat () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersRead (0 chat users) (0 chat messages) (0 projectees)" |> Info |> log
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
            | HandleMoreChatMessagesQry _ -> "HandleMoreChatMessagesQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
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
                let projecteeDic = ProjecteeDic ()
                let state = (chatUserDic, chatMessageDic) |> Initialization |> updateState source projecteeDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUserCreated _ -> "OnUserCreated when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserActivity _ -> "OnUserActivity when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleInitializeChatProjectionQry _ -> "HandleInitializeChatProjectionQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleMoreChatMessagesQry _ -> "HandleMoreChatMessagesQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleSendChatMessageCmd _ -> "HandleSendChatMessageCmd when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead () }
        and projectingChat (state, chatUserDic, chatMessageDic, projecteeDic) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingChat" |> IgnoredInput |> Agent |> log ; return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | Housekeeping ->
                let source = "Housekeeping"
                sprintf "%s when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let expirationCutoff = cutoff (int (CHAT_MESSAGE_EXPIRES_AFTER |> hoursToSeconds) * 1<second>)
                let updatedChatMessageDic = ChatMessageDic ()
                chatMessageDic |> List.ofSeq |> List.iter (fun (KeyValue (chatMessageId, chatMessage)) ->
                    if chatMessage.Timestamp > expirationCutoff then (chatMessageId, chatMessage) |> updatedChatMessageDic.Add)
                let state, chatMessageDic =
                    if updatedChatMessageDic.Count = chatMessageDic.Count then state, chatMessageDic
                    else
                        let state = (updatedChatMessageDic, state) |> ChatMessageChange |> updateState source projecteeDic
                        state, updatedChatMessageDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUsersRead _ -> "OnUsersRead when projectingChat" |> IgnoredInput |> Agent |> log ; return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUserCreated (userId, userName, userType) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source userId userName userType chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                if userId |> chatUserDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, { UserName = userName ; UserType = userType ; LastActivity = None }) |> chatUserDic.Add
                sprintf "%s when projectingChat -> %i chat user/s)" source chatUserDic.Count |> Info |> log
                let state = (chatUserDic, state) |> ChatUserChange |> updateState source projecteeDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUserTypeChanged (userId, userType) ->
                let source = "OnUserTypeChanged"
                sprintf "%s (%A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source userId userType chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let state, updatedChatMessageDic =
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
                        let state = (chatUserDic, state) |> ChatUserChange |> updateState source projecteeDic
                        state, updatedChatMessageDic
                    else state, chatMessageDic
                let state, chatMessageDic =
                    if updatedChatMessageDic.Count = chatMessageDic.Count then state, chatMessageDic
                    else
                        let state = (updatedChatMessageDic, state) |> ChatMessageChange |> updateState source projecteeDic
                        state, updatedChatMessageDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUserSignedInOrOut (userId, signedIn) ->
                let source = "OnUserSignedInOrOut"
                sprintf "%s (%A %b) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source userId signedIn chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                if userId |> chatUserDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let chatUser = chatUserDic.[userId]
                    let connectionIds = projecteeDic.Keys |> List.ofSeq
                    chatUser.UserName |> (if signedIn then UserSignedInMsg else UserSignedOutMsg) |> ChatProjectionMsg |> ServerChatMsg |> sendMsg connectionIds
                    chatUserDic.[userId] <- { chatUser with LastActivity = match signedIn with | true -> DateTimeOffset.UtcNow |> Some | false -> None }
                let state = (chatUserDic, state) |> ChatUserChange |> updateState source projecteeDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | OnUserActivity userId ->
                let source = "OnUserActivity"
                sprintf "%s (%A) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source userId chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let updated =
                    if userId |> chatUserDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                        let chatUser = chatUserDic.[userId]
                        let now = DateTimeOffset.UtcNow
                        let throttled = match chatUser.LastActivity with | Some lastActivity -> (now - lastActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE | None -> false
                        if throttled |> not then chatUserDic.[userId] <- { chatUser with LastActivity = now |> Some }
                        else sprintf "%s throttled for %A" source userId |> Info |> log
                        throttled |> not
                    else false
                let state = if updated then (chatUserDic, state) |> ChatUserChange |> updateState source projecteeDic else state
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source connectionIds chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | HandleInitializeChatProjectionQry (_, connectionId, reply) ->
                let source = "HandleInitializeChatProjectionQry"
                sprintf "%s for %A when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source connectionId chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let initializedState, minChatMessageOrdinal, hasMoreChatMessages =
                    if chatMessageDic.Count <= CHAT_MESSAGE_BATCH_SIZE then
                        let minChatMessageOrdinal =
                            if chatMessageDic.Count > 0 then chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.min |> Some
                            else None
                        state, minChatMessageOrdinal, false
                    else
                        let initialChatMessages =
                            chatMessageDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> chatMessageId, chatMessage)
                            |> List.sortBy (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.rev |> List.take CHAT_MESSAGE_BATCH_SIZE
                        let minChatMessageOrdinal = initialChatMessages |> List.map (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.min |> Some
                        let initialChatMessageDic = ChatMessageDic ()
                        initialChatMessages |> List.iter (fun (chatMessageId, chatMessage) -> (chatMessageId, chatMessage) |> initialChatMessageDic.Add)
                        { state with ChatMessageDic = initialChatMessageDic }, minChatMessageOrdinal, true
                let projectee = { LastRvn = initialRvn ; MinChatMessageOrdinal = minChatMessageOrdinal ; LastHasMoreChatMessages = hasMoreChatMessages }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = (initializedState |> chatProjectionDto, hasMoreChatMessages) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | HandleMoreChatMessagesQry (_, connectionId, reply) ->
                let source = "HandleMoreChatMessagesQry"
                sprintf "%s for %A when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source connectionId chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if connectionId |> projecteeDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" connectionId) UNEXPECTED_ERROR |> OtherError |> OtherAuthQryError |> Error
                    else
                        let projectee = projecteeDic.[connectionId]
                        let moreChatMessages =
                            chatMessageDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> chatMessageId, chatMessage)
                            |> List.filter (fun (_, chatMessage) ->
                                match projectee.MinChatMessageOrdinal with
                                | Some minChatMessageOrdinal -> minChatMessageOrdinal > chatMessage.Ordinal
                                | None -> true) // note: should never happen                                   
                            |> List.sortBy (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.rev                       
                        let moreChatMessages, hasMoreChatMessages =
                            if moreChatMessages.Length <= CHAT_MESSAGE_BATCH_SIZE then moreChatMessages, false
                            else moreChatMessages |> List.take CHAT_MESSAGE_BATCH_SIZE, true
                        let minChatMessageOrdinal =
                            if moreChatMessages.Length > 0 then moreChatMessages |> List.map (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.min |> Some
                            else projectee.MinChatMessageOrdinal // note: should never happen
                        let chatMessageDtos = moreChatMessages |> List.map chatMessageDto
                        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; MinChatMessageOrdinal = minChatMessageOrdinal ; LastHasMoreChatMessages = hasMoreChatMessages }
                        projecteeDic.[connectionId] <- projectee
                        (projectee.LastRvn, chatMessageDtos, hasMoreChatMessages) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic)
            | HandleSendChatMessageCmd (_, userId, chatMessageId, messageText, reply) ->
                let source = "HandleSendChatMessageCmd" 
                sprintf "%s (%A %A) when projectingChat (%i chat user/s) (%i chat message/s) (%i projectee/s)" source userId chatMessageId chatUserDic.Count chatMessageDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if chatMessageId |> chatMessageDic.ContainsKey then ifDebug (sprintf "%A already exists" chatMessageId) UNEXPECTED_ERROR |> OtherError |> OtherAuthCmdError |> Error
                    else
                        if userId |> chatUserDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" userId) UNEXPECTED_ERROR |> OtherError |> OtherAuthCmdError |> Error
                        else
                            let nextOrdinal =
                                if chatMessageDic.Count = 0 then 1
                                else (chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.max) + 1
                            (chatMessageId, { Ordinal = nextOrdinal ; UserId = userId ; MessageText = messageText ; Timestamp = DateTimeOffset.UtcNow }) |> chatMessageDic.Add |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                let state = (chatMessageDic, state) |> ChatMessageChange |> updateState source projecteeDic
                return! projectingChat (state, chatUserDic, chatMessageDic, projecteeDic) }
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
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten (subset) | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeChatProjectionQry (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleInitializeChatProjectionQry) |> agent.PostAndAsyncReply
    member __.HandleMoreChatMessagesQry (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleMoreChatMessagesQry) |> agent.PostAndAsyncReply
    member __.HandleSendChatMessageCmd (token, userId, chatMessageId, messageText) =
        (fun reply -> (token, userId, chatMessageId, messageText, reply) |> HandleSendChatMessageCmd) |> agent.PostAndAsyncReply

let chat = Chat ()
