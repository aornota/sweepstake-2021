module Aornota.Sweepstake2018.UI.Pages.Chat.State

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open System

open Elmish

let initialize authUser isCurrentPage : State * Cmd<Input> =
    let state = {
        AuthUser = authUser
        ProjectionState = Initializing
        IsCurrentPage = isCurrentPage
        UnseenCount = 0 }
    let cmd = InitializeChatProjectionQry |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
    state, cmd

let defaultNewChatMessageState () = { NewChatMessageId = ChatMessageId.Create () ; NewMessageText = String.Empty ; NewMessageErrorText = None ; SendChatMessageStatus = None }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private chatUser (chatUserDto:ChatUserDto) = { UserName = chatUserDto.UserName ; LastActivity = chatUserDto.LastActivity }

let private chatMessage (chatMessageDto:ChatMessageDto) =
    { UserId = chatMessageDto.UserId ; MessageText = chatMessageDto.MessageText ; Timestamp = chatMessageDto.Timestamp ; Expired = false }

let private chatProjection (chatProjectionDto:ChatProjectionDto) =
    let chatUserDic = ChatUserDic ()
    chatProjectionDto.ChatUserDtos |> List.iter (fun chatUserDto ->
        if chatUserDto.UserId |> chatUserDic.ContainsKey |> not then // note: silently ignore duplicate UserIds (should never happer)
            (chatUserDto.UserId, chatUserDto |> chatUser) |> chatUserDic.Add)
    let chatMessageDic = ChatMessageDic ()
    chatProjectionDto.ChatMessageDtos |> List.iter (fun chatMessageDto ->
        if chatMessageDto.ChatMessageId |> chatMessageDic.ContainsKey |> not then // note: silently ignore duplicate ChatMessageIds (should never happer)
            (chatMessageDto.ChatMessageId, chatMessageDto |> chatMessage) |> chatMessageDic.Add)
    { Rvn = initialRvn ; ChatUserDic = chatUserDic ; ChatMessageDic = chatMessageDic }

let private applyChatUsersDelta currentRvn deltaRvn (delta:Delta<UserId, ChatUserDto>) (chatUserDic:ChatUserDic) =
    let chatUserDic = ChatUserDic chatUserDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (userId, chatUserDto) -> if userId |> chatUserDic.ContainsKey then (userId, chatUserDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (userId, chatUserDto) -> (userId, chatUserDto |> chatUser) |> chatUserDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (userId, chatUserDto) -> if userId |> chatUserDic.ContainsKey |> not then (userId, chatUserDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (userId, chatUserDto) -> chatUserDic.[userId] <- (chatUserDto |> chatUser)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun userId -> if userId |> chatUserDic.ContainsKey |> not then userId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (chatUserDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> chatUserDic |> Ok)

let private applyChatMessagesDelta currentRvn deltaRvn (delta:Delta<ChatMessageId, ChatMessageDto>) (chatMessageDic:ChatMessageDic) =
    let chatMessageDic = ChatMessageDic chatMessageDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (chatMessageId, chatMessageDto) -> if chatMessageId |> chatMessageDic.ContainsKey then (chatMessageId, chatMessageDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (chatMessageId, chatMessageDto) -> (chatMessageId, chatMessageDto |> chatMessage) |> chatMessageDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (chatMessageId, chatMessageDto) -> if chatMessageId |> chatMessageDic.ContainsKey |> not then (chatMessageId, chatMessageDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (chatMessageId, chatMessageDto) -> chatMessageDic.[chatMessageId] <- (chatMessageDto |> chatMessage)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun chatMessageId -> if chatMessageId |> chatMessageDic.ContainsKey |> not then chatMessageId |> Some else None)
        // Note: delta.Removed correspond to "expired" (i.e. no longer cached on server) - but marked as such on client, rather than removed.
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fun chatMessageId ->
            let chatMessage = chatMessageDic.[chatMessageId]
            chatMessageDic.[chatMessageId] <- { chatMessage with Expired = true }) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> chatMessageDic |> Ok)

let private handleServerChatMsg serverChatMsg state : State * Cmd<Input> =
    let cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
    let qryErrorText error = match error with | AuthQryJwtError _ | AuthQryAuthznError _ -> UNEXPECTED_ERROR | OtherAuthQryError (OtherError errorText) -> errorText
    match serverChatMsg, state.ProjectionState with
    | InitializeChatProjectionQryResult (Ok (chatProjectionDto, hasMoreChatMessages)), Initializing ->
        let activeState = { 
            ChatProjection = chatProjectionDto |> chatProjection
            HasMoreChatMessages = hasMoreChatMessages
            MoreChatMessagesPending = false
            NewChatMessageState = defaultNewChatMessageState () }
        let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else chatProjectionDto.ChatMessageDtos.Length
        { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
    | InitializeChatProjectionQryResult (Error error), Initializing ->
        { state with ProjectionState = InitializationFailed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | MoreChatMessagesQryResult (Ok (rvn, chatMessageDtos, hasMoreChatMessages)), Active activeState ->
        if activeState.MoreChatMessagesPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else if chatMessageDtos.Length = 0 then
            let activeState = { activeState with HasMoreChatMessages = hasMoreChatMessages ; MoreChatMessagesPending = false }
            { state with ProjectionState = Active activeState }, "Unable to retrieve more chat messages<br><br>They have probably expired" |> warningToastCmd
        else
            let chatProjection = activeState.ChatProjection
            let addedChatMessageDtos = chatMessageDtos |> List.map (fun chatMessageDto -> chatMessageDto.ChatMessageId, chatMessageDto)
            let chatMessageDtoDelta = { Added = addedChatMessageDtos ; Changed = [] ; Removed = [] }
            match chatProjection.ChatMessageDic |> applyChatMessagesDelta chatProjection.Rvn rvn chatMessageDtoDelta with
            | Ok chatMessageDic ->
                let chatProjection = { chatProjection with Rvn = rvn ; ChatMessageDic = chatMessageDic }
                let activeState = { activeState with ChatProjection = chatProjection ; HasMoreChatMessages = hasMoreChatMessages ; MoreChatMessagesPending = false }
                let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else chatMessageDtos.Length // note: probably superfluous since will usually be current page
                { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatMessageDtoDelta chatProjection.ChatMessageDic error)
                let state, cmd = initialize state.AuthUser state.IsCurrentPage
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | MoreChatMessagesQryResult (Error error), Active activeState ->
        if activeState.MoreChatMessagesPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else
            let activeState = { activeState with MoreChatMessagesPending = false }
            { state with ProjectionState = Active activeState }, shouldNeverHappenCmd (sprintf "MoreChatMessagesQryResult Error %A" error)
    | SendChatMessageCmdResult result, Active activeState ->
        let newChatMessageState = activeState.NewChatMessageState
        match newChatMessageState.SendChatMessageStatus with
        | Some SendChatMessagePending ->
            match result with
            | Ok _ ->
                let activeState = { activeState with NewChatMessageState = defaultNewChatMessageState () }
                { state with ProjectionState = Active activeState }, "Chat message has been sent" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "SendChatMessageCmdResult error -> %A" error) (error |> cmdErrorText)
                let newChatMessageState = { newChatMessageState with SendChatMessageStatus = errorText |> SendChatMessageFailed |> Some }
                let activeState = { activeState with NewChatMessageState = newChatMessageState }
                { state with ProjectionState = Active activeState }, "Unable to send chat message" |> errorToastCmd
        | Some (SendChatMessageFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected SendChatMessageCmdResult when SendChatMessageStatus is not SendChatMessagePending -> %A" result)    
    | ChatProjectionMsg (ChatUsersDeltaMsg (deltaRvn, chatUserDtoDelta)), Active activeState ->
        let chatProjection = activeState.ChatProjection
        match chatProjection.ChatUserDic |> applyChatUsersDelta chatProjection.Rvn deltaRvn chatUserDtoDelta with
        | Ok chatUserDic ->
            let chatProjection = { chatProjection with Rvn = deltaRvn ; ChatUserDic = chatUserDic }
            let activeState = { activeState with ChatProjection = chatProjection }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatUserDtoDelta chatProjection.ChatUserDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | ChatProjectionMsg (ChatMessagesDeltaMsg (deltaRvn, chatMessageDtoDelta, hasMoreChatMessages)), Active activeState ->
        let chatProjection = activeState.ChatProjection
        match chatProjection.ChatMessageDic |> applyChatMessagesDelta chatProjection.Rvn deltaRvn chatMessageDtoDelta with
        | Ok chatMessageDic ->
            let chatProjection = { chatProjection with Rvn = deltaRvn ; ChatMessageDic = chatMessageDic }
            let activeState = { activeState with ChatProjection = chatProjection ; HasMoreChatMessages = hasMoreChatMessages }
            let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else chatMessageDtoDelta.Added.Length
            { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatMessageDtoDelta chatProjection.ChatMessageDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | ChatProjectionMsg (UserSignedInMsg (UserName userName)), Active _ ->
        state, if UserName userName = state.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed in" userName |> infoToastCmd
    | ChatProjectionMsg (UserSignedOutMsg (UserName userName)), Active _ ->
        state, if UserName userName = state.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed out" userName |> infoToastCmd
    | ChatProjectionMsg _, _ -> // note: silently ignore ChatProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerChatMsg when %A -> %A" state.ProjectionState serverChatMsg)

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ShowMarkdownSyntaxModal, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerChatMsg serverChatMsg, _ ->
            let state, cmd = state |> handleServerChatMsg serverChatMsg
            state, cmd, false
        | ToggleChatIsCurrentPage isCurrentPage, _ ->
            { state with IsCurrentPage = isCurrentPage ; UnseenCount = if isCurrentPage then 0 else state.UnseenCount }, Cmd.none, false
        | DismissChatMessage chatMessageId, Active activeState -> // note: silently ignore unknown chatMessageId (should never happen)
            if chatMessageId |> activeState.ChatProjection.ChatMessageDic.ContainsKey then chatMessageId |> activeState.ChatProjection.ChatMessageDic.Remove |> ignore
            state, Cmd.none, true
        | NewMessageTextChanged newMessageText, Active activeState ->
            let newChatMessageState = { activeState.NewChatMessageState with NewMessageText = newMessageText ; NewMessageErrorText = Markdown newMessageText |> validateChatMessageText }
            let activeState = { activeState with NewChatMessageState = newChatMessageState }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | MoreChatMessages, Active activeState -> // note: assume no need to validate HasMoreChatMessages (i.e. because Chat.Render.render will ensure that MoreChatMessages can only be dispatched when true)
            let activeState = { activeState with MoreChatMessagesPending = true }
            let cmd = MoreChatMessagesQry |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ProjectionState = Active activeState }, cmd, false
        | SendChatMessage, Active activeState -> // note: assume no need to validate NewMessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
            let newChatMessageState = activeState.NewChatMessageState
            let newChatMessageId, newMessageText = newChatMessageState.NewChatMessageId, Markdown (newChatMessageState.NewMessageText.Trim ())
            let newChatMessageState = { newChatMessageState with SendChatMessageStatus = SendChatMessagePending |> Some }
            let cmd = (newChatMessageId, newMessageText) |> SendChatMessageCmd |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
            let activeState = { activeState with NewChatMessageState = newChatMessageState }
            { state with ProjectionState = Active activeState }, cmd, false
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
