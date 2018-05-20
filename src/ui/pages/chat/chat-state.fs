module Aornota.Sweepstake2018.UI.Pages.Chat.State

open Aornota.Common.Delta
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
    state, InitializeChatProjectionQry |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg

let private defaultNewChatMessage () = { NewChatMessageId = ChatMessageId.Create () ; NewMessageText = String.Empty ; NewMessageErrorText = None }

let private shouldNeverHappen debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private chatProjection (chatProjectionDto:ChatProjectionDto) =
    let chatUserDic = ChatUserDic ()
    chatProjectionDto.ChatUserDtos |> List.iter (fun chatUserDto ->
        if chatUserDto.UserId |> chatUserDic.ContainsKey |> not then // note: silently ignore duplicate UserIds (should never happer)
            let chatUser = { UserName = chatUserDto.UserName ; LastApi = chatUserDto.LastApi }
            (chatUserDto.UserId, chatUser) |> chatUserDic.Add)
    let chatMessageDic = ChatMessageDic ()
    chatProjectionDto.ChatMessageDtos |> List.iter (fun chatMessageDto ->
        if chatMessageDto.ChatMessageId |> chatMessageDic.ContainsKey |> not then // note: silently ignore duplicate ChatMessageIds (should never happer)
            let chatMessage = { UserId = chatMessageDto.UserId ; MessageText = chatMessageDto.MessageText ; Timestamp = chatMessageDto.Timestamp ; Expired = false }
            (chatMessageDto.ChatMessageId, chatMessage) |> chatMessageDic.Add)
    { Rvn = chatProjectionDto.Rvn ; ChatUserDic = chatUserDic ; ChatMessageDic = chatMessageDic }

let private applyChatUserDelta currentRvn deltaRvn (delta:Delta<UserId, ChatUserDto>) (chatUserDic:ChatUserDic) =
    let chatUser (chatUserDto:ChatUserDto) = { UserName = chatUserDto.UserName ; LastApi = chatUserDto.LastApi }
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

let private applyChatMessageDelta currentRvn deltaRvn (delta:Delta<ChatMessageId, ChatMessageDto>) (chatMessageDic:ChatMessageDic) =
    let chatMessage (chatMessageDto:ChatMessageDto) =
        { UserId = chatMessageDto.UserId ; MessageText = chatMessageDto.MessageText ; Timestamp = chatMessageDto.Timestamp ; Expired = false }
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
    | InitializeChatProjectionQryResult (Ok chatProjectionDto), Initializing ->
        let activeState = { ChatProjection = chatProjectionDto |> chatProjection ; UnconfirmedChatMessageDic = ChatMessageDic () ; NewChatMessage = defaultNewChatMessage () }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeChatProjectionQryResult (Error error), Initializing ->
        { state with ProjectionState = InitializationFailed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | ChatProjectionMsg (ChatUsersDeltaMsg (deltaRvn, chatUserDtoDelta)), Active activeState ->
        let chatProjection = activeState.ChatProjection
        match chatProjection.ChatUserDic |> applyChatUserDelta chatProjection.Rvn deltaRvn chatUserDtoDelta with
        | Ok chatUserDic ->
            let chatProjection = { chatProjection with Rvn = deltaRvn ; ChatUserDic = chatUserDic }
            let activeState = { activeState with ChatProjection = chatProjection }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" chatUserDtoDelta chatProjection.ChatUserDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | ChatProjectionMsg (ChatMessagesDeltaMsg (deltaRvn, chatMessageDtoDelta)), Active activeState ->
        let chatProjection = activeState.ChatProjection
        match chatProjection.ChatMessageDic |> applyChatMessageDelta chatProjection.Rvn deltaRvn chatMessageDtoDelta with
        | Ok chatMessageDic ->
            let remove = activeState.UnconfirmedChatMessageDic |> List.ofSeq |> List.choose (fun (KeyValue (chatMessageId, _)) ->
                if chatMessageId |> chatMessageDic.ContainsKey then chatMessageId |> Some else None)
            remove |> List.iter (activeState.UnconfirmedChatMessageDic.Remove >> ignore)
            let chatProjection = { chatProjection with Rvn = deltaRvn ; ChatMessageDic = chatMessageDic }
            let activeState = { activeState with ChatProjection = chatProjection }
            let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else chatMessageDtoDelta.Added.Length
            { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" chatMessageDtoDelta chatProjection.ChatMessageDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | ChatProjectionMsg (UserSignedInMsg (UserName userName)), Active _ ->
        state, if UserName userName = state.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed in" userName |> infoToastCmd
    | ChatProjectionMsg (UserSignedOutMsg (UserName userName)), Active _ ->
        state, if UserName userName = state.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed out" userName |> infoToastCmd
    | ChatProjectionMsg _, _ -> state, Cmd.none // note: silently ignore ChatProjectionMsg if not Active
    | SendChatMessageCmdResult (Ok _), Active _ -> state, Cmd.none // note: nothing to do here
    | SendChatMessageCmdResult (Error (chatMessageId, error)), Active activeState -> // note: silently ignore unknown chatMessageId (should never happen)
        if chatMessageId |> activeState.UnconfirmedChatMessageDic.ContainsKey then chatMessageId |> activeState.UnconfirmedChatMessageDic.Remove |> ignore
        let cmd = error |> cmdErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
        state, Cmd.batch [ cmd ; "sending chat message" |> unexpectedErrorWhen |> errorToastCmd ]
    | _, _ -> state, shouldNeverHappen (sprintf "Unexpected ServerChatMsg when %A -> %A" state.ProjectionState serverChatMsg)

let transition input state =
    match input, state.ProjectionState with
    | AddNotificationMessage _, _ -> state, Cmd.none // note: expected to be handled by Program.State.transition 
    | ShowMarkdownSyntaxModal, _ -> state, Cmd.none // note: expected to be handled by Program.State.transition
    | SendUiAuthMsg _, _ -> state, Cmd.none // note: expected to be handled by Program.State.transition
    | ReceiveServerChatMsg serverChatMsg, _ -> state |> handleServerChatMsg serverChatMsg
    | ToggleChatIsCurrentPage isCurrentPage, _ -> { state with IsCurrentPage = isCurrentPage ; UnseenCount = if isCurrentPage then 0 else state.UnseenCount }, Cmd.none
    | DismissChatMessage chatMessageId, Active activeState -> // note: silently ignore unknown chatMessageId (should never happen)
        if chatMessageId |> activeState.ChatProjection.ChatMessageDic.ContainsKey then chatMessageId |> activeState.ChatProjection.ChatMessageDic.Remove |> ignore
        state, Cmd.none
    | NewMessageTextChanged newMessageText, Active activeState ->
        let newChatMessage = { activeState.NewChatMessage with NewMessageText = newMessageText ; NewMessageErrorText = Markdown newMessageText |> validateChatMessageText }
        let activeState = { activeState with NewChatMessage = newChatMessage }
        { state with ProjectionState = Active activeState }, Cmd.none
    | SendChatMessage, Active activeState -> // note: assume no need to validate NewMessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
        let newChatMessageId, newMessageText = activeState.NewChatMessage.NewChatMessageId, Markdown (activeState.NewChatMessage.NewMessageText.Trim ())
        if newChatMessageId |> activeState.UnconfirmedChatMessageDic.ContainsKey |> not then // note: silently ignore already-known newChatMessageId (should never happen)
            let newChatMessage = { UserId = state.AuthUser.UserId ; MessageText = newMessageText ; Timestamp = DateTimeOffset.UtcNow ; Expired = false }
            (newChatMessageId, newChatMessage) |> activeState.UnconfirmedChatMessageDic.Add
        let cmd = (newChatMessageId, newMessageText) |> SendChatMessageCmd |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
        let activeState = { activeState with NewChatMessage = defaultNewChatMessage () }
        { state with ProjectionState = Active activeState }, cmd
    | _, _ -> state, shouldNeverHappen (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input)
