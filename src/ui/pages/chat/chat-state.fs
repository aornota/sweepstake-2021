module Aornota.Sweepstake2018.UI.Pages.Chat.State

open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open System

open Elmish

let private initialChatMessageUis : ChatMessageUi list =
#if DEBUG
    // TEMP-NMB: Pre-populate some ChatMessages...
    (*[
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "neph" ; MessageText = "Hello?" } ; ChatMessageType = Sent; Timestamp = DateTime.Now }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "neph" ; MessageText = "Sorry\nReally" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddSeconds (-17.) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "neph" ; MessageText = "I am w*nking as I write this..." } ; ChatMessageType = SendFailed "Message 'I am w*nking as I write this...' is inappropriate" ; Timestamp = DateTime.Now.AddSeconds (-33.) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "bubbles" ; MessageText = "Lurking" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddSeconds (-45.) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "neph" ; MessageText = "Where's bubbles?" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddMinutes (-2.2) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "buttercup" ; MessageText = "Hi blossom!" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddMinutes (-3.) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "neph" ; MessageText = "Yo!" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddMinutes (-5.8) }
        { ChatMessage = { ChatMessageId = ChatMessageId.Create () ; UserName = "blossom" ; MessageText = "Hiya!" } ; ChatMessageType = Received; Timestamp = DateTime.Now.AddMinutes (-8.) }
    ]*)
    // ...or not...
    []
    // ...NMB-TEMP
#else
    []
#endif

let private defaultNewChatMessage () = { NewChatMessageId = ChatMessageId.Create () ; MessageText = Markdown String.Empty ; ErrorText = None }

let initialize authUser isCurrentPage : State * Cmd<Input> =
    let state = {
        AuthUser = authUser
        IsCurrentPage = isCurrentPage
        UnseenCount = 0
        ChatMessageUis = initialChatMessageUis
        NewChatMessage = defaultNewChatMessage () }
    state, Cmd.none

let private handleServerChatMsg serverChatMsg state : State * Cmd<Input> =
    match serverChatMsg with
    | SendChatMessageResultMsgOLD (Ok chatMessage) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessage.ChatMessageId -> { chatMessageUi with ChatMessageType = Received }
                | Sent _ | SendFailed _ | Received -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | SendChatMessageResultMsgOLD (Error (chatMessageId, errorText)) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessageId -> { chatMessageUi with ChatMessageType = SendFailed errorText }
                | Sent _ | SendFailed _ | Received -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, "Unable to send chat message" |> errorToastCmd
    | OtherUserChatMessageMsgOLD chatMessage ->
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Received ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        { state with ChatMessageUis = chatMessageUis ; UnseenCount = state.UnseenCount + match state.IsCurrentPage with | true -> 0 | false -> 1 }, Cmd.none

let transition input state =
    match input with
    | ShowMarkdownSyntaxModal -> state, Cmd.none // note: expected to be handled by Program.State.transition
    | SendUiAuthMsg _ -> state, Cmd.none // note: expected to be handled by Program.State.transition
    | ReceiveServerChatMsg serverChatMsg -> state |> handleServerChatMsg serverChatMsg
    | ToggleChatIsCurrentPage isCurrentPage -> { state with IsCurrentPage = isCurrentPage ; UnseenCount = if isCurrentPage then 0 else state.UnseenCount }, Cmd.none
    | DismissChatMessage chatMessageId -> // note: silently ignore unknown chatMessageId
        let chatMessageUis = state.ChatMessageUis |> List.filter (fun chatMessageUi -> chatMessageUi.ChatMessage.ChatMessageId <> chatMessageId)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | MessageTextChanged messageText ->
        let newChatMessage = { state.NewChatMessage with MessageText = messageText ; ErrorText = validateChatMessageText messageText }
        { state with NewChatMessage = newChatMessage }, Cmd.none
    | SendChatMessage -> // note: assume no need to validate state.NewChatMessage.MessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
        let (UserName userName) = state.AuthUser.UserName
        let chatMessage = { ChatMessageId = state.NewChatMessage.NewChatMessageId ; UserName = userName ; MessageText = state.NewChatMessage.MessageText }
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Sent ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        let cmd = chatMessage |> SendChatMessageCmd |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ChatMessageUis = chatMessageUis ; NewChatMessage = defaultNewChatMessage () }, cmd
