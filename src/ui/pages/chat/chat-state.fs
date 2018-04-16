module Aornota.Sweepstake2018.UI.Pages.Chat.State

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages.Chat.Common
open Aornota.Sweepstake2018.UI.Shared

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

let private defaultNewChatMessage () = { NewChatMessageId = ChatMessageId.Create () ; MessageText = String.Empty ; ErrorText = None }

let initialize authenticatedUser isCurrentPage : State * Cmd<Input> =
    let state = {
        AuthenticatedUser = authenticatedUser
        IsCurrentPage = isCurrentPage
        UnseenCount = 0
        ChatMessageUis = initialChatMessageUis
        NewChatMessage = defaultNewChatMessage () }
    state, Cmd.none

let private handleServerChatWsApi serverChatWsApi state : State * Cmd<Input> =
    match serverChatWsApi with
    | SendChatMessageResultWs (Ok chatMessage) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessage.ChatMessageId -> { chatMessageUi with ChatMessageType = Received }
                | _ -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | SendChatMessageResultWs (Error (chatMessageId, errorText)) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessageId -> { chatMessageUi with ChatMessageType = SendFailed errorText }
                | _ -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, errorToastCmd "Unable to send chat message"
    | OtherUserChatMessageWs chatMessage ->
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Received ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        { state with ChatMessageUis = chatMessageUis ; UnseenCount = state.UnseenCount + match state.IsCurrentPage with | true -> 0 | false -> 1 }, Cmd.none

let private handleSharedInput sharedInput state =
    match sharedInput with
    | ReceiveServerWsApi (ServerChatWsApi serverChatWsApi) -> handleServerChatWsApi serverChatWsApi state
    | _ -> state, Cmd.none // note: all other SharedInput expected to be handled by App.State.transition

let transition input state =
    match input with
    | SharedInput sharedInput -> handleSharedInput sharedInput state
    | ToggleChatIsCurrentPage isCurrentPage -> { state with IsCurrentPage = isCurrentPage ; UnseenCount = if isCurrentPage then 0 else state.UnseenCount }, Cmd.none
    | DismissChatMessage chatMessageId -> // note: silently ignore unknown chatMessageId
        let chatMessageUis = state.ChatMessageUis |> List.filter (fun chatMessageUi -> chatMessageUi.ChatMessage.ChatMessageId <> chatMessageId)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | MessageTextChanged messageText ->
        let newChatMessage = { state.NewChatMessage with MessageText = messageText ; ErrorText = validateChatMessageText messageText }
        { state with NewChatMessage = newChatMessage }, Cmd.none
    | SendChatMessage -> // note: assume no need to validate state.NewChatMessage.MessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
        let chatMessage = { ChatMessageId = state.NewChatMessage.NewChatMessageId ; UserName = state.AuthenticatedUser.UserName ; MessageText = state.NewChatMessage.MessageText }
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Sent ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        let cmd = SendAuthenticatedWsApi (state.AuthenticatedUser, SendChatMessageWs chatMessage) |> SharedInput |> Cmd.ofMsg
        { state with ChatMessageUis = chatMessageUis ; NewChatMessage = defaultNewChatMessage () }, cmd
