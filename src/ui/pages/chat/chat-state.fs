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

let initialize authenticatedUser : ChatState * Cmd<ChatInput> =
    let state = {
        AuthenticatedUser = authenticatedUser
        ChatMessageUis = initialChatMessageUis
        NewChatMessage = defaultNewChatMessage () }
    state, Cmd.none

let transition input state =
    let unchanged = state, Cmd.none
    match input with
    | SharedInput (ReceiveServerWsApi (ServerChatWsApi (SendChatMessageResultWs (Ok chatMessage)))) ->
        // TODO-NMB: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessage.ChatMessageId -> { chatMessageUi with ChatMessageType = Received }
                | _ -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | SharedInput (ReceiveServerWsApi (ServerChatWsApi (SendChatMessageResultWs (Error (chatMessageId, errorText))))) ->
        // TODO-NMB: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            state.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessageId -> { chatMessageUi with ChatMessageType = SendFailed errorText }
                | _ -> chatMessageUi)
        { state with ChatMessageUis = chatMessageUis }, errorToastCmd "Unable to send chat message"
    | SharedInput (ReceiveServerWsApi (ServerChatWsApi (OtherUserChatMessageWs chatMessage))) ->
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Received ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | SharedInput _ -> unchanged // note: all other SharedInput expected to be handled by App.State.transition
    | DismissChatMessage chatMessageId -> // note: silently ignore unknown chatMessageId
        let chatMessageUis = state.ChatMessageUis |> List.filter (fun chatMessageUi -> chatMessageUi.ChatMessage.ChatMessageId <> chatMessageId)
        { state with ChatMessageUis = chatMessageUis }, Cmd.none
    | MessageTextChanged messageText ->
        let newChatMessage = { state.NewChatMessage with MessageText = messageText ; ErrorText = validateChatMessageText messageText }
        { state with NewChatMessage = newChatMessage }, Cmd.none
    | SendChatMessage -> // note: assume no need to validate state.NewChatMessage.MessageText
        let chatMessage = { ChatMessageId = state.NewChatMessage.NewChatMessageId ; UserName = state.AuthenticatedUser.UserName ; MessageText = state.NewChatMessage.MessageText }
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Sent ; Timestamp = DateTime.Now } :: state.ChatMessageUis
        let cmd = SendAuthenticatedWsApi (state.AuthenticatedUser, SendChatMessageWs chatMessage) |> SharedInput |> Cmd.ofMsg
        { state with ChatMessageUis = chatMessageUis ; NewChatMessage = defaultNewChatMessage () }, cmd
