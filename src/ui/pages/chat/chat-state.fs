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

let private handleServerChatWsApi serverChatWsApi chatState : ChatState * Cmd<ChatInput> =
    match serverChatWsApi with
    | SendChatMessageResultWs (Ok chatMessage) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            chatState.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessage.ChatMessageId -> { chatMessageUi with ChatMessageType = Received }
                | _ -> chatMessageUi)
        { chatState with ChatMessageUis = chatMessageUis }, Cmd.none
    | SendChatMessageResultWs (Error (chatMessageId, errorText)) -> // TODO-NMB-LOW: AddDebugMessage if no corresponding Sent message?...
        let chatMessageUis =
            chatState.ChatMessageUis
            |> List.map (fun chatMessageUi ->
                match chatMessageUi.ChatMessageType with
                | Sent when chatMessageUi.ChatMessage.ChatMessageId = chatMessageId -> { chatMessageUi with ChatMessageType = SendFailed errorText }
                | _ -> chatMessageUi)
        { chatState with ChatMessageUis = chatMessageUis }, errorToastCmd "Unable to send chat message"
    | OtherUserChatMessageWs chatMessage ->
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Received ; Timestamp = DateTime.Now } :: chatState.ChatMessageUis
        { chatState with ChatMessageUis = chatMessageUis }, Cmd.none

let private handleSharedInput sharedInput chatState =
    match sharedInput with
    | ReceiveServerWsApi (ServerChatWsApi serverChatWsApi) -> handleServerChatWsApi serverChatWsApi chatState
    | _ -> chatState, Cmd.none // note: all other SharedInput expected to be handled by App.State.transition

let transition chatInput chatState =
    match chatInput with
    | SharedInput sharedInput -> handleSharedInput sharedInput chatState
    | DismissChatMessage chatMessageId -> // note: silently ignore unknown chatMessageId
        let chatMessageUis = chatState.ChatMessageUis |> List.filter (fun chatMessageUi -> chatMessageUi.ChatMessage.ChatMessageId <> chatMessageId)
        { chatState with ChatMessageUis = chatMessageUis }, Cmd.none
    | MessageTextChanged messageText ->
        let newChatMessage = { chatState.NewChatMessage with MessageText = messageText ; ErrorText = validateChatMessageText messageText }
        { chatState with NewChatMessage = newChatMessage }, Cmd.none
    | SendChatMessage -> // note: assume no need to validate state.NewChatMessage.MessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
        let chatMessage = { ChatMessageId = chatState.NewChatMessage.NewChatMessageId ; UserName = chatState.AuthenticatedUser.UserName ; MessageText = chatState.NewChatMessage.MessageText }
        let chatMessageUis = { ChatMessage = chatMessage ; ChatMessageType = Sent ; Timestamp = DateTime.Now } :: chatState.ChatMessageUis
        let cmd = SendAuthenticatedWsApi (chatState.AuthenticatedUser, SendChatMessageWs chatMessage) |> SharedInput |> Cmd.ofMsg
        { chatState with ChatMessageUis = chatMessageUis ; NewChatMessage = defaultNewChatMessage () }, cmd
