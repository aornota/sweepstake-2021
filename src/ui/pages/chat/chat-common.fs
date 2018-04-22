module Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui

open System

type Input =
    | ShowMarkdownSyntaxModal
    | SendAuthWsApi of authUser : AuthUser * uiAuthWsApi : UiAuthWsApi
    | ReceiveServerChatWsApi of serverWsApi : ServerChatWsApi
    | ToggleChatIsCurrentPage of isCurrentPage : bool
    | DismissChatMessage of chatMessageId : ChatMessageId
    | MessageTextChanged of messageText : Markdown
    | SendChatMessage

type ChatMessageType =
    | Sent
    | SendFailed of errorText : string
    | Received

type ChatMessageUi = {
    ChatMessage : ChatMessage
    ChatMessageType : ChatMessageType
    Timestamp : DateTime }

type NewChatMessage = {
    NewChatMessageId : ChatMessageId
    MessageText : Markdown
    ErrorText : string option }

type State = {
    AuthUser : AuthUser
    IsCurrentPage : bool
    ChatMessageUis : ChatMessageUi list
    UnseenCount : int
    NewChatMessage : NewChatMessage }

let [<Literal>] private MAX_CHAT_MESSAGE_LENGTH = 2000

let validateChatMessageText (Markdown messageText) =
    if String.IsNullOrWhiteSpace messageText then Some "Chat message must not be blank"
    else if messageText.Length > MAX_CHAT_MESSAGE_LENGTH then Some "Chat message is too long"
    else None
