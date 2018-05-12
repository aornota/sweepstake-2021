module Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System

type Input =
    | ShowMarkdownSyntaxModal
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerChatMsg of serverChatMsg : ServerChatMsg
    | ToggleChatIsCurrentPage of isCurrentPage : bool
    | DismissChatMessage of chatMessageId : ChatMessageId
    | MessageTextChanged of messageText : Markdown
    | SendChatMessage

type ChatMessageType =
    | Sent
    | SendFailed of errorText : string
    | Received

type ChatMessageUi = {
    ChatMessage : ChatMessageOLD
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
    if String.IsNullOrWhiteSpace messageText then "Chat message must not be blank" |> Some
    else if messageText.Length > MAX_CHAT_MESSAGE_LENGTH then "Chat message is too long" |> Some
    else None
