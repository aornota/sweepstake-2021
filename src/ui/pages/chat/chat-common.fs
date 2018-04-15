module Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Shared

open System

type Input =
    | SharedInput of sharedInput : SharedInput
    | DismissChatMessage of chatMessageId : ChatMessageId
    | MessageTextChanged of messageText : string
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
    MessageText : string
    ErrorText : string option }

type State = {
    AuthenticatedUser : AuthenticatedUser
    ChatMessageUis : ChatMessageUi list
    NewChatMessage : NewChatMessage }

let [<Literal>] private MAX_CHAT_MESSAGE_LENGTH = 100

let validateChatMessageText (messageText:string) =
    if String.IsNullOrWhiteSpace messageText then Some "Chat message must not be blank"
    else if messageText.Length > MAX_CHAT_MESSAGE_LENGTH then Some "Chat message is too long"
    else None
