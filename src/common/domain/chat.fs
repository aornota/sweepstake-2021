module Aornota.Sweepstake2018.Common.Domain.Chat

open Aornota.Common.Markdown

open Aornota.Sweepstake2018.Common.Domain.User

open System

type ChatMessageId = | ChatMessageId of guid : Guid with
    static member Create () = Guid.NewGuid () |> ChatMessageId

type ChatMessageDto = { ChatMessageId : ChatMessageId ; UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset }

let [<Literal>] private MAX_CHAT_MESSAGE_LENGTH = 2000

let validateChatMessageText (Markdown messageText) =
    if String.IsNullOrWhiteSpace messageText then "Chat message must not be blank" |> Some
    else if (messageText.Trim ()).Length > MAX_CHAT_MESSAGE_LENGTH then "Chat message is too long" |> Some
    else None
