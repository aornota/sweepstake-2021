module Aornota.Sweepstake2018.Shared.Domain

open System

type Markdown = | Markdown of markdown : string

type UserId = | UserId of guid : Guid with static member Create () = Guid.NewGuid () |> UserId
type SessionId = | SessionId of guid : Guid with static member Create () = Guid.NewGuid () |> SessionId

// TODO-NMB-MEDIUM: Permissions?...
type AuthenticatedUser = {
    UserId : UserId
    SessionId : SessionId
    UserName : string }

// TODO-NMB-MEDIUM: Change to string (rather than AuthenticatedUser) - once Jwt functionality implemented...
type Jwt = | Jwt of jwt : AuthenticatedUser

type ChatMessageId = | ChatMessageId of guid : Guid with static member Create () = Guid.NewGuid () |> ChatMessageId

type ChatMessage = {
    ChatMessageId : ChatMessageId
    UserName : string
    MessageText : Markdown }
