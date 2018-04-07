module Aornota.Sweepstake2018.Shared.Domain

open System

type UserId = | UserId of guid : Guid with static member Create () = Guid.NewGuid () |> UserId
type SessionId = | SessionId of guid : Guid with static member Create () = Guid.NewGuid () |> SessionId

// TODO-NMB: Permissions?...
type AuthenticatedUser = {
    UserId : UserId
    SessionId : SessionId
    UserName : string }

// TODO-NMB: Change to string (rather than AuthenticatedUser) - once Jwt functionality implemented...
type Jwt = | Jwt of jwt : AuthenticatedUser

type ChatMessageId = | ChatMessageId of guid : Guid with static member Create () = Guid.NewGuid () |> ChatMessageId

type ChatMessage = {
    ChatMessageId : ChatMessageId
    UserName : string
    MessageText : string }
