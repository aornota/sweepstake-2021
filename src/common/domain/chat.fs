module Aornota.Sweepstake2018.Common.Domain.Chat

(*open Aornota.Common.Projection
open Aornota.Common.UnitsOfMeasure*)

open Aornota.Sweepstake2018.Common.Domain.Core

open System

(*type ChatUserDto =
    {
        UserId : UserId
        UserName : UserName
        SignedInStatusDto : SignedInStatusDto
    }
    interface IItemId<ChatUserDto> with
        member self.ItemId =
            let (UserId id) = self.UserId
            id*)

type ChatMessageId = | ChatMessageId of guid : Guid with
    static member Create () = Guid.NewGuid () |> ChatMessageId

type ChatMessageOLD = { // TODO-NMB-HIGH: Retire this...
    ChatMessageId : ChatMessageId
    UserName : string
    MessageText : Markdown }

// TODO-NMB-HIGH: "Persist" ChatMessages (i.e. in memory) for a short period (e.g. clear via Tick _), so can send "recent" in ChatProjection?...

(*type ChatMessageDto =
    {
        ChatMessageId : ChatMessageId
        UserId : UserId
        MessageText : Markdown
        SinceSent : float<second>
    }
    interface IItemId<ChatMessageDto> with
        member self.ItemId =
            let (ChatMessageId id) = self.ChatMessageId
            id*)
