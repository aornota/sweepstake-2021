module Aornota.Sweepstake2018.Server.Events.UserDraftEvents

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.User

open System

type UserDraftId = | UserDraftId of guid : Guid with
    static member Create () = Guid.NewGuid () |> UserDraftId

type UserDraftEvent =
    | UserDraftCreated of userDraftId : UserDraftId * userId : UserId * draftId : DraftId
    | Drafted of userDraftId : UserDraftId * userDraftPick : UserDraftPick
    | Undrafted of userDraftId : UserDraftId * userDraftPick : UserDraftPick
    // TODO-SOON... | PriorityIncreased of userDraftId : UserDraftId * ...
    with
        member self.UserDraftId =
            match self with
            | UserDraftCreated (userDraftId, _, _) -> userDraftId
            | Drafted (userDraftId, _) -> userDraftId
            | Undrafted (userDraftId, _) -> userDraftId
