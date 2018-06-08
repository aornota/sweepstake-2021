module Aornota.Sweepstake2018.UI.Pages.Drafts.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerDraftsMsg of serverDraftsMsg : ServerDraftsMsg
    | RemoveFromDraft of draftId : DraftId * userDraftPick : UserDraftPick

type PickOverrideStatus = | Removing

type PickOverride = { UserDraftPick : UserDraftPick ; PickOverrideStatus : PickOverrideStatus }

type PickOverridesState = {
    PickOverrides : PickOverride list
    PendingRvn : Rvn option }

type State = {
    PickOverridesState : PickOverridesState }

let isRemoving pickOverride = match pickOverride.PickOverrideStatus with | Removing -> true
