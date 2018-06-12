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
    | ShowDraft of draftId : DraftId
    | ChangePriority of draftId : DraftId * userDraftPick : UserDraftPick * priorityChange : PriorityChange
    | RemoveFromDraft of draftId : DraftId * userDraftPick : UserDraftPick

type State = {
    CurrentDraftId : DraftId option
    RemovalPending : (UserDraftPick * Rvn) option
    ChangePriorityPending : (UserDraftPick * PriorityChange * Rvn) option
    LastPriorityChanged : (UserDraftPick * PriorityChange) option }
