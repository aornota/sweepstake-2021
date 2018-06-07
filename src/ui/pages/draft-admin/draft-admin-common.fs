module Aornota.Sweepstake2018.UI.Pages.DraftAdmin.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Shared

open System.Collections.Generic

type ProcessDraftInput = // TODO-SOON-ISH...
    | ProcessDraft
    | CancelProcessDraft

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerDraftAdminMsg of serverDraftAdminMsg : ServerDraftAdminMsg
    | ShowProcessDraftModal of draftId : DraftId
    | ProcessDraftInput of processDraftInput : ProcessDraftInput

type UserDraftSummaryDic = Dictionary<UserDraftKey, UserDraftSummaryDto>

type ProcessDraftStatus =
    | ProcessDraftPending
    | ProcessDraftFailed of errorText : string

type ProcessDraftState = { // TODO-SOON-ISH...
    ProcessDraftStatus : ProcessDraftStatus option }

type State = {
    UserDraftSummaryProjection : Projection<Rvn * UserDraftSummaryDic>
    ProcessDraftState : ProcessDraftState option }
