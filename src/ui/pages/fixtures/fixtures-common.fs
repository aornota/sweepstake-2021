module Aornota.Sweepstake2018.UI.Pages.Fixtures.Common

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

type FixtureFilter =
    | AllFixtures
    | GroupFixtures of group : Group
    | KnockoutFixtures

type ConfirmParticipantInput = // TODO-SOON-ISH: Or just hack data?...
    | ConfirmConfirmParticipant
    | CancelConfirmParticipant

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerFixturesMsg of serverFixturesMsg : ServerFixturesMsg
    | ShowAllFixtures
    | ShowGroupFixtures of group : Group option
    | ShowKnockoutFixtures
    | ShowConfirmParticipantModal of fixtureId : FixtureId * role : Role
    | ConfirmParticipantInput of confirmParticipantInput : ConfirmParticipantInput

type ConfirmParticipantStatus =
    | ConfirmParticipantPending
    | ConfirmParticipantFailed of errorText : string

type ConfirmParticipantState = { // TODO-SOON-ISH: Or just hack data?...
    ConfirmParticipantStatus : ConfirmParticipantStatus option }

type State = {
    CurrentFixtureFilter : FixtureFilter
    ConfirmParticipantState : ConfirmParticipantState option }
