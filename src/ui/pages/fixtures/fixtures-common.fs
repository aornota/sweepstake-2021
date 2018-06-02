module Aornota.Sweepstake2018.UI.Pages.Fixtures.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System
open System.Collections.Generic

type FixtureFilter =
    | AllFixtures
    | GroupFixtures of group : Group
    | KnockoutFixtures

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerFixturesMsg of serverFixturesMsg : ServerFixturesMsg
    | ShowAllFixtures
    | ShowGroupFixtures of group : Group option
    | ShowKnockoutFixtures
    | ShowConfirmParticipantModal of fixtureId : FixtureId * role : Role

type Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipantDto : ParticipantDto ; AwayParticipantDto : ParticipantDto ; KickOff : DateTimeOffset }
type FixtureDic = Dictionary<FixtureId, Fixture>

type FixturesProjection = { Rvn : Rvn ; FixtureDic : FixtureDic }

type ActiveState = {
    FixturesProjection : FixturesProjection
    CurrentFixtureFilter : FixtureFilter }

type ProjectionState =
    | Initializing of currentFixtureFilter : FixtureFilter
    | InitializationFailed
    | Active of activeState : ActiveState

type State = { ProjectionState : ProjectionState }
