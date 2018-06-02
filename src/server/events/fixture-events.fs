module Aornota.Sweepstake2018.Server.Events.FixtureEvents

open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.Squad

open System

type FixtureEvent =
    | FixtureCreated of fixtureId : FixtureId * stage : Stage * homeParticipant : Participant * awayParticipant : Participant * kickOff : DateTimeOffset
    | ParticipantConfirmed of fixtureId : FixtureId * role : Role * squadId : SquadId
    with
        member self.FixtureId =
            match self with
            | FixtureCreated (fixtureId, _, _, _, _) -> fixtureId
            | ParticipantConfirmed (fixtureId, _, _) -> fixtureId
