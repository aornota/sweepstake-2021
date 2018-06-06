module Aornota.Sweepstake2018.Common.Domain.Fixture

open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad

open System

type FixtureId = | FixtureId of guid : Guid with
    static member Create () = Guid.NewGuid () |> FixtureId

type Role = | Home | Away

type Stage =
    | Group of group : Group
    | RoundOf16 of matchNumber : uint32
    | QuarterFinal of quarterFinalOrdinal : uint32
    | SemiFinal of semiFinalOrdinal : uint32
    | ThirdPlacePlayOff
    | Final

type Unconfirmed =
    | Winner of stage : Stage
    | RunnerUp of group : Group
    | Loser of semiFinalOrdinal : uint32

type Participant =
    | Confirmed of squadId : SquadId
    | Unconfirmed of unconfirmed : Unconfirmed

type FixtureDto = { FixtureId : FixtureId ; Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }
