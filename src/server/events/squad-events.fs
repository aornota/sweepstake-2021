module Aornota.Sweepstake2018.Server.Events.SquadEvents

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad

type SquadEvent =
    | SquadCreated of squadId : SquadId * squadName : SquadName * group : Group * seeding : Seeding * coachName : CoachName
    | PlayerAdded of squadId : SquadId * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | PlayerNameChanged of squadId : SquadId * playerId : PlayerId * playerName : PlayerName
    | PlayerTypeChanged of squadId : SquadId * playerId : PlayerId * playerType : PlayerType
    | PlayerWithdrawn of squadId : SquadId * playerId : PlayerId // TODO-NMB-MEDIUM: dateWithdrawn?...
    | SquadEliminated of squadId : SquadId
    with
        member self.SquadId =
            match self with
            | SquadCreated (squadId, _, _, _, _) -> squadId
            | PlayerAdded (squadId, _, _, _) -> squadId
            | PlayerNameChanged (squadId, _, _) -> squadId
            | PlayerTypeChanged (squadId, _, _) -> squadId
            | PlayerWithdrawn (squadId, _) -> squadId
            | SquadEliminated squadId -> squadId