module Aornota.Sweepstake2018.Server.Signal

open Aornota.Common.Revision
open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents

type UserRead = { UserId : UserId ; Rvn : Rvn ; UserName : UserName ; UserType : UserType }

type PlayerRead = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type SquadRead = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayersRead : PlayerRead list }

type Signal =
    | Tick of ticks : int<tick> * secondsPerTick : int<second/tick>
    | SendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list
    | UsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | SquadsEventsRead of squadsEvents : (SquadId * (Rvn * SquadEvent) list) list
    | UsersRead of usersRead : UserRead list
    | SquadsRead of squadRead : SquadRead list
    | UserEventWritten of rvn : Rvn * userEvent : UserEvent
    | SquadEventWritten of rvn : Rvn * squadEvent : SquadEvent
    | UserSignedIn of userId : UserId
    | UserSignedOut of userId : UserId
    | UserActivity of userId : UserId
    | ConnectionsSignedOut of connectionIds : ConnectionId list
    | Disconnected of connectionId : ConnectionId
