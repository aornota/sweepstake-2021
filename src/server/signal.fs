module Aornota.Sweepstake2018.Server.Signal

open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.NewsEvents
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents

open System

type UserRead = { UserId : UserId ; Rvn : Rvn ; UserName : UserName ; UserType : UserType }

type NewsRead = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; PostType : PostType ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Removed : bool }

type PlayerRead = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type SquadRead = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayersRead : PlayerRead list }

type Signal =
    | Tick of ticks : int<tick> * secondsPerTick : int<second/tick>
    | SendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list
    | UsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | UsersRead of usersRead : UserRead list
    | UserEventWritten of rvn : Rvn * userEvent : UserEvent
    | NewsEventsRead of newsEvents : (PostId * (Rvn * NewsEvent) list) list
    | NewsRead of newsRead : NewsRead list
    | NewsEventWritten of rvn : Rvn * newsEvent : NewsEvent
    | SquadsEventsRead of squadsEvents : (SquadId * (Rvn * SquadEvent) list) list
    | SquadsRead of squadRead : SquadRead list
    | SquadEventWritten of rvn : Rvn * squadEvent : SquadEvent
    | UserSignedIn of userId : UserId
    | UserSignedOut of userId : UserId
    | UserActivity of userId : UserId
    | ConnectionsSignedOut of connectionIds : ConnectionId list
    | Disconnected of connectionId : ConnectionId
