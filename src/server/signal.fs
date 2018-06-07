module Aornota.Sweepstake2018.Server.Signal

open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.DraftEvents
open Aornota.Sweepstake2018.Server.Events.FixtureEvents
open Aornota.Sweepstake2018.Server.Events.NewsEvents
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserDraftEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents

open System

type UserRead = { UserId : UserId ; Rvn : Rvn ; UserName : UserName ; UserType : UserType }

type NewsRead = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; PostType : PostType ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Removed : bool }

type PlayerRead = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type SquadRead = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayersRead : PlayerRead list }

type FixtureRead = { FixtureId : FixtureId ; Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }

type DraftRead = { DraftId : DraftId ; Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus }

type UserDraftPickRead = { UserDraftPick : UserDraftPick ; Rank : int }
type UserDraftRead = { UserDraftId : UserDraftId ; Rvn : Rvn ; UserDraftKey : UserDraftKey ; UserDraftPicksRead : UserDraftPickRead list }

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
    | FixturesEventsRead of fixturesEvents : (FixtureId * (Rvn * FixtureEvent) list) list
    | FixturesRead of fixturesRead : FixtureRead list
    | FixtureEventWritten of rvn : Rvn * fixtureEvent : FixtureEvent
    | DraftsEventsRead of draftsEvents : (DraftId * (Rvn * DraftEvent) list) list
    | DraftsRead of draftsRead : DraftRead list
    | DraftEventWritten of rvn : Rvn * draftEvent : DraftEvent
    | UserDraftsEventsRead of userDraftsEvents : (UserDraftId * (Rvn * UserDraftEvent) list) list
    | UserDraftsRead of userDraftsRead : UserDraftRead list
    | UserDraftEventWritten of rvn : Rvn * userDraftEvent : UserDraftEvent
    | UserSignedIn of userId : UserId
    | UserSignedOut of userId : UserId
    | UserActivity of userId : UserId
    | ConnectionsSignedOut of connectionIds : ConnectionId list
    | Disconnected of connectionId : ConnectionId
