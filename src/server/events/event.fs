module Aornota.Sweepstake2018.Server.Events.Event

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.User

type Recipients =
    | OnlyConnectionId of connectionId : ConnectionId
    | AllAuthExceptConnectionId of connectionId : ConnectionId
    | AllAuthExceptUserId of userId : UserId
    | SameUserSessionExceptConnectionId of userSession : UserSession * connectionId : ConnectionId

type Event =
    | Tick of ticks : int<tick> * secondsPerTick : int<second/tick>
    | SendMsg of serverMsg : ServerMsg * recipients : Recipients
    | UsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | UsersRead of users : (UserId * UserName * UserType) list
    | UserEventWritten of rvn : Rvn * userEvent : UserEvent
    | UserSignedIn of userId : UserId
    | UserApi of userId : UserId
    | UserSignedOut of userId : UserId
    | Disconnected of connectionId : ConnectionId
