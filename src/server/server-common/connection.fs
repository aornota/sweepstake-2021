module Aornota.Sweepstake2018.Server.Connection

open Aornota.Sweepstake2018.Common.Domain.Core

open System

type ConnectionId = | ConnectionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> ConnectionId

type UserSession = UserId * SessionId
