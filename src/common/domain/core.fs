module Aornota.Sweepstake2018.Common.Domain.Core

open System

type SessionId = | SessionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SessionId

type Group = | GroupA | GroupB | GroupC | GroupD | GroupE | GroupF | GroupG | GroupH
