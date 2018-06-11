module Aornota.Sweepstake2018.Common.Domain.Core

open System

type SessionId = | SessionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SessionId

type Group = | GroupA | GroupB | GroupC | GroupD | GroupE | GroupF | GroupG | GroupH

type DraftOrdinal = | DraftOrdinal of draftOrdinal : int

let groups = [ GroupA ; GroupB ; GroupC ; GroupD ; GroupE ; GroupF ; GroupG ; GroupH ]

let groupText group =
    let groupText = match group with | GroupA -> "A" | GroupB -> "B" | GroupC -> "C" | GroupD -> "D" | GroupE -> "E" | GroupF -> "F" | GroupG -> "G" | GroupH -> "H"
    sprintf "Group %s" groupText
