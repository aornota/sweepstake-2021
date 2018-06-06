module Aornota.Sweepstake2018.Common.Domain.Draft

open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Squad

open System

type DraftId = | DraftId of guid : Guid with
    static member Create () = Guid.NewGuid () |> DraftId

type DraftOrdinal = | DraftOrdinal of draftOrdinal : int

type DraftType =
    | Constrained of starts : DateTimeOffset * ends : DateTimeOffset
    | Unconstrained

type DraftStatus =
    | PendingOpen of starts : DateTimeOffset * ends : DateTimeOffset
    | Opened of ends : DateTimeOffset
    | PendingProcessing
    | Processed
    | PendingFreeSelection
    | FreeSelection

type DraftStatusDto =
    | PendingOpenDto of starts : DateTimeOffset * ends : DateTimeOffset
    | OpenedDto of ends : DateTimeOffset
    | PendingProcessingDto
    | FreeSelectionDto

type CurrentDraftDto = { DraftId : DraftId ; DraftOrdinal : DraftOrdinal ; DraftStatusDto : DraftStatusDto }

type UserDraftPick =
    | TeamPick of squadId : SquadId
    | PlayerPick of squadId : SquadId * playerId : PlayerId

let draftTextLower (DraftOrdinal draftOrdinal) =
    if draftOrdinal = 1 then "first draft"
    else if draftOrdinal = 2 then "second draft"
    else if draftOrdinal = 4 then "third draft"
    else sprintf "draft #%i" draftOrdinal

let draftStatus draftType = match draftType with | Constrained (starts, ends) -> (starts, ends) |> PendingOpen | Unconstrained -> PendingFreeSelection
