module Aornota.Sweepstake2018.Server.Events.DraftEvents

open Aornota.Sweepstake2018.Common.Domain.Draft

type DraftEvent =
    | DraftCreated of draftId : DraftId * draftOrdinal : DraftOrdinal * draftType : DraftType
    | DraftOpened of draftId : DraftId
    | DraftPendingProcessing of draftId : DraftId
    | DraftProcessed of draftId : DraftId
    | DraftFreeSelection of draftId : DraftId
    with
        member self.DraftId =
            match self with
            | DraftCreated (draftId, _, _) -> draftId
            | DraftOpened draftId -> draftId
            | DraftPendingProcessing draftId -> draftId
            | DraftProcessed draftId -> draftId
            | DraftFreeSelection draftId -> draftId
