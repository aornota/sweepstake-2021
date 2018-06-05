module Aornota.Sweepstake2018.Server.Agents.Entities.Drafts

(* Broadcasts: DraftsRead
               TODO:UserDraftsRead              
   Subscribes: Tick
               SquadsRead
               SquadEventWritten (PlayerAdded | PlayerTypeChanged | PlayerWithdrawn)
               DraftsEventsRead
               TODO:UserDraftsEventsRead *)

open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError
open Aornota.Common.UnitsOfMeasure

open Aornota.Server.Common.Helpers

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Agents.Ticker
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Events.DraftEvents
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserDraftEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private DraftsInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | Housekeeping
    | OnSquadsRead of squadsRead : SquadRead list
    | OnPlayerAdded of squadId : SquadId * playerId : PlayerId * playerType : PlayerType
    | OnPlayerTypeChanged of squadId : SquadId * playerId : PlayerId * playerType : PlayerType
    | OnPlayerWithdrawn of squadId : SquadId * playerId : PlayerId
    | OnDraftsEventsRead of draftsEvents : (DraftId * (Rvn * DraftEvent) list) list
    | OnUserDraftsEventsRead of userDraftsEvents : (UserDraftId * (Rvn * UserDraftEvent) list) list
    | HandleCreateDraftCmd of token : DraftAdminToken * auditUserId : UserId * draftId : DraftId * draftOrdinal : DraftOrdinal * draftType : DraftType
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleProcessDraftCmd of token : DraftAdminToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>

type private Player = { PlayerType : PlayerType ; Withdrawn : bool }
type private PlayerDic = Dictionary<PlayerId, Player>
type private SquadDic = Dictionary<SquadId, PlayerDic>

type private Draft = { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus }
type private DraftDic = Dictionary<DraftId, Draft>

type private UserDraftPick = { UserDraftPick : UserDraftPick ; Rank : uint32 }
type private UserDraft = { UserId : UserId ; DraftId : DraftId ; UserDraftPicks : UserDraftPick list }
type private UserDraftDic = Dictionary<UserDraftId, UserDraft>

// TODO-NEXT: More types?...

let [<Literal>] private HOUSEKEEPING_INTERVAL = 1.<minute>

let private log category = (Entity Entity.Drafts, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private agentId = Guid "ffffffff-ffff-ffff-ffff-000000000001" |> UserId

let private applyDraftEvent source idAndDraftResult (nextRvn, draftEvent:DraftEvent) =
    let otherError errorText = otherError (sprintf "%s#applyDraftEvent" source) errorText
    match idAndDraftResult, draftEvent with
    | Ok (draftId, _), _ when draftId <> draftEvent.DraftId -> // note: should never happen
        ifDebug (sprintf "DraftId mismatch for %A -> %A" draftId draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" draftId nextRvn draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), _ when validateNextRvn (Some draft.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" draftId draft.Rvn nextRvn draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, None), DraftCreated (_, draftOrdinal, draftType) ->
        (draftId, { Rvn = initialRvn ; DraftOrdinal = draftOrdinal ; DraftStatus = draftType |> draftStatus } |> Some) |> Ok
    | Ok (draftId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial DraftEvent for %A -> %A" draftId draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftOpened _ ->
        match draft.DraftStatus with
        | PendingOpen (_, ends) ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = ends |> Opened } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftPendingProcessing _ ->
        match draft.DraftStatus with
        | Opened _ ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = PendingProcessing } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftProcessed _ ->
        match draft.DraftStatus with
        | PendingProcessing ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = Processed } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftFreeSelection _ ->
        match draft.DraftStatus with
        | PendingFreeSelection ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = FreeSelection } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Error error, _ -> error |> Error

let private initializeDrafts source (draftsEvents:(DraftId * (Rvn * DraftEvent) list) list) =
    let source = sprintf "%s#initializeDrafts" source
    let draftDic = DraftDic ()
    let results =
        draftsEvents
        |> List.map (fun (draftId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndDraftResult (rvn, draftEvent) -> applyDraftEvent source idAndDraftResult (rvn, draftEvent)) (Ok (draftId, None))
            | [] -> ifDebug (sprintf "No DraftEvents for %A" draftId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndDraftResult -> match idAndDraftResult with | Ok (draftId, Some draft) -> (draftId, draft) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (draftId, draft) -> draftDic.Add (draftId, draft))
    let errors =
        results
        |> List.choose (fun idAndDraftResult ->
            match idAndDraftResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyDraftEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    draftDic, errors

let private updateDraft draftId draft (draftDic:DraftDic) = if draftId |> draftDic.ContainsKey then draftDic.[draftId] <- draft

let private tryFindDraft draftId onError (draftDic:DraftDic) =
    if draftId |> draftDic.ContainsKey then (draftId, draftDic.[draftId]) |> Ok else ifDebug (sprintf "%A does not exist" draftId) UNEXPECTED_ERROR |> onError

let private tryApplyDraftEvent source draftId draft nextRvn draftEvent =
    match applyDraftEvent source (Ok (draftId, draft)) (nextRvn, draftEvent) with
    | Ok (_, Some post) -> (post, nextRvn, draftEvent) |> Ok
    | Ok (_, None) -> ifDebug "applyDraftEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteDraftEventAsync auditUserId rvn draftEvent (draft:Draft) = async {
    let! result = (auditUserId, rvn, draftEvent) |> persistence.WriteDraftEventAsync
    return match result with | Ok _ -> (draftEvent.DraftId, draft) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

let private ifAllRead (draftDic:DraftDic option, userDraftDic:UserDraftDic option, squadsRead:(SquadRead list) option) =
    match draftDic, userDraftDic, squadsRead with
    | Some draftDic, Some userDraftDic, Some squadsRead ->
        let squadDic = SquadDic ()
        squadsRead
        |> List.iter (fun squadRead ->
            let playerDic = PlayerDic ()
            squadRead.PlayersRead |> List.iter (fun playerRead ->
                let withdrawn = match playerRead.PlayerStatus with | Active -> false | Withdrawn _ -> true
                (playerRead.PlayerId, { PlayerType = playerRead.PlayerType ; Withdrawn = withdrawn }) |> playerDic.Add)
            (squadRead.SquadId, playerDic) |> squadDic.Add)
        (draftDic, userDraftDic, squadDic) |> Some
    | _ -> None

type Drafts () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None None
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | Housekeeping -> "Housekeeping when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerAdded _ -> "OnPlayerAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftsEventsRead _ -> "OnDraftsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserDraftsEventsRead _ -> "OnUserDraftsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateDraftCmd _ -> "HandleCreateDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleProcessDraftCmd _ -> "HandleProcessDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead draftDic userDraftDic squadsRead = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Reset _ -> "Reset when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Housekeeping -> "Housekeeping when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squads/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squads = squadsRead |> Some
                match (draftDic, userDraftDic, squads) |> ifAllRead with
                | Some (draftDic, userDraftDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squads
            | OnPlayerAdded _ -> "OnPlayerAdded when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnDraftsEventsRead draftsEvents ->
                let source = "OnDraftsEventsRead"
                let draftDic, errors = initializeDrafts source draftsEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i event/s) (%i draft/s) when pendingAllRead" source draftsEvents.Length draftDic.Count |> Info |> log
                let draftsRead = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) ->
                    { DraftId = draftId ; Rvn = draft.Rvn ; DraftOrdinal = draft.DraftOrdinal ; DraftStatus = draft.DraftStatus })
                draftsRead |> DraftsRead |> broadcaster.Broadcast
                let draftDic = draftDic |> Some
                match (draftDic, userDraftDic, squadsRead) |> ifAllRead with
                | Some (draftDic, userDraftDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squadsRead
            | OnUserDraftsEventsRead _userDraftsEvents ->
                let _source = "OnUserDraftsEventsRead"

                // TODO-NEXT: Ignore if not for *current* Draft? *Probably* not...
                let userDraftDic = UserDraftDic () // TEMP-NMB...

                let userDraftDic = userDraftDic |> Some
                match (draftDic, userDraftDic, squadsRead) |> ifAllRead with
                | Some (draftDic, userDraftDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleCreateDraftCmd _ -> "HandleCreateDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleProcessDraftCmd _ -> "HandleProcessDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead }
        and managingDrafts draftDic userDraftDic squadDic = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingDrafts draftDic userDraftDic squadDic
            | Start _ -> sprintf "Start when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic squadDic
            | Reset reply ->
                sprintf "Reset when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s) -> pendingAllRead" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None None
            | Housekeeping ->
                let source = "Housekeeping"
                sprintf "%s when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" source draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                // Note: Only limited validation (since other scenarios should never arise).
                let now = DateTimeOffset.UtcNow
                let noLongerOpened = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
                    match draft.DraftStatus with | Opened ends when ends < now -> (draftId, draft) |> Some | _ -> None)
                noLongerOpened |> List.iter (fun (draftId, draft) ->
                    let result =
                        draftId |> DraftPendingProcessing |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn)
                        |> Result.bind (fun (draft, rvn, draftEvent) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft } |> Async.RunSynchronously)
                    result |> logResult source (fun (draftId, draft) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
                    match result with | Ok (draftId, draft) -> draftDic |> updateDraft draftId draft | Error _ -> ())
                let noLongerPendingOpen = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
                    match draft.DraftStatus with | PendingOpen (starts, _) when starts < now -> (draftId, draft) |> Some | _ -> None)               
                noLongerPendingOpen |> List.iter (fun (draftId, draft) ->
                    let result =
                        draftId |> DraftOpened |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn)
                        |> Result.bind (fun (draft, rvn, draftEvent) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft } |> Async.RunSynchronously)
                    result |> logResult source (fun (draftId, draft) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
                    match result with | Ok (draftId, draft) -> draftDic |> updateDraft draftId draft | Error _ -> ())
                let unprocessed = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (_, draft)) ->
                    match draft.DraftStatus with | PendingOpen _ | Opened _ | PendingProcessing -> draft |> Some | _ -> None)
                if unprocessed.Length = 0 then
                    let noLongerPendingFreeSelection = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
                        match draft.DraftStatus with | PendingFreeSelection -> (draftId, draft) |> Some | _ -> None)               
                    noLongerPendingFreeSelection |> List.iter (fun (draftId, draft) ->
                        let result =
                            draftId |> DraftFreeSelection |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn)
                            |> Result.bind (fun (draft, rvn, draftEvent) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft } |> Async.RunSynchronously)
                        result |> logResult source (fun (draftId, draft) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
                        match result with | Ok (draftId, draft) -> draftDic |> updateDraft draftId draft | Error _ -> ())
                return! managingDrafts draftDic userDraftDic squadDic
            | OnSquadsRead _ -> sprintf "OnSquadsRead when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic squadDic
            | OnPlayerAdded (squadId, playerId, playerType) ->
                sprintf "OnPlayerAdded when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey |> not then
                        (playerId, { PlayerType = playerType ; Withdrawn = false }) |> playerDic.Add
                return! managingDrafts draftDic userDraftDic squadDic
            | OnPlayerTypeChanged (squadId, playerId, playerType) ->
                sprintf "OnPlayerTypeChanged when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey then
                        let player = playerDic.[playerId]
                        playerDic.[playerId] <- { player with PlayerType = playerType }
                return! managingDrafts draftDic userDraftDic squadDic
            | OnPlayerWithdrawn (squadId, playerId) ->
                sprintf "OnPlayerWithdrawn when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey then
                        let player = playerDic.[playerId]
                        playerDic.[playerId] <- { player with Withdrawn = true }

                        // TODO-SOON: What if Player has been drafted? Auto-remove? Or just ignore when processing Draft?...

                return! managingDrafts draftDic userDraftDic squadDic
            | OnDraftsEventsRead _ -> sprintf "OnDraftsEventsRead when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic squadDic
            | OnUserDraftsEventsRead _ -> sprintf "OnUserDraftsEventsRead when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic squadDic
            | HandleCreateDraftCmd (_, auditUserId, draftId, draftOrdinal, draftType, reply) ->
                let source = "HandleCreateDraftCmd"
                sprintf "%s for %A (%A %A) when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" source draftId draftOrdinal draftType draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    if draftId |> draftDic.ContainsKey |> not then () |> Ok else ifDebug (sprintf "%A already exists" draftId) UNEXPECTED_ERROR |> otherCmdError source
                    // Note: Only limited validation (since other scenarios should never arise).
                    |> Result.bind (fun _ ->
                        match draftDic |> List.ofSeq |> List.tryFind (fun (KeyValue(_, draft)) -> draft.DraftOrdinal = draftOrdinal) with
                        | Some _ -> ifDebug (sprintf "%A already exists" draftOrdinal) UNEXPECTED_ERROR |> otherCmdError source
                        | None -> () |> Ok)
                    |> Result.bind (fun _ ->
                        let highestOrdinal =
                            if draftDic.Count = 0 then 0
                            else
                                draftDic |> List.ofSeq |> List.map (fun (KeyValue(_, draft)) ->
                                    let (DraftOrdinal ordinal) = draft.DraftOrdinal
                                    ordinal) |> List.max
                        let (DraftOrdinal newOrdinal) = draftOrdinal
                        if newOrdinal <> highestOrdinal + 1 then ifDebug (sprintf "%A is not contiguous with highest existing ordinal %i" draftOrdinal highestOrdinal) UNEXPECTED_ERROR |> otherCmdError source
                        else (draftId, draftOrdinal, draftType) |> DraftCreated |> tryApplyDraftEvent source draftId None initialRvn)
                let! result = match result with | Ok (draft, rvn, draftEvent) -> tryWriteDraftEventAsync auditUserId rvn draftEvent draft | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (draftId, draft) -> sprintf "Audit%A %A %A" auditUserId draftId draft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (draftId, draft) -> (draftId, draft) |> draftDic.Add | Error _ -> ()
                return! managingDrafts draftDic userDraftDic squadDic
            | HandleProcessDraftCmd (_, auditUserId, draftId, currentRvn, reply) ->
                let source = "HandleProcessDraftCmd"
                sprintf "%s for %A (%A) when managingDrafts (%i draft/s) (%i user-draft/s) (%i squads/s)" source draftId currentRvn draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    draftDic |> tryFindDraft draftId (otherCmdError source)
                    |> Result.bind (fun (draftId, draft) ->
                        match draft.DraftStatus with
                        | PendingProcessing -> (draftId, draft) |> Ok
                        | _ -> ifDebug (sprintf "%A (%A) is not PendingProcessing" draftId draft) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (_draftId, _draft) ->

                        // TODO-SOON: Implement Draft processing [return unit on success? DraftOrdinal? TBC...] - including writing (lots of) events?...

                        ifDebug (sprintf "%s is not yet implemented" source) UNEXPECTED_ERROR |> otherCmdError source)
                result |> logResult source (fun (draftId, draft) -> Some (sprintf "Audit%A %A %A" auditUserId draftId draft)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (draftId, draft) -> draftDic |> updateDraft draftId draft | Error _ -> ()
                return! managingDrafts draftDic userDraftDic squadDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Drafts |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in DraftEventWritten | UserDraftEventWritten events (since Drafts agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event ->
                match event with
                | Tick (ticks, secondsPerTick) -> if (ticks, secondsPerTick) |> isEveryNSeconds (int (HOUSEKEEPING_INTERVAL |> minutesToSeconds) * 1<second>) then Housekeeping |> agent.Post
                | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
                | SquadEventWritten (_, userEvent) ->
                    match userEvent with
                    | PlayerAdded (squadId, playerId, _, playerType) -> (squadId, playerId, playerType) |> OnPlayerAdded |> agent.Post
                    | PlayerTypeChanged (squadId, playerId, playerType) -> (squadId, playerId, playerType) |> OnPlayerTypeChanged |> agent.Post
                    | PlayerWithdrawn (squadId, playerId, _) -> (squadId, playerId) |> OnPlayerWithdrawn |> agent.Post
                    | _ -> () // note: only interested in PlayerAdded | PlayerTypeChanged | PlayerWithdrawn
                | DraftsEventsRead draftsEvents -> draftsEvents |> self.OnDraftsEventsRead
                | UserDraftsEventsRead userDraftsEvents -> userDraftsEvents |> self.OnUserDraftsEventsRead
                | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to Tick | SquadsRead | SquadEventWritten (subset) | DraftsEventsRead | UserDraftsEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.Housekeeping () = Housekeeping |> agent.Post
    member __.OnSquadsRead squadsRead = squadsRead |> OnSquadsRead |> agent.Post
    member __.OnDraftsEventsRead draftsEvents = draftsEvents |> OnDraftsEventsRead |> agent.Post
    member __.OnUserDraftsEventsRead userDraftsEvents = userDraftsEvents |> OnUserDraftsEventsRead |> agent.Post
    member __.HandleCreateDraftCmdAsync (token, auditUserId, draftId, draftOrdinal, draftType) =
        (fun reply -> (token, auditUserId, draftId, draftOrdinal, draftType, reply) |> HandleCreateDraftCmd) |> agent.PostAndAsyncReply
    member __.HandleProcessDraftCmdAsync (token, auditUserId, draftId, currentRvn) =
        (fun reply -> (token, auditUserId, draftId, currentRvn, reply) |> HandleProcessDraftCmd) |> agent.PostAndAsyncReply

let drafts = Drafts ()
