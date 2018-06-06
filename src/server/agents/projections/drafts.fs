module Aornota.Sweepstake2018.Server.Agents.Projections.Drafts

(* Broadcasts: SendMsg
   Subscribes: DraftsRead
               DraftEventWritten (DraftCreated | DraftOpened | DraftPendingProcessing | DraftProcessed | DraftFreeSelection)
               TODO:UserDraftsRead
               TODO:UserDraftEventWritten (TBC)
               ConnectionsSignedOut | Disconnected *)

open Aornota.Common.Revision

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.DraftEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private SquadsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnDraftsRead of draftsRead : DraftRead list
    | OnDraftEventWritten of draftEvent : DraftEvent
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeDraftsProjectionQry of token : DraftToken * connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<CurrentDraftDto option, AuthQryError<string>>>

type private Draft = { DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus }
type private DraftDic = Dictionary<DraftId, Draft>

type private Projectee = { LastRvn : Rvn ; UserId : UserId }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { CurrentDraftDto : CurrentDraftDto option }

type private StateChangeType =
    | Initialization of draftDic : DraftDic
    | DraftChange of draftDic : DraftDic * state : State

let private log category = (Projection Drafts, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private currentDraftDto (draftDic:DraftDic) =
    let drafts = draftDic |> List.ofSeq |> List.sortBy (fun (KeyValue (_, draft)) ->
        match draft.DraftStatus with | Opened _ -> 1 | PendingProcessing -> 2 | PendingOpen _ -> 3 | FreeSelection -> 4 | _ -> 5)
    match drafts with
    | KeyValue (draftId, draft) :: _ ->
        let draftStatusDto =
            match draft.DraftStatus with
            | PendingOpen (starts, ends) -> (starts, ends) |> PendingOpenDto |> Some
            | Opened ends -> ends |> OpenedDto |> Some
            | PendingProcessing -> PendingProcessingDto |> Some
            | FreeSelection -> FreeSelectionDto |> Some
            | _ -> None // note: should never happen
        match draftStatusDto with
        | Some draftStatusDto -> { DraftId = draftId ; DraftOrdinal = draft.DraftOrdinal ; DraftStatusDto = draftStatusDto } |> Some
        | None -> None
    | [] -> None

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendCurrentDraftDto (projecteeDic:ProjecteeDic) currentDraftDto =
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, _)) ->
        currentDraftDto |> CurrentDraftChangedMsg |> DraftsProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ])

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization draftDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { CurrentDraftDto = draftDic |> currentDraftDto }
        | DraftChange (draftDic, state) ->
            let currentDraftDto = draftDic |> currentDraftDto
            if currentDraftDto <> state.CurrentDraftDto then
                sprintf "%s -> CurrentDraftDto changed %A -> %i projectee/s" source currentDraftDto projecteeDic.Count |> Info |> log
                currentDraftDto |> sendCurrentDraftDto projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with CurrentDraftDto = currentDraftDto }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private ifAllRead source (draftsRead:(DraftRead list) option) =
    match draftsRead with
    | Some draftsRead ->
        let draftDic = DraftDic ()
        draftsRead |> List.iter (fun draftRead -> (draftRead.DraftId, { DraftOrdinal = draftRead.DraftOrdinal ; DraftStatus = draftRead.DraftStatus }) |> draftDic.Add)
        let projecteeDic = ProjecteeDic ()
        let state = draftDic |> Initialization |> updateState source projecteeDic
        (state, draftDic, projecteeDic) |> Some
    | _ -> None

type Drafts () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 drafts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None
            | OnDraftsRead _ -> "OnDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftEventWritten _ -> "OnDraftEventWritten when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeDraftsProjectionQry _ -> "HandleInitializeDraftsProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead draftsRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead
            | OnDraftsRead draftsRead ->
                let source = "OnDraftsRead"
                sprintf "%s (%i draft/s) when pendingAllRead" source draftsRead.Length |> Info |> log
                let draftsRead = draftsRead |> Some
                match draftsRead |> ifAllRead source with
                | Some (state, draftDic, projecteeDic) ->
                    return! projectingDrafts state draftDic projecteeDic
                | None -> return! pendingAllRead draftsRead
            | OnDraftEventWritten _ -> "OnDraftEventWritten when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead
            | RemoveConnections _ -> "RemoveConnections when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead
            | HandleInitializeDraftsProjectionQry _ -> "HandleInitializeDraftsProjectionQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead }
        and projectingDrafts state draftDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingDrafts" |> IgnoredInput |> Agent |> log ; return! projectingDrafts state draftDic projecteeDic
            | OnDraftsRead _ -> "OnDraftsRead when projectingDrafts" |> IgnoredInput |> Agent |> log ; return! projectingDrafts state draftDic projecteeDic
            | OnDraftEventWritten draftEvent ->
                let source = "OnDraftEventWritten"
                sprintf "%s (%A) when projectingDrafts (%i draft/s) (%i projectee/s)" source draftEvent draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    match draftEvent with
                    | DraftCreated (draftId, draftOrdinal, draftType) ->
                        if draftId |> draftDic.ContainsKey |> not then // note: silently ignore already-known draftId (should never happen)
                            (draftId, { DraftOrdinal = draftOrdinal ; DraftStatus = draftType |> draftStatus }) |> draftDic.Add
                            (draftDic, state) |> DraftChange |> updateState source projecteeDic
                        else state
                    | _ ->
                        let draftId = draftEvent.DraftId
                        if draftId |> draftDic.ContainsKey then // note: silently ignore unknown draftId (should never happen)
                            let draft = draftDic.[draftId]
                            let draft =
                                match draftEvent with
                                | DraftOpened _ -> match draft.DraftStatus with | PendingOpen (_, ends) -> { draft with DraftStatus = ends |> Opened } | _ -> draft
                                | DraftPendingProcessing _ -> match draft.DraftStatus with | Opened _ -> { draft with DraftStatus = PendingProcessing } | _ -> draft
                                | DraftProcessed _ -> match draft.DraftStatus with | PendingProcessing -> { draft with DraftStatus = Processed } | _ -> draft
                                | DraftFreeSelection _ -> match draft.DraftStatus with | PendingFreeSelection -> { draft with DraftStatus = FreeSelection } | _ -> draft
                                | _ -> draft // note: should never happen
                            draftDic.[draftId] <- draft
                            (draftDic, state) |> DraftChange |> updateState source projecteeDic
                        else state
                return! projectingDrafts state draftDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingDrafts (%i draft/s) (%i projectee/s)" source connectionIds draftDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingDrafts -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingDrafts state draftDic projecteeDic
            | HandleInitializeDraftsProjectionQry (_, connectionId, userId, reply) ->
                let source = "HandleInitializeDraftsProjectionQry"
                sprintf "%s for %A (%A) when projectingDrafts (%i draft/s) (%i projectee/s)" source connectionId userId draftDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = userId }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingDrafts -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state.CurrentDraftDto |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingDrafts state draftDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Drafts |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | DraftsRead draftsRead -> draftsRead |> OnDraftsRead |> agent.Post
            | DraftEventWritten (_, draftEvent) -> draftEvent |> OnDraftEventWritten |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to DraftsRead | DraftEventWritten | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeDraftsProjectionQryAsync (token, connectionId, userId) =
        (fun reply -> (token, connectionId, userId, reply) |> HandleInitializeDraftsProjectionQry) |> agent.PostAndAsyncReply

let drafts = Drafts ()
