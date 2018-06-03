module Aornota.Sweepstake2018.Server.Agents.Projections.Squads

(* Broadcasts: SendMsg
   Subscribes: UsersRead
               UserEventWritten (UserCreated only)
               SquadsRead
               SquadEventWritten (PlayerAdded | PlayerNameChanged | PlayerTypeChanged | PlayerWithdrawn | SquadEliminated)
               DraftsRead
               DraftEventWritten (DraftCreated | DraftOpened | DraftPendingProcessing | DraftProcessed | DraftFreeSelection)
               TODO:UserDraftsRead
               TODO:UserDraftEventWritten TBC)
               ConnectionsSignedOut | Disconnected *)

open Aornota.Common.Revision

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.DraftEvents
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private SquadsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnUsersRead of usersRead : UserRead list
    | OnSquadsRead of squadsRead : SquadRead list
    | OnDraftsRead of draftsRead : DraftRead list
    | OnUserCreated of userId : UserId * userName : UserName
    | OnPlayerAdded of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | OnPlayerNameChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | OnPlayerTypeChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | OnPlayerWithdrawn of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * dateWithdrawn : DateTimeOffset option
    | OnSquadEliminated of squadId : SquadId * squadRvn : Rvn
    | OnDraftEventWritten of draftEvent : DraftEvent
    | SignOutConnections of connectionIds : ConnectionId list
    | RemoveConnection of connectionId : ConnectionId
    | HandleInitializeSquadsProjectionUnauthQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<SquadsProjectionDto, OtherError<string>>>
    | HandleInitializeSquadsProjectionAuthQry of token : SquadsProjectionAuthQryToken * connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<SquadsProjectionDto * CurrentDraftDto option, AuthQryError<string>>>

type private UserDic = Dictionary<UserId, UserName>

type private Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus } // TODO-NMB-MEDIUM: pickedBy? score?...
type private PlayerDic = Dictionary<PlayerId, Player>

type private Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic }
type private SquadDic = Dictionary<SquadId, Squad>

type private Draft = { DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus }
type private DraftDic = Dictionary<DraftId, Draft>

type private Projectee = { LastRvn : Rvn ; UserId : UserId option }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { SquadDic : SquadDic ; CurrentDraftDto : CurrentDraftDto option }

type private StateChangeType =
    | Initialization of squadDic : SquadDic * draftDic : DraftDic
    | SquadChange of squadDic : SquadDic * state : State
    | PlayerChange of squadId : SquadId * squadRvn : Rvn * playerDic : PlayerDic * state : State
    | DraftChange of draftDic : DraftDic * state : State

type private PlayerDtoDic = Dictionary<PlayerId, PlayerDto>
type private SquadOnlyDtoDic = Dictionary<SquadId, SquadOnlyDto>

let private log category = (Projection Squads, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private playerDto (playerId, player:Player) =
    match player.PlayerStatus with
    | Active | Withdrawn (Some _) -> { PlayerDto.PlayerId = playerId ; PlayerName = player.PlayerName ; PlayerType = player.PlayerType ; PlayerStatus = player.PlayerStatus } |> Some
    | Withdrawn None -> None

let private playerDtoDic (playerDic:PlayerDic) =
    let playerDtoDic = PlayerDtoDic ()
    playerDic |> List.ofSeq |> List.iter (fun (KeyValue (playerId, player)) ->
        match (playerId, player) |> playerDto with | Some playerDto -> (playerDto.PlayerId, playerDto) |> playerDtoDic.Add | None -> ())
    playerDtoDic

let private squadOnlyDto (squadId, squad:Squad) =
    { SquadId = squadId ; Rvn = squad.Rvn ; SquadName = squad.SquadName ; Group = squad.Group ; Seeding = squad.Seeding ; CoachName = squad.CoachName ; Eliminated = squad.Eliminated }

let private squadOnlyDtoDic (squadDic:SquadDic) =
    let squadOnlyDtoDic = SquadOnlyDtoDic ()
    squadDic |> List.ofSeq |> List.iter (fun (KeyValue (squadId, squad)) ->
        let squadOnlyDto = (squadId, squad) |> squadOnlyDto
        (squadOnlyDto.SquadId, squadOnlyDto) |> squadOnlyDtoDic.Add)
    squadOnlyDtoDic

let private squadDto (squadId, squad:Squad) =
    let playerDtos = squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (playerId, player)) -> (playerId, player) |> playerDto)
    { SquadOnlyDto = (squadId, squad) |> squadOnlyDto ; PlayerDtos = playerDtos }

let private squadsProjectionDto state =
    let squadDtos = state.SquadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> (squadId, squad) |> squadDto)
    { SquadDtos = squadDtos }

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

let private squadsProjectionDtoPlus (_userId:UserId) state =
    let squadDtos = state.SquadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> (squadId, squad) |> squadDto)
    { SquadDtos = squadDtos }, state.CurrentDraftDto

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendPlayerDtoDelta squadId squadRvn (projecteeDic:ProjecteeDic) playerDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendPlayerDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, squadId, squadRvn, playerDtoDelta) |> PlayersDeltaMsg |> SquadsProjectionMsg |> ServerSquadsMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendSquadOnlyDtoDelta (projecteeDic:ProjecteeDic) squadOnlyDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendSquadOnlyDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, squadOnlyDtoDelta) |> SquadsDeltaMsg |> SquadsProjectionMsg |> ServerSquadsMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendCurrentDraftDto (projecteeDic:ProjecteeDic) currentDraftDto =
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        match projectee.UserId with | Some _ -> currentDraftDto |> CurrentDraftChangedMsg |> SquadsProjectionMsg |> ServerSquadsMsg |> sendMsg [ connectionId ] | None -> ())

let private copySquadDic (squadDic:SquadDic) =
    let copiedSquadDic = SquadDic ()
    squadDic |> List.ofSeq |> List.iter (fun (KeyValue (squadId, squad)) ->
        let squad = { squad with PlayerDic = PlayerDic squad.PlayerDic }
        (squadId, squad) |> copiedSquadDic.Add)
    copiedSquadDic

let private tryFindUserName (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey |> not then None // note: silently ignore unknown userId (should never happen)
    else userDic.[userId] |> Some

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization (squadDic, draftDic) ->
            sprintf "%s -> initialized" source |> Info |> log
            { SquadDic = squadDic |> copySquadDic ; CurrentDraftDto = draftDic |> currentDraftDto }
        | SquadChange (squadDic, state) ->
            let previousSquadOnlyDtoDic = state.SquadDic |> squadOnlyDtoDic
            let squadOnlyDtoDic = squadDic |> squadOnlyDtoDic
            let squadOnlyDtoDelta = squadOnlyDtoDic |> delta previousSquadOnlyDtoDic
            if squadOnlyDtoDelta |> isEmpty |> not then
                sprintf "%s -> SquadOnlyDto delta %A -> %i projectee/s" source squadOnlyDtoDelta projecteeDic.Count |> Info |> log
                squadOnlyDtoDelta |> sendSquadOnlyDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with SquadDic = squadDic |> copySquadDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
        | PlayerChange (squadId, squadRvn, playerDic, state) ->
            let squadDic = state.SquadDic
            if squadId |> squadDic.ContainsKey then
                let squad = squadDic.[squadId]
                let previousPlayerDtoDic = squad.PlayerDic |> playerDtoDic
                let playerDtoDic = playerDic |> playerDtoDic
                let playerDtoDelta = playerDtoDic |> delta previousPlayerDtoDic
                if playerDtoDelta |> isEmpty |> not then
                    sprintf "%s -> PlayerDto delta %A -> %i projectee/s" source playerDtoDelta projecteeDic.Count |> Info |> log
                    playerDtoDelta |> sendPlayerDtoDelta squadId squadRvn projecteeDic
                    let squad = { squad with Rvn = squadRvn ; PlayerDic = PlayerDic playerDic }
                    squadDic.[squadId] <- squad
                    sprintf "%s -> updated" source |> Info |> log
                    state
                else
                    sprintf "%s -> unchanged" source |> Info |> log
                    state
            else // note: should never happen
                state
        | DraftChange (draftDic, state) ->
            let currentDraftDto = draftDic |> currentDraftDto
            if currentDraftDto <> state.CurrentDraftDto then
                sprintf "%s -> CurrentDraftDto changed %A -> %i (potential) projectee/s" source currentDraftDto projecteeDic.Count |> Info |> log
                currentDraftDto |> sendCurrentDraftDto projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with CurrentDraftDto = currentDraftDto }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private ifAllRead source (usersRead:(UserRead list) option, squadsRead:(SquadRead list) option, draftsRead:(DraftRead list) option) =
    match usersRead, squadsRead, draftsRead with
    | Some usersRead, Some squadsRead, Some draftsRead ->
        let userDic = UserDic ()
        usersRead |> List.iter (fun userRead -> (userRead.UserId, userRead.UserName) |> userDic.Add)
        let squadDic = SquadDic ()
        squadsRead |> List.iter (fun squadRead ->
            let playerDic = PlayerDic ()
            squadRead.PlayersRead |> List.iter (fun playerRead ->
                let player = { PlayerName = playerRead.PlayerName ; PlayerType = playerRead.PlayerType ; PlayerStatus = playerRead.PlayerStatus }
                (playerRead.PlayerId, player) |> playerDic.Add)
            let squad = {
                Rvn = squadRead.Rvn ; SquadName = squadRead.SquadName ; Group = squadRead.Group ; Seeding = squadRead.Seeding ; CoachName = squadRead.CoachName
                Eliminated = squadRead.Eliminated ; PlayerDic = playerDic }
            (squadRead.SquadId, squad) |> squadDic.Add)
        let draftDic = DraftDic ()
        draftsRead |> List.iter (fun draftRead -> (draftRead.DraftId, { DraftOrdinal = draftRead.DraftOrdinal ; DraftStatus = draftRead.DraftStatus }) |> draftDic.Add)
        let projecteeDic = ProjecteeDic ()
        let state = (squadDic, draftDic) |> Initialization |> updateState source projecteeDic
        (state, userDic, squadDic, draftDic, projecteeDic) |> Some
    | _ -> None

type Squads () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 squads) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None None
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftsRead _ -> "OnDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerAdded _ -> "OnPlayerAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerNameChanged _ -> "OnPlayerNameChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadEliminated _ -> "OnSquadEliminated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftEventWritten _ -> "OnDraftEventWritten when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | SignOutConnections _ -> "SignOutConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeSquadsProjectionUnauthQry _ -> "HandleInitializeSquadsProjectionUnauthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeSquadsProjectionAuthQry _ -> "HandleInitializeSquadsProjectionAuthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead usersRead squadsRead draftsRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnUsersRead usersRead ->
                let source = "OnUsersRead"
                sprintf "%s (%i user/s) when pendingAllRead" source usersRead.Length |> Info |> log
                let usersRead = usersRead |> Some
                match (usersRead, squadsRead, draftsRead) |> ifAllRead source with
                | Some (state, userDic, squadDic, draftDic, projecteeDic) ->
                    return! projectingSquads state userDic squadDic draftDic projecteeDic
                | None -> return! pendingAllRead usersRead squadsRead draftsRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squads/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squadsRead = squadsRead |> Some
                match (usersRead, squadsRead, draftsRead) |> ifAllRead source with
                | Some (state, userDic, squadDic, draftDic, projecteeDic) ->
                    return! projectingSquads state userDic squadDic draftDic projecteeDic
                | None -> return! pendingAllRead usersRead squadsRead draftsRead
            | OnDraftsRead draftsRead ->
                let source = "OnDraftsRead"
                sprintf "%s (%i draft/s) when pendingAllRead" source draftsRead.Length |> Info |> log
                let draftsRead = draftsRead |> Some
                match (usersRead, squadsRead, draftsRead) |> ifAllRead source with
                | Some (state, userDic, squadDic, draftDic, projecteeDic) ->
                    return! projectingSquads state userDic squadDic draftDic projecteeDic
                | None -> return! pendingAllRead usersRead squadsRead draftsRead
            | OnUserCreated _ -> "OnUserCreated when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnPlayerAdded _ -> "OnPlayerAdded when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnPlayerNameChanged _ -> "OnPlayerNameChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnSquadEliminated _ -> "OnSquadEliminated when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | OnDraftEventWritten _ -> "OnDraftEventWritten when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | SignOutConnections _ -> "SignOutConnections when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | RemoveConnection _ -> "RemoveConnection when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | HandleInitializeSquadsProjectionUnauthQry _ -> "HandleInitializeSquadsProjectionUnauthQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead
            | HandleInitializeSquadsProjectionAuthQry _ -> "HandleInitializeSquadsProjectionAuthQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead usersRead squadsRead draftsRead }
        and projectingSquads state userDic squadDic draftDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnUsersRead _ -> "OnUsersRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnSquadsRead _ -> "OnSquadsRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnDraftsRead _ -> "OnDraftsRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnUserCreated (userId, userName) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source userId userName userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, userName) |> userDic.Add
                sprintf "%s when projectingSquads -> %i user/s)" source userDic.Count |> Info |> log
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnPlayerAdded (squadId, squadRvn, playerId, playerName, playerType) ->
                let source = "OnPlayerAdded"
                sprintf "%s (%A %A %A %A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source squadId squadRvn playerId playerName playerType userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey |> not then // note: silently ignore already-known playerId (should never happen)
                            (playerId, { PlayerName = playerName ; PlayerType = playerType ; PlayerStatus = Active }) |> playerDic.Add
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnPlayerNameChanged (squadId, squadRvn, playerId, playerName) ->
                let source = "OnPlayerNameChanged"
                sprintf "%s (%A %A %A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source squadId squadRvn playerId playerName userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerName = playerName }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnPlayerTypeChanged (squadId, squadRvn, playerId, playerType) ->
                let source = "OnPlayerTypeChanged"
                sprintf "%s (%A %A %A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source squadId squadRvn playerId playerType userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerType = playerType }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnPlayerWithdrawn (squadId, squadRvn, playerId, dateWithdrawn) ->
                let source = "OnPlayerWithdrawn"
                sprintf "%s (%A %A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source squadId squadRvn playerId userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerStatus = dateWithdrawn |> Withdrawn }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnSquadEliminated (squadId, squadRvn) ->
                let source = "OnSquadEliminated"
                sprintf "%s (%A %A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source squadId squadRvn userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        squadDic.[squadId] <- { squad with Rvn = squadRvn ; Eliminated = true }
                        (squadDic, state) |> SquadChange |> updateState source projecteeDic
                    else state
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | OnDraftEventWritten draftEvent ->
                let source = "OnDraftEventWritten"
                sprintf "%s (%A) when projectingSquads (%i user/s) (%i squads/s) (%i draft/s) (%i projectee/s)" source draftEvent userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
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
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | SignOutConnections connectionIds ->
                let source = "SignOutConnections"
                sprintf "%s (%A) when projectingSquads (%i user/s) (%i squad/s) (%i draft/s) (%i projectee/s)" source connectionIds userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId ->
                    if connectionId |> projecteeDic.ContainsKey then // note: silently ignore unknown connectionIds 
                        let projectee = projecteeDic.[connectionId]
                        projecteeDic.[connectionId] <- { projectee with UserId = None })
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | RemoveConnection connectionId ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingSquads (%i user/s) (%i squad/s) (%i draft/s) (%i projectee/s)" source connectionId userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | HandleInitializeSquadsProjectionUnauthQry (connectionId, reply) ->
                let source = "HandleInitializeSquadsProjectionUnauthQry"
                sprintf "%s for %A when projectingSquads (%i user/s) (%i squad/s) (%i draft/s) (%i projectee/s)" source connectionId userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = None }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingSquads -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> squadsProjectionDto |> Ok
                result |> logResult source (fun squadsProjectionDto -> sprintf "%i squad/s" squadsProjectionDto.SquadDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingSquads state userDic squadDic draftDic projecteeDic
            | HandleInitializeSquadsProjectionAuthQry (_, connectionId, userId, reply) ->
                let source = "HandleInitializeSquadsProjectionAuthQry"
                sprintf "%s for %A (%A) when projectingSquads (%i user/s) (%i squad/s) (%i draft/s) (%i projectee/s)" source connectionId userId userDic.Count squadDic.Count draftDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = userId |> Some }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingSquads -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> squadsProjectionDtoPlus userId |> Ok
                result |> logResult source (fun (squadsProjectionDto, currentDraftDto) -> sprintf "%i squad/s (%A)" squadsProjectionDto.SquadDtos.Length currentDraftDto |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingSquads state userDic squadDic draftDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Squads |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
            | DraftsRead draftsRead -> draftsRead |> OnDraftsRead |> agent.Post
            | UserEventWritten (_, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, _) -> (userId, userName) |> OnUserCreated |> agent.Post
                | _ -> ()
            | SquadEventWritten (rvn, userEvent) ->
                match userEvent with
                | SquadCreated _ -> () // note: no need to handle since cannot happen once SquadsRead
                | PlayerAdded (squadId, playerId, playerName, playerType) -> (squadId, rvn, playerId, playerName, playerType) |> OnPlayerAdded |> agent.Post
                | PlayerNameChanged (squadId, playerId, playerName) -> (squadId, rvn, playerId, playerName) |> OnPlayerNameChanged |> agent.Post
                | PlayerTypeChanged (squadId, playerId, playerType) -> (squadId, rvn, playerId, playerType) |> OnPlayerTypeChanged |> agent.Post
                | PlayerWithdrawn (squadId, playerId, dateWithdrawn) -> (squadId, rvn, playerId, dateWithdrawn) |> OnPlayerWithdrawn |> agent.Post
                | SquadEliminated squadId -> (squadId, rvn) |> OnSquadEliminated |> agent.Post
            | DraftEventWritten (_, draftEvent) -> draftEvent |> OnDraftEventWritten |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> SignOutConnections |> agent.Post
            | Disconnected connectionId -> connectionId |> RemoveConnection |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to SquadsRead | SquadEventWritten (subset) | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeSquadsProjectionUnauthQry (connectionId) =
        (fun reply -> (connectionId, reply) |> HandleInitializeSquadsProjectionUnauthQry) |> agent.PostAndAsyncReply
    member __.HandleInitializeSquadsProjectionAuthQry (token, connectionId, userId) =
        (fun reply -> (token, connectionId, userId, reply) |> HandleInitializeSquadsProjectionAuthQry) |> agent.PostAndAsyncReply

let squads = Squads ()
