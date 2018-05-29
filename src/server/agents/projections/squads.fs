module Aornota.Sweepstake2018.Server.Agents.Projections.Squads

// TODO-SOON: Will also need User/s details (e.g. to map UserId to UserName for "picked by")?...

(* Broadcasts: SendMsg
   Subscribes: SquadsRead
               SquadEventWritten (PlayerAdded | TODO:PlayerNameChanged? | TODO:PlayerTypeChanged? | TODO:PlayerWithdrawn? | TODO:SquadEliminated?)
               TODO:ConnectionsSignedOut?
               Disconnected *)

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private ChatInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnSquadsRead of squadsRead : SquadRead list
    | OnPlayerAdded of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    // TODO-SOON... | OnPlayerNameChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    // TODO-SOON... | OnPlayerTypeChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    // TODO-SOON... | OnPlayerWithdrawn of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId // TODO-NMB-MEDIUM: dateWithdrawn?...
    // TODO-SOON... | OnSquadEliminated of squadId : SquadId * squadRvn : Rvn
    | RemoveConnection of connectionId : ConnectionId
    | HandleInitializeSquadsProjectionUnauthQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<SquadsProjectionDto, OtherError<string>>>
    | HandleInitializeSquadsProjectionAuthQry of token : SquadsProjectionAuthQryToken * connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<SquadsProjectionDto, AuthQryError<string>>>

type private Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; Withdrawn : bool } // TODO-NMB-MEDIUM: dateWithdrawn? draftedBy? pickedBy? score?...
type private PlayerDic = Dictionary<PlayerId, Player>

type private Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic }
type private SquadDic = Dictionary<SquadId, Squad>

type private Projectee = { LastRvn : Rvn ; UserId : UserId option }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { SquadDic : SquadDic }

type private StateChangeType =
    | Initialization of squadDic : SquadDic
    // TODO-SOON... | Squads of squadDic : SquadDic * state : State
    | Players of squadId : SquadId * squadRvn : Rvn * playerDic : PlayerDic * state : State

type private PlayerDtoDic = Dictionary<PlayerId, PlayerDto>

let private log category = (Projection Squads, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private playerDto (playerId, player:Player) : PlayerDto = { PlayerId = playerId ; PlayerName = player.PlayerName ; PlayerType = player.PlayerType ; Withdrawn = player.Withdrawn }

let private playerDtoDic (playerDic:PlayerDic) =
    let playerDtoDic = PlayerDtoDic ()
    playerDic |> List.ofSeq |> List.iter (fun (KeyValue (playerId, player)) ->
        let playerDto = (playerId, player) |> playerDto
        (playerDto.PlayerId, playerDto) |> playerDtoDic.Add)
    playerDtoDic

let private squadDto (squadId, squad:Squad) =
    let playerDtos = squad.PlayerDic |> List.ofSeq |> List.map (fun (KeyValue (playerId, player)) -> (playerId, player) |> playerDto)
    let squadOnlyDto =
        { SquadId = squadId ; Rvn = squad.Rvn ; SquadName = squad.SquadName ; Group = squad.Group ; Seeding = squad.Seeding ; CoachName = squad.CoachName ; Eliminated = squad.Eliminated }
    { SquadOnlyDto = squadOnlyDto ; PlayerDtos = playerDtos }

let private squadsProjectionDto (_userId:UserId option) (state:State) =
    let squadDtos = state.SquadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> (squadId, squad) |> squadDto)
    { SquadDtos = squadDtos }

let private sendPlayerDtoDelta squadId squadRvn (projecteeDic:ProjecteeDic) playerDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendPlayerDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, squadId, squadRvn, playerDtoDelta) |> PlayersDeltaMsg |> SquadsProjectionMsg |> ServerSquadsMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization squadDic ->
            sprintf "%s -> initial" source |> Info |> log
            let copiedSquadDic = SquadDic ()
            squadDic |> List.ofSeq |> List.iter (fun (KeyValue (squadId, squad)) ->
                let squad = { squad with PlayerDic = PlayerDic squad.PlayerDic }
                (squadId, squad) |> copiedSquadDic.Add)
            { SquadDic = copiedSquadDic }
        | Players (squadId, squadRvn, playerDic, state) ->
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
    newState

type Squads () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnSquadsRead (0 squads) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingOnSquadsRead ()
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerAdded _ -> "OnPlayerAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeSquadsProjectionUnauthQry _ -> "HandleInitializeSquadsProjectionUnauthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeSquadsProjectionAuthQry _ -> "HandleInitializeSquadsProjectionAuthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnSquadsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnSquadsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsRead ()
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squads/s) when pendingOnSquadsRead" source squadsRead.Length |> Info |> log
                let squadDic = SquadDic ()
                squadsRead |> List.iter (fun squadRead ->
                    let playerDic = PlayerDic ()
                    squadRead.PlayersRead |> List.iter (fun playerRead ->
                        let player = { PlayerName = playerRead.PlayerName ; PlayerType = playerRead.PlayerType ; Withdrawn = playerRead.Withdrawn }
                        (playerRead.PlayerId, player) |> playerDic.Add)
                    let squad = {
                        Rvn = squadRead.Rvn ; SquadName = squadRead.SquadName ; Group = squadRead.Group ; Seeding = squadRead.Seeding ; CoachName = squadRead.CoachName
                        Eliminated = squadRead.Eliminated ; PlayerDic = playerDic }
                    (squadRead.SquadId, squad) |> squadDic.Add)
                let projecteeDic = ProjecteeDic ()
                let state = squadDic |> Initialization |> updateState source projecteeDic
                return! projectingSquads (state, squadDic, projecteeDic)
            | OnPlayerAdded _ -> "OnPlayerAdded when pendingOnSquadsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsRead ()
            | RemoveConnection _ -> "RemoveConnection when pendingOnSquadsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsRead ()
            | HandleInitializeSquadsProjectionUnauthQry _ -> "HandleInitializeSquadsProjectionUnauthQry when pendingOnSquadsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsRead ()
            | HandleInitializeSquadsProjectionAuthQry _ -> "HandleInitializeSquadsProjectionAuthQry when pendingOnUspendingOnSquadsReadersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsRead () }
        and projectingSquads (state, squadDic, projecteeDic) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads (state, squadDic, projecteeDic)
            | OnSquadsRead _ -> "OnSquadsRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads (state, squadDic, projecteeDic)
            | OnPlayerAdded (squadId, squadRvn, playerId, playerName, playerType) ->
                let source = "OnPlayerAdded"
                sprintf "%s (%A %A %A %A %A) when projectingSquads (%i squads/s) (%i projectee/s)" source squadId squadRvn playerId playerName playerType squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey |> not then // note: silently ignore already-known playerId (should never happen)
                            (playerId, { PlayerName = playerName ; PlayerType = playerType ; Withdrawn = false }) |> playerDic.Add
                            (squadId, squadRvn, playerDic, state) |> Players |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads (state, squadDic, projecteeDic)
            | RemoveConnection connectionId ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingSquads (%i squad/s) (%i projectee/s)" source connectionId squadDic.Count projecteeDic.Count |> Info |> log
                if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingSquads (state, squadDic, projecteeDic)
            | HandleInitializeSquadsProjectionUnauthQry (connectionId, reply) ->
                let source = "HandleInitializeSquadsProjectionUnauthQry"
                sprintf "%s for %A when projectingSquads (%i squad/s) (%i projectee/s)" source connectionId squadDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = None }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingSquads -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> squadsProjectionDto None |> Ok
                result |> logResult source (fun squadsProjectionDto -> sprintf "%i squad/s" squadsProjectionDto.SquadDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingSquads (state, squadDic, projecteeDic)
            | HandleInitializeSquadsProjectionAuthQry (_, connectionId, userId, reply) ->
                let source = "HandleInitializeSquadsProjectionAuthQry"
                sprintf "%s for %A (%A) when projectingSquads (%i squad/s) (%i projectee/s)" source connectionId userId squadDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = userId |> Some }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingSquads -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> squadsProjectionDto (userId |> Some) |> Ok
                result |> logResult source (fun squadsProjectionDto -> sprintf "%i squad/s" squadsProjectionDto.SquadDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingSquads (state, squadDic, projecteeDic) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Squads |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
            | SquadEventWritten (rvn, userEvent) ->
                match userEvent with
                | PlayerAdded (squadId, playerId, playerName, playerType) -> (squadId, rvn, playerId, playerName, playerType) |> OnPlayerAdded |> agent.Post
                | _ -> ()
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
