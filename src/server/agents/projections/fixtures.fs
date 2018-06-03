module Aornota.Sweepstake2018.Server.Agents.Projections.Fixtures

(* Broadcasts: SendMsg
   Subscribes: SquadsRead
               FixturesRead
               // note: no need to handle SquadEventWritten since SquadCreated cannot happen once SquadsRead (and SquadEliminated is of no interest)
               FixtureEventWritten (ParticipantConfirmed only)
               Disconnected *)

open Aornota.Common.Revision

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.FixtureEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private FixtureInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnSquadsRead of squadsRead : SquadRead list
    | OnFixturesRead of fixturesRead : FixtureRead list
    | OnParticipantConfirmed of fixtureId : FixtureId * rvn : Rvn * role : Role * squadId : SquadId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeFixturesProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<FixturesProjectionDto, OtherError<string>>>
    
type private SquadDic = Dictionary<SquadId, SquadName>

type private Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipantDto : ParticipantDto ; AwayParticipantDto : ParticipantDto ; KickOff : DateTimeOffset }
type private FixtureDic = Dictionary<FixtureId, Fixture>

type private Projectee = { LastRvn : Rvn }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { FixtureDic : FixtureDic }

type private StateChangeType =
    | Initialization of fixtureDic : FixtureDic
    | FixtureChange of fixtureDic : FixtureDic * state : State

type private FixtureDtoDic = Dictionary<FixtureId, FixtureDto>

let private log category = (Projection Projection.Fixtures, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private fixtureDto (fixtureId, fixture:Fixture) =
    { FixtureId = fixtureId ; Rvn = fixture.Rvn ; Stage = fixture.Stage ; HomeParticipantDto = fixture.HomeParticipantDto ; AwayParticipantDto = fixture.AwayParticipantDto
      KickOff = fixture.KickOff }

let private fixtureDtoDic (fixtureDic:FixtureDic) =
    let fixtureDtoDic = FixtureDtoDic ()
    fixtureDic |> List.ofSeq |> List.iter (fun (KeyValue (fixtureId, fixture)) -> 
        let fixtureDto = (fixtureId, fixture) |> fixtureDto
        (fixtureDto.FixtureId, fixtureDto) |> fixtureDtoDic.Add)
    fixtureDtoDic

let private fixtureProjectionDto state =
    { FixtureDtos = state.FixtureDic |> List.ofSeq |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture) |> fixtureDto) }

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendFixtureDtoDelta (projecteeDic:ProjecteeDic) fixtureDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendFixtureDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, fixtureDtoDelta) |> FixturesDeltaMsg |> FixturesProjectionMsg |> ServerFixturesMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization fixtureDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { FixtureDic = FixtureDic fixtureDic }
        | FixtureChange (fixtureDic, state) ->
            let previousFixtureDtoDic = state.FixtureDic |> fixtureDtoDic
            let fixtureDtoDic = fixtureDic |> fixtureDtoDic
            let fixtureDtoDelta = fixtureDtoDic |> delta previousFixtureDtoDic
            if fixtureDtoDelta |> isEmpty |> not then
                sprintf "%s -> FixtureDto delta %A -> %i projectee/s" source fixtureDtoDelta projecteeDic.Count |> Info |> log
                fixtureDtoDelta |> sendFixtureDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with FixtureDic = FixtureDic fixtureDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private tryFindSquadName (squadDic:SquadDic) squadId =
    if squadId |> squadDic.ContainsKey |> not then None // note: silently ignore unknown squadId (should never happen)
    else squadDic.[squadId] |> Some

let private participantDto (squadDic:SquadDic) participant =
    match participant with
    | Confirmed squadId -> match squadId |> tryFindSquadName squadDic with | Some squadName -> (squadId, squadName) |> ConfirmedDto |> Some | None -> None
    | Unconfirmed unconfirmed -> unconfirmed |> UnconfirmedDto |> Some

let private ifAllRead source (squadsRead:(SquadRead list) option, fixturesRead:(FixtureRead list) option) =
    match squadsRead, fixturesRead with
    | Some squadsRead, Some fixturesRead ->
        let squadDic = SquadDic ()
        squadsRead |> List.iter (fun squadRead -> (squadRead.SquadId, squadRead.SquadName) |> squadDic.Add)
        let fixtureDic = FixtureDic ()
        fixturesRead
        |> List.iter (fun fixtureRead ->
            match fixtureRead.HomeParticipant |> participantDto squadDic, fixtureRead.AwayParticipant |> participantDto squadDic with
            | Some homeParticipantDto, Some awayParticipantDto ->
                let fixture = { Rvn = fixtureRead.Rvn ; Stage = fixtureRead.Stage ; HomeParticipantDto = homeParticipantDto ; AwayParticipantDto = awayParticipantDto ; KickOff = fixtureRead.KickOff }
                (fixtureRead.FixtureId, fixture) |> fixtureDic.Add
            | _ -> ())
        let projecteeDic = ProjecteeDic ()
        let state = fixtureDic |> Initialization |> updateState source projecteeDic
        (state, squadDic, fixtureDic, projecteeDic) |> Some
    | _ -> None

type Fixtures () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 users) (0 posts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnFixturesRead _ -> "OnFixturesRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead squadsRead fixturesRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead fixturesRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squads/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squads = squadsRead |> Some
                match (squads, fixturesRead) |> ifAllRead source with
                | Some (state, squadDic, fixtureDic, projecteeDic) ->
                    return! projectingFixtures state squadDic fixtureDic projecteeDic
                | None -> return! pendingAllRead squads fixturesRead
            | OnFixturesRead fixturesRead ->
                let source = "OnFixturesRead"
                sprintf "%s (%i fixture/s) when pendingAllRead" source fixturesRead.Length |> Info |> log
                let fixtures = fixturesRead |> Some
                match (squadsRead, fixtures) |> ifAllRead source with
                | Some (state, squadDic, fixtureDic, projecteeDic) ->
                    return! projectingFixtures state squadDic fixtureDic projecteeDic
                | None -> return! pendingAllRead squadsRead fixtures
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead fixturesRead
            | RemoveConnections _ -> "RemoveConnections when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead fixturesRead
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead fixturesRead }
        and projectingFixtures state squadDic fixtureDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state squadDic fixtureDic projecteeDic
            | OnSquadsRead _ -> "OnSquadsRead when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state squadDic fixtureDic projecteeDic
            | OnFixturesRead _ -> "OnFixturesRead when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state squadDic fixtureDic projecteeDic
            | OnParticipantConfirmed (fixtureId, rvn, role, squadId) ->
                let source = "OnParticipantConfirmed"
                sprintf "%s (%A %A) when projectingFixtures (%i squad/s) (%i fixture/s) (%i projectee/s)" source fixtureId rvn squadDic.Count fixtureDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if fixtureId |> fixtureDic.ContainsKey then // note: silently ignore unknown fixtureId (should never happen)
                        let fixture = fixtureDic.[fixtureId]
                        match role with
                        | Home ->
                            match (squadId |> Confirmed) |> participantDto squadDic with
                            | Some homeParticipantDto ->
                                fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; HomeParticipantDto = homeParticipantDto }
                                (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                            | None -> state
                        | Away ->
                            match (squadId |> Confirmed) |> participantDto squadDic with
                            | Some awayParticipantDto ->
                                fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; AwayParticipantDto = awayParticipantDto }
                                (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                            | None -> state
                    else state
                return! projectingFixtures state squadDic fixtureDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingFixtures (%i squad/s) (%i fixture/s) (%i projectee/s)" source connectionIds squadDic.Count fixtureDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingFixtures state squadDic fixtureDic projecteeDic
            | HandleInitializeFixturesProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeFixturesProjectionQry"
                sprintf "%s for %A when projectingFixtures (%i squad/s) (%i fixture/s) (%i projectee/s)" source connectionId squadDic.Count fixtureDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> fixtureProjectionDto |> Ok
                result |> logResult source (fun fixtureProjectionDto -> sprintf "%i fixture/s" fixtureProjectionDto.FixtureDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingFixtures state squadDic fixtureDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Fixtures |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
            | FixturesRead fixturesRead -> fixturesRead |> OnFixturesRead |> agent.Post
            | FixtureEventWritten (rvn, fixtureEvent) ->
                match fixtureEvent with
                | FixtureCreated _ -> () // note: no need to handle since cannot happen once FixturesRead
                | ParticipantConfirmed (fixtureId, role, squadId) -> (fixtureId, rvn, role, squadId) |> OnParticipantConfirmed |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten (subset) | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeFixturesProjectionQry connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeFixturesProjectionQry) |> agent.PostAndAsyncReply

let fixtures = Fixtures ()
