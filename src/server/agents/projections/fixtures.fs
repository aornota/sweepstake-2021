module Aornota.Sweepstake2018.Server.Agents.Projections.Fixtures

(* Broadcasts: SendMsg
   Subscribes: FixturesRead
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
    | OnFixturesRead of fixturesRead : FixtureRead list
    | OnParticipantConfirmed of fixtureId : FixtureId * rvn : Rvn * role : Role * squadId : SquadId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeFixturesProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<FixtureDto list, OtherError<string>>>
    
type private Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }
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

let private fixtureDto (fixtureId, fixture:Fixture) : FixtureDto =
    { FixtureId = fixtureId ; Rvn = fixture.Rvn ; Stage = fixture.Stage ; HomeParticipant = fixture.HomeParticipant ; AwayParticipant = fixture.AwayParticipant ; KickOff = fixture.KickOff }

let private fixtureDtoDic (fixtureDic:FixtureDic) =
    let fixtureDtoDic = FixtureDtoDic ()
    fixtureDic |> List.ofSeq |> List.iter (fun (KeyValue (fixtureId, fixture)) -> 
        let fixtureDto = (fixtureId, fixture) |> fixtureDto
        (fixtureDto.FixtureId, fixtureDto) |> fixtureDtoDic.Add)
    fixtureDtoDic

let private fixtureDtos state = state.FixtureDic |> List.ofSeq |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture) |> fixtureDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendFixtureDtoDelta (projecteeDic:ProjecteeDic) fixtureDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendFixtureDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, fixtureDtoDelta) |> FixturesDeltaMsg |> FixturesProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
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

type Fixtures () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingFixturesRead (0 users) (0 posts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingFixturesRead ()
            | OnFixturesRead _ -> "OnFixturesRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingFixturesRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingFixturesRead" |> IgnoredInput |> Agent |> log ; return! pendingFixturesRead ()
            | OnFixturesRead fixturesRead ->
                let source = "OnFixturesRead"
                sprintf "%s (%i fixture/s) when pendingFixturesRead" source fixturesRead.Length |> Info |> log
                let fixtureDic = FixtureDic ()
                fixturesRead |> List.iter (fun fixtureRead ->
                    let fixture = { Rvn = fixtureRead.Rvn ; Stage = fixtureRead.Stage ; HomeParticipant = fixtureRead.HomeParticipant ; AwayParticipant = fixtureRead.AwayParticipant
                                    KickOff = fixtureRead.KickOff }
                    (fixtureRead.FixtureId, fixture) |> fixtureDic.Add)
                let projecteeDic = ProjecteeDic ()
                let state = fixtureDic |> Initialization |> updateState source projecteeDic
                return! projectingFixtures state fixtureDic projecteeDic
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when pendingFixturesRead" |> IgnoredInput |> Agent |> log ; return! pendingFixturesRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingFixturesRead" |> IgnoredInput |> Agent |> log ; return! pendingFixturesRead ()
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when pendingFixturesRead" |> IgnoredInput |> Agent |> log ; return! pendingFixturesRead () }
        and projectingFixtures state fixtureDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state fixtureDic projecteeDic
            | OnFixturesRead _ -> "OnFixturesRead when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state fixtureDic projecteeDic
            | OnParticipantConfirmed (fixtureId, rvn, role, squadId) ->
                let source = "OnParticipantConfirmed"
                sprintf "%s (%A %A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source fixtureId rvn fixtureDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if fixtureId |> fixtureDic.ContainsKey then // note: silently ignore unknown fixtureId (should never happen)
                        let fixture = fixtureDic.[fixtureId]
                        match role with
                        | Home ->
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; HomeParticipant = squadId |> Confirmed }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                        | Away ->
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; AwayParticipant = squadId |> Confirmed }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                    else state
                return! projectingFixtures state fixtureDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source connectionIds fixtureDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingFixtures state fixtureDic projecteeDic
            | HandleInitializeFixturesProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeFixturesProjectionQry"
                sprintf "%s for %A when projectingFixtures (%i fixture/s) (%i projectee/s)" source connectionId fixtureDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> fixtureDtos |> Ok
                result |> logResult source (fun fixtureDtos -> sprintf "%i fixture/s" fixtureDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingFixtures state fixtureDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Fixtures |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | FixturesRead fixturesRead -> fixturesRead |> OnFixturesRead |> agent.Post
            | FixtureEventWritten (rvn, fixtureEvent) ->
                match fixtureEvent with
                | FixtureCreated _ -> () // note: no need to handle since cannot happen once FixturesRead
                | ParticipantConfirmed (fixtureId, role, squadId) -> (fixtureId, rvn, role, squadId) |> OnParticipantConfirmed |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to FixturesRead | FixtureEventWritten | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeFixturesProjectionQryAsync connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeFixturesProjectionQry) |> agent.PostAndAsyncReply

let fixtures = Fixtures ()
