module Aornota.Sweepstake2018.Server.Agents.Entities.Fixtures

(* Broadcasts: FixturesRead
   Subscribes: FixturesEventsRead *)

open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers

open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Events.FixtureEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private FixturesInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | OnFixturesEventsRead of fixturesEvents : (FixtureId * (Rvn * FixtureEvent) list) list
    | HandleCreateFixtureCmd of token : CreateFixtureToken * auditUserId : UserId * fixtureId : FixtureId * stage : Stage * homeParticipant : Participant * awayParticipant : Participant
        * kickOff : DateTimeOffset * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleConfirmParticipantCmd of token : ConfirmFixtureToken * auditUserId : UserId * fixtureId : FixtureId * currentRvn : Rvn * role : Role * squadId : SquadId
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>

type private Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }
type private FixtureDic = Dictionary<FixtureId, Fixture>

let private log category = (Entity Entity.Fixtures, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private applyFixtureEvent source idAndFixtureResult (nextRvn, fixtureEvent:FixtureEvent) =
    let otherError errorText = otherError (sprintf "%s#applyFixtureEvent" source) errorText
    match idAndFixtureResult, fixtureEvent with
    | Ok (fixtureId, _), _ when fixtureId <> fixtureEvent.FixtureId -> // note: should never happen
        ifDebug (sprintf "FixtureId mismatch for %A -> %A" fixtureId fixtureEvent) UNEXPECTED_ERROR |> otherError
    | Ok (fixtureId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" fixtureId nextRvn fixtureEvent) UNEXPECTED_ERROR |> otherError
    | Ok (fixtureId, Some fixture), _ when validateNextRvn (Some fixture.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" fixtureId fixture.Rvn nextRvn fixtureEvent) UNEXPECTED_ERROR |> otherError
    | Ok (fixtureId, None), FixtureCreated (_, stage, homeParticipant, awayParticipant, kickOff) ->
        (fixtureId, { Rvn = nextRvn ; Stage = stage ; HomeParticipant = homeParticipant ; AwayParticipant = awayParticipant ; KickOff = kickOff } |> Some) |> Ok
    | Ok (fixtureId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial FixtureEvent for %A -> %A" fixtureId fixtureEvent) UNEXPECTED_ERROR |> otherError
    | Ok (fixtureId, Some fixture), FixtureCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial FixtureEvent for %A (%A) -> %A" fixtureId fixture fixtureEvent) UNEXPECTED_ERROR |> otherError
    | Ok (fixtureId, Some fixture), ParticipantConfirmed (_, role, squadId) ->
        let fixture =
            match role with
            | Home -> { fixture with Rvn = nextRvn ; HomeParticipant = squadId |> Confirmed }
            | Away -> { fixture with Rvn = nextRvn ; AwayParticipant = squadId |> Confirmed }
        (fixtureId, fixture |> Some) |> Ok
    | Error error, _ -> error |> Error

let private initializeFixtures source (fixturesEvents:(FixtureId * (Rvn * FixtureEvent) list) list) =
    let source = sprintf "%s#initializeFixtures" source
    let fixtureDic = FixtureDic ()
    let results =
        fixturesEvents
        |> List.map (fun (fixtureId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndFixtureResult (rvn, fixtureEvent) -> applyFixtureEvent source idAndFixtureResult (rvn, fixtureEvent)) (Ok (fixtureId, None))
            | [] -> ifDebug (sprintf "No FixtureEvents for %A" fixtureId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndFixtureResult -> match idAndFixtureResult with | Ok (fixtureId, Some fixture) -> (fixtureId, fixture) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (fixtureId, fixture) -> fixtureDic.Add (fixtureId, fixture))
    let errors =
        results
        |> List.choose (fun idAndFixtureResult ->
            match idAndFixtureResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyFixtureEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    fixtureDic, errors

let private updateFixture fixtureId fixture (fixtureDic:FixtureDic) = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] <- fixture

let private tryFindFixture fixtureId onError (fixtureDic:FixtureDic) =
    if fixtureId |> fixtureDic.ContainsKey then (fixtureId, fixtureDic.[fixtureId]) |> Ok else ifDebug (sprintf "%A does not exist" fixtureId) UNEXPECTED_ERROR |> onError

let private tryApplyFixtureEvent source fixtureId fixture nextRvn fixtureEvent =
    match applyFixtureEvent source (Ok (fixtureId, fixture)) (nextRvn, fixtureEvent) with
    | Ok (_, Some post) -> (post, nextRvn, fixtureEvent) |> Ok
    | Ok (_, None) -> ifDebug "applyFixtureEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteFixtureEventAsync auditUserId rvn fixtureEvent (fixture:Fixture) = async {
    let! result = (auditUserId, rvn, fixtureEvent) |> persistence.WriteFixtureEventAsync
    return match result with | Ok _ -> (fixtureEvent.FixtureId, fixture) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

type Fixtures () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingOnFixturesEventsRead" |> Info |> log
                () |> reply.Reply
                return! pendingOnFixturesEventsRead ()
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnFixturesEventsRead _ -> "OnFixturesEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateFixtureCmd _ -> "HandleCreateFixtureCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleConfirmParticipantCmd _ -> "HandleConfirmParticipantCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnFixturesEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingOnFixturesEventsRead ()
            | Start _ -> "Start when pendingOnFixturesEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnFixturesEventsRead ()
            | Reset _ -> "Reset when pendingOnFixturesEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnFixturesEventsRead ()
            | OnFixturesEventsRead fixturesEvents ->
                let source = "OnFixturesEventsRead"
                let fixtures, errors = initializeFixtures source fixturesEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i fixture/s) when pendingOnFixturesEventsRead -> managingFixtures (%i fixture/s)" source fixturesEvents.Length fixtures.Count |> Info |> log
                let fixturesRead =
                    fixtures
                    |> List.ofSeq
                    |> List.map (fun (KeyValue (fixtureId, fixture)) ->
                        { FixtureId = fixtureId ; Rvn = fixture.Rvn ; Stage = fixture.Stage ; HomeParticipant = fixture.HomeParticipant ; AwayParticipant = fixture.AwayParticipant ; KickOff = fixture.KickOff })
                fixturesRead |> FixturesRead |> broadcaster.Broadcast       
                return! managingFixtures fixtures
            | HandleCreateFixtureCmd _ -> "HandleCreateFixtureCmd when pendingOnFixturesEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnFixturesEventsRead ()
            | HandleConfirmParticipantCmd _ -> "HandleConfirmParticipantCmd when pendingOnFixturesEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnFixturesEventsRead () }
        and managingFixtures fixtureDic = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingFixtures fixtureDic
            | Start _ -> sprintf "Start when managingFixtures (%i fixture/s)" fixtureDic.Count |> IgnoredInput |> Agent |> log ; return! managingFixtures fixtureDic
            | Reset reply ->
                sprintf "Reset when managingFixtures (%i fixture/s) -> pendingOnUsersEventsRead" fixtureDic.Count |> Info |> log
                () |> reply.Reply
                return! pendingOnFixturesEventsRead ()
            | OnFixturesEventsRead _ -> sprintf "OnFixturesEventsRead when managingFixtures (%i fixture/s)" fixtureDic.Count |> IgnoredInput |> Agent |> log ; return! managingFixtures fixtureDic
            | HandleCreateFixtureCmd (_, auditUserId, fixtureId, stage, homeParticipant, awayParticipant, kickOff, reply) ->
                let source = "HandleCreateFixtureCmd"
                sprintf "%s for %A (%A %A %A) when managingFixtures (%i fixture/s)" source fixtureId stage homeParticipant awayParticipant fixtureDic.Count |> Verbose |> log
                let result =
                    if fixtureId |> fixtureDic.ContainsKey |> not then () |> Ok else ifDebug (sprintf "%A already exists" fixtureId) UNEXPECTED_ERROR |> otherCmdError source
                    |> Result.bind (fun _ -> (fixtureId, stage, homeParticipant, awayParticipant, kickOff) |> FixtureCreated |> tryApplyFixtureEvent source fixtureId None initialRvn)
                let! result = match result with | Ok (fixture, rvn, fixtureEvent) -> tryWriteFixtureEventAsync auditUserId rvn fixtureEvent fixture | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (fixtureId, fixture) -> sprintf "Audit%A %A %A" auditUserId fixtureId fixture |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (fixtureId, fixture) -> (fixtureId, fixture) |> fixtureDic.Add | Error _ -> ()
                return! managingFixtures fixtureDic
            | HandleConfirmParticipantCmd (_, auditUserId, fixtureId, currentRvn, role, squadId, reply) ->
                let source = "HandleConfirmParticipantCmd"
                sprintf "%s for %A (%A %A %A) when managingFixtures (%i fixture/s)" source fixtureId currentRvn role squadId fixtureDic.Count |> Verbose |> log
                let result =
                    fixtureDic |> tryFindFixture fixtureId (otherCmdError source)
                    |> Result.bind (fun (fixtureId, fixture) ->
                        let errorText =
                            match role with
                            | Home ->
                                match fixture.HomeParticipant with
                                | Confirmed squadId -> ifDebug (sprintf "HomeParticipant has already been confirmed (%A)" squadId) UNEXPECTED_ERROR |> Some
                                | Unconfirmed _ -> None
                            | Away ->
                                match fixture.AwayParticipant with
                                | Confirmed squadId -> ifDebug (sprintf "AwayParticipant has already been confirmed (%A)" squadId) UNEXPECTED_ERROR |> Some
                                | Unconfirmed _ -> None
                        match errorText with | Some errorText -> errorText |> otherCmdError source | None -> (fixtureId, fixture) |> Ok)
                    |> Result.bind (fun (fixtureId, fixture) -> (fixtureId, role, squadId) |> ParticipantConfirmed |> tryApplyFixtureEvent source fixtureId (Some fixture) (incrementRvn currentRvn))
                let! result = match result with | Ok (fixture, rvn, fixtureEvent) -> tryWriteFixtureEventAsync auditUserId rvn fixtureEvent fixture | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (fixtureId, fixture) -> Some (sprintf "Audit%A %A %A" auditUserId fixtureId fixture)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (fixtureId, fixture) -> fixtureDic |> updateFixture fixtureId fixture | Error _ -> ()
                return! managingFixtures fixtureDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Fixtures |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in FixturesEventWritten events (since Fixtures agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event -> match event with | FixturesEventsRead fixturesEvents -> fixturesEvents |> self.OnFixturesEventsRead | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to FixturesEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.OnFixturesEventsRead fixturesEvents = fixturesEvents |> OnFixturesEventsRead |> agent.Post
    member __.HandleCreateFixtureCmdAsync (token, auditUserId, fixtureId, stage, homeParticipant, awayParticipant, kickOff) =
        (fun reply -> (token, auditUserId, fixtureId, stage, homeParticipant, awayParticipant, kickOff, reply) |> HandleCreateFixtureCmd) |> agent.PostAndAsyncReply
    member __.HandleConfirmParticipantCmdAsync (token, auditUserId, fixtureId, currentRvn, role, squadId) =
        (fun reply -> (token, auditUserId, fixtureId, currentRvn, role, squadId, reply) |> HandleConfirmParticipantCmd) |> agent.PostAndAsyncReply

let fixtures = Fixtures ()
