module Aornota.Sweepstake2018.UI.Pages.Fixtures.State

open Aornota.Common.Delta
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Fixtures.Common

open Elmish

let initialize currentFixtureFilter : State * Cmd<Input> =
    let currentFixtureFilter = match currentFixtureFilter with | Some currentFixtureFilter -> currentFixtureFilter | None -> AllFixtures
    let state = { ProjectionState = Initializing currentFixtureFilter }
    let cmd = InitializeFixturesProjectionQry |> UiUnauthFixturesMsg |> SendUiUnauthMsg |> Cmd.ofMsg
    state, cmd

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private fixture (fixtureDto:FixtureDto) =
    { Rvn = fixtureDto.Rvn ; Stage = fixtureDto.Stage ; HomeParticipantDto = fixtureDto.HomeParticipantDto ; AwayParticipantDto = fixtureDto.AwayParticipantDto ; KickOff = fixtureDto.KickOff }

let private fixturesProjection (fixturesProjectionDto:FixturesProjectionDto) =
    let fixtureDic = FixtureDic ()
    fixturesProjectionDto.FixtureDtos |> List.iter (fun fixtureDto ->
        let fixtureId = fixtureDto.FixtureId
        if fixtureId |> fixtureDic.ContainsKey |> not then // note: silently ignore duplicate FixtureIds (should never happen)
            (fixtureId, fixtureDto |> fixture) |> fixtureDic.Add)
    { Rvn = initialRvn ; FixtureDic = fixtureDic }

let private applyFixturesDelta currentRvn deltaRvn (delta:Delta<FixtureId, FixtureDto>) (fixtureDic:FixtureDic) =
    let fixtureDic = FixtureDic fixtureDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (fixtureId, fixtureDto) -> if fixtureId |> fixtureDic.ContainsKey then (fixtureId, fixtureDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (fixtureId, fixtureDto) -> (fixtureId, fixtureDto |> fixture) |> fixtureDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (fixtureId, fixtureDto) -> if fixtureId |> fixtureDic.ContainsKey |> not then (fixtureId, fixtureDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (fixtureId, fixtureDto) -> fixtureDic.[fixtureId] <- (fixtureDto |> fixture)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun fixtureId -> if fixtureId |> fixtureDic.ContainsKey |> not then fixtureId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fixtureDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> fixtureDic |> Ok)

let private handleServerFixturesMsg serverFixturesMsg state : State * Cmd<Input> =
    match serverFixturesMsg, state.ProjectionState with
    | InitializeFixturesProjectionQryResult (Ok fixturesProjectionDto), Initializing currentFixtureFilter ->
        let fixturesProjection = fixturesProjectionDto |> fixturesProjection
        let activeState = {
            FixturesProjection = fixturesProjection
            CurrentFixtureFilter = currentFixtureFilter }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeFixturesProjectionQryResult (Error (OtherError errorText)), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | FixturesProjectionMsg (FixturesDeltaMsg (deltaRvn, fixtureDtoDelta)), Active activeState ->
        let fixturesProjection = activeState.FixturesProjection
        match fixturesProjection.FixtureDic |> applyFixturesDelta fixturesProjection.Rvn deltaRvn fixtureDtoDelta with
        | Ok fixtureDic ->
            let fixturesProjection = { fixturesProjection with Rvn = deltaRvn ; FixtureDic = fixtureDic }
            let activeState = { activeState with FixturesProjection = fixturesProjection }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" fixtureDtoDelta fixturesProjection.FixtureDic error)
            let state, cmd = initialize (activeState.CurrentFixtureFilter |> Some)
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | FixturesProjectionMsg _, _ -> // note: silently ignore SquadsProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerFixturesMsg when %A -> %A" state.ProjectionState serverFixturesMsg)

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiUnauthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerFixturesMsg serverFixturesMsg, _ ->
            let state, cmd = state |> handleServerFixturesMsg serverFixturesMsg
            state, cmd, false
        | ShowAllFixtures, Active activeState ->
            let activeState = { activeState with CurrentFixtureFilter = AllFixtures }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowGroupFixtures group, Active activeState ->
            let group =
                match group with
                | Some group -> group
                | None -> match activeState.CurrentFixtureFilter with | GroupFixtures group -> group | AllFixtures | KnockoutFixtures -> GroupA
            let activeState = { activeState with CurrentFixtureFilter = group |> GroupFixtures }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowKnockoutFixtures, Active activeState ->
            let activeState = { activeState with CurrentFixtureFilter = KnockoutFixtures }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowConfirmParticipantModal (_fixtureId, _role), Active _activeState -> // note: no need to check for unknown squadId (should never happen)
            // TODO-SOON?...
            state, "Confirm participant functionality is not yet implemented" |> warningToastCmd, true
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
