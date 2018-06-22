module Aornota.Sweepstake2018.UI.Pages.Fixtures.State

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.UI.Pages.Fixtures.Common
open Aornota.Sweepstake2018.UI.Shared

open Elmish

let initialize currentFixturesFilter : State * Cmd<Input> =
    let currentFixturesFilter = match currentFixturesFilter with | Some currentFixturesFilter -> currentFixturesFilter | None -> AllFixtures
    { CurrentFixturesFilter = currentFixturesFilter ; LastGroup = None ; ConfirmParticipantState = None }, Cmd.none

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleServerFixturesMsg serverFixturesMsg state : State * Cmd<Input> =
    match serverFixturesMsg with
    | ConfirmParticipantCmdResult _result ->
        // TODO-SOON-ISH: Or just hack data?...
        state, Cmd.none
    | AddMatchEventCmdResult _result ->
        // TODO-NEXT...
        state, Cmd.none
    | RemoveMatchEventCmdResult _result ->
        // TODO-NEXT...
        state, Cmd.none

let private updateLast state = match state.CurrentFixturesFilter with | AllFixtures -> state | GroupFixtures group -> { state with LastGroup = group } | KnockoutFixtures -> state

let transition input (fixturesProjection:Projection<_ * FixtureDic>) state =
    let state, cmd, isUserNonApiActivity =
        match input, fixturesProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerFixturesMsg serverFixturesMsg, Ready _ ->
            let state, cmd = state |> handleServerFixturesMsg serverFixturesMsg
            state, cmd, false
        | ShowAllFixtures, Ready _ ->
            let state = state |> updateLast
            { state with CurrentFixturesFilter = AllFixtures }, Cmd.none, true
        | ShowGroupFixtures group, Ready _ ->
            let state = state |> updateLast
            let state =
                match state.CurrentFixturesFilter, group with
                | GroupFixtures (Some _), None -> state
                | _, None -> { state with CurrentFixturesFilter = state.LastGroup |> GroupFixtures }
                | _ -> { state with CurrentFixturesFilter = group |> GroupFixtures }
            state, Cmd.none, true
        | ShowKnockoutFixtures, Ready _ ->
            let state = state |> updateLast
            { state with CurrentFixturesFilter = KnockoutFixtures }, Cmd.none, true
        | ShowConfirmParticipantModal (_fixtureId, _role), Ready _ -> // note: no need to check for unknown fixtureId (should never happen)
            // TODO-SOON-ISH: Or just hack data?...
            state, "Confirm participant functionality is not yet implemented" |> warningToastCmd, true
        | ConfirmParticipantInput _confirmParticipantInput, Ready (_, _fixtureDic) ->
            // TODO-SOON-ISH: Or just hack data?...
            state, Cmd.none, false
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" fixturesProjection input), false
    state, cmd, isUserNonApiActivity
