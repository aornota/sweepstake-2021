module Aornota.Sweepstake2018.UI.Pages.Squads.State

open Aornota.Common.Delta
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Squads.Common

open System

open Elmish

let initialize () : State * Cmd<Input> =
    let state = { ProjectionState = Initializing }
    let cmd = InitializeSquadsProjectionQry |> UiUnauthSquadsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
    state, cmd

let private defaultAddPlayerState squadId playerType addPlayerStatus resultRvn = {
    SquadId = squadId
    NewPlayerId = PlayerId.Create ()
    NewPlayerNameText = String.Empty
    NewPlayerNameErrorText = None
    NewPlayerType = playerType
    NewPlayerTypeErrorText = None
    AddPlayerStatus = addPlayerStatus
    ResultRvn = resultRvn }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

// TODO-NEXT: Deltas (&c.)...

let private handleServerSquadsMsg serverSquadsMsg authUser state : State * Cmd<Input> =
    let cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
    match serverSquadsMsg, state.ProjectionState with
    | InitializeSquadsProjectionQryResult (Ok tempNMB), Initializing ->
        // TODO-NEXT: Map Dto/s to SquadsProjection | Set CurrentSquadId? | ...
        let activeState = {
            SquadsProjection = { Rvn = initialRvn } // TEMP-NMB...
            CurrentGroup = GroupA |> Some
            CurrentSquadId = None
            AddPlayerState = None }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeSquadsProjectionQryResult (Error (OtherError errorText)), Initializing ->
        { state with ProjectionState = InitializationFailed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | AddPlayerCmdResult (Ok _), Active activeState ->
        // TODO-NEXT: Check AddPlayerState->AddPlayerStatus is AddPlayerPending? | "Reset" AddPlayerState (but preserve SquadId | NewPlayerType)...
        state, Cmd.none
    | AddPlayerCmdResult (Error error), Active activeState ->
        // TODO-NEXT: Check AddPlayerState->AddPlayerStatus is AddPlayerPending? | Update AddPlayerState, i.e. AddPlayerFailed errorText (cf. Program.State.handleSignInResult)...
        state, Cmd.none
    (*match serverSquadsMsg, state.ProjectionState with
    | SquadsProjectionMsg (XyzDeltaMsg (deltaRvn, chatUserDtoDelta)), Active activeState ->
        let chatProjection = activeState.ChatProjection
        match chatProjection.ChatUserDic |> applyChatUserDelta chatProjection.Rvn deltaRvn chatUserDtoDelta with
        | Ok chatUserDic ->
            let chatProjection = { chatProjection with Rvn = deltaRvn ; ChatUserDic = chatUserDic }
            let activeState = { activeState with ChatProjection = chatProjection }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatUserDtoDelta chatProjection.ChatUserDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | SquadsProjectionMsg _, _ -> // note: silently ignore SquadsProjectionMsg if not Active
        state, Cmd.none*)
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerSquadsMsg when %A -> %A" state.ProjectionState serverSquadsMsg)

let transition input authUser state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiUnauthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerSquadsMsg serverSquadsMsg, _ ->
            let state, cmd = state |> handleServerSquadsMsg serverSquadsMsg authUser
            state, cmd, false
        | ShowGroup group, Active activeState ->
            let activeState = { activeState with CurrentGroup = group |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
