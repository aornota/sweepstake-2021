module Aornota.Sweepstake2018.UI.Pages.Scores.State

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen

open Aornota.Sweepstake2018.UI.Pages.Scores.Common
open Aornota.Sweepstake2018.UI.Shared

open Elmish

let initialize currentUserId : State * Cmd<Input> = { CurrentUserId = currentUserId }, Cmd.none

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let transition input (squadsProjection:Projection<_ * SquadDic>) state =
    let state, cmd, isUserNonApiActivity =
        match input, squadsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ShowUser userId, Ready _ -> // note: no need to check for unknown userId (should never happen)
            { state with CurrentUserId = userId |> Some }, Cmd.none, true
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" squadsProjection input), false
    state, cmd, isUserNonApiActivity
