module Aornota.Sweepstake2018.UI.Pages.Drafts.State

open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Drafts.Common
open Aornota.Sweepstake2018.UI.Shared

open Elmish

let initialize () =
    let pickOverridesState = { PickOverrides = [] ; PendingRvn = None }
    { PickOverridesState = pickOverridesState }, Cmd.none

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleServerDraftsMsg serverDraftsMsg (squadDic:SquadDic) state : State * Cmd<Input> =
    match serverDraftsMsg with
    | ServerDraftsMsg.RemoveFromDraftCmdResult (Ok userDraftPick) ->
        let pickOverrides = state.PickOverridesState.PickOverrides
        if pickOverrides |> List.exists (fun pickOverride -> pickOverride.UserDraftPick = userDraftPick && pickOverride |> isRemoving) then
            state, sprintf "<strong>%s</strong> has been removed from draft" (userDraftPick |> userDraftPickText squadDic) |> successToastCmd
        else state, shouldNeverHappenCmd (sprintf "Unexpected ServerDraftsMsg.RemoveFromDraftCmdResult Ok when not Removing %A" userDraftPick)
    | ServerDraftsMsg.RemoveFromDraftCmdResult (Error (userDraftPick, error)) ->
        let pickOverridesState = state.PickOverridesState
        let pickOverrides = pickOverridesState.PickOverrides
        if pickOverrides |> List.exists (fun pickOverride -> pickOverride.UserDraftPick = userDraftPick && pickOverride |> isRemoving) then
            let errorText = ifDebug (sprintf "ServerDraftsMsg.RemoveFromDraftCmdResult error -> %A" error) (error |> cmdErrorText)
            let errorCmd = errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
            let errorToastCmd = sprintf "Unable to remove <strong>%s</strong> from draft" (userDraftPick |> userDraftPickText squadDic) |> errorToastCmd
            let pickOverrides = pickOverrides |> List.filter (fun pickOverride -> pickOverride.UserDraftPick <> userDraftPick || pickOverride |> isRemoving |> not)
            let pickOverridesState = { pickOverridesState with PickOverrides = pickOverrides }
            { state with PickOverridesState = pickOverridesState }, Cmd.batch [ errorCmd ; errorToastCmd ]
        else state, shouldNeverHappenCmd (sprintf "Unexpected ServerDraftsMsg.RemoveFromDraftCmdResult Error when not Removing %A" userDraftPick)

let transition input (authUser:AuthUser option) (squadsProjection:Projection<_ * SquadDic>) (currentUserDraftDto:CurrentUserDraftDto option) state =
    let state, cmd, isUserNonApiActivity =
        match input, squadsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerDraftsMsg serverDraftsMsg, Ready (_, squadDic) ->
            let state, cmd = state |> handleServerDraftsMsg serverDraftsMsg squadDic
            state, cmd, false
        | RemoveFromDraft (draftId, userDraftPick), Ready _ ->
            match authUser with
            | Some authUser ->
                let isPicked =
                    match currentUserDraftDto with
                    | Some currentUserDraftDto -> currentUserDraftDto.UserDraftPickDtos |> List.exists (fun userDraftPickDto -> userDraftPickDto.UserDraftPick = userDraftPick)
                    | None -> false
                if isPicked then
                    let pickOverridesState = state.PickOverridesState
                    let pickOverrides = pickOverridesState.PickOverrides

                    // TODO-NEXT: What if overridden for another reason?...

                    if pickOverrides |> List.exists (fun pickOverride -> pickOverride.UserDraftPick = userDraftPick) |> not then
                        let pickOverride = { UserDraftPick = userDraftPick ; PickOverrideStatus = Removing }
                        let currentRvn =
                            match currentUserDraftDto with
                            | Some currentUserDraftDto ->
                                let (Rvn currentRvn) = currentUserDraftDto.Rvn
                                match pickOverridesState.PendingRvn with | Some (Rvn rvn) when rvn > currentRvn -> Rvn rvn | Some _ | None -> Rvn currentRvn
                            | None -> match pickOverridesState.PendingRvn with | Some rvn -> rvn | None -> initialRvn
                        let cmd = (authUser.UserId, draftId, currentRvn, userDraftPick) |> UiAuthDraftsMsg.RemoveFromDraftCmd |> UiAuthDraftsMsg |> SendUiAuthMsg |> Cmd.ofMsg
                        let pickOverridesState = { pickOverridesState with PickOverrides = pickOverride :: pickOverrides ; PendingRvn = currentRvn |> incrementRvn |> Some }
                        { state with PickOverridesState = pickOverridesState }, cmd, true
                    else state, UNEXPECTED_ERROR |> errorToastCmd, true
                else state, UNEXPECTED_ERROR |> errorToastCmd, true
            | None -> // note: should never happen
                state, Cmd.none, false
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" squadsProjection input), false
    state, cmd, isUserNonApiActivity
