module Aornota.Sweepstake2018.UI.Pages.Squads.State

open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Squads.Common
open Aornota.Sweepstake2018.UI.Shared

open System

open Elmish

let initialize currentSquadId : State * Cmd<Input> =
    { CurrentDraftPicks = [] ; CurrentSquadId = currentSquadId ; AddPlayersState = None ; ChangePlayerNameState = None ; ChangePlayerTypeState = None ; WithdrawPlayerState = None
      EliminateSquadState = None }, Cmd.none

let private squadRvn (squadDic:SquadDic) squadId = if squadId |> squadDic.ContainsKey then squadDic.[squadId].Rvn |> Some else None

let private defaultAddPlayersState squadId playerType addPlayerStatus resultRvn = {
    SquadId = squadId
    NewPlayerId = PlayerId.Create ()
    NewPlayerNameText = String.Empty
    NewPlayerNameErrorText = None
    NewPlayerType = playerType
    AddPlayerStatus = addPlayerStatus
    ResultRvn = resultRvn }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleAddPlayerCmdResult (result:Result<Rvn * PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.AddPlayersState with
    | Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            match result with
            | Ok (rvn, playerName) ->
                let (PlayerName playerName) = playerName
                let addPlayersState = defaultAddPlayersState addPlayersState.SquadId addPlayersState.NewPlayerType None (rvn |> Some)
                { state with AddPlayersState = addPlayersState |> Some }, sprintf "<strong>%s</strong> has been added" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPlayersState = { addPlayersState with AddPlayerStatus = errorText |> AddPlayerFailed |> Some }
                { state with AddPlayersState = addPlayersState |> Some }, "Unable to add player" |> errorToastCmd
        | Some (AddPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersStatus is not AddPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersState is None -> %A" result)

let private handleChangePlayerNameCmdResult (result:Result<PlayerName * PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ChangePlayerNameState with
    | Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            match result with
            | Ok (previousPlayerName, playerName) ->
                let (PlayerName previousPlayerName), (PlayerName playerName) = previousPlayerName, playerName
                { state with ChangePlayerNameState = None }, sprintf "<strong>%s</strong> is now <strong>%s</strong>" previousPlayerName playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerNameCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = errorText |> ChangePlayerNameFailed |> Some }
                { state with ChangePlayerNameState = changePlayerNameState |> Some }, "Unable to edit player name" |> errorToastCmd
        | Some (ChangePlayerNameFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameStatus is not ChangePlayerNamePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameState is None -> %A" result)

let private handleChangePlayerTypeCmdResult (result:Result<PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ChangePlayerTypeState with
    | Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                { state with ChangePlayerTypeState = None }, sprintf "Position has been changed for <strong>%s</strong>" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerTypeCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = errorText |> ChangePlayerTypeFailed |> Some }
                { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, "Unable to change player position" |> errorToastCmd
        | Some (ChangePlayerTypeFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeStatus is not ChangePlayerTypePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeState is None -> %A" result)

let private handleWithdrawPlayerCmdResult (result:Result<PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.WithdrawPlayerState with
    | Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                { state with WithdrawPlayerState = None }, sprintf "<strong>%s</strong> has been withdrawn" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "WithdrawPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = errorText |> WithdrawPlayerFailed |> Some }
                { state with WithdrawPlayerState = withdrawPlayerState |> Some }, "Unable to withdraw player" |> errorToastCmd
        | Some (WithdrawPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerStatus is not WithdrawPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerState is None -> %A" result)

let private handleEliminateSquadCmdResult (result:Result<SquadName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.EliminateSquadState with
    | Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            match result with
            | Ok squadName ->
                let (SquadName squadName) = squadName
                { state with EliminateSquadState = None }, sprintf "<strong>%s</strong> has been eliminated" squadName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "EliminateSquadCmdResult error -> %A" error) (error |> cmdErrorText)
                let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = errorText |> EliminateSquadFailed |> Some }
                { state with EliminateSquadState = eliminateSquadState |> Some }, "Unable to eliminate team" |> errorToastCmd
        | Some (EliminateSquadFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadStatus is not EliminateSquadPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadState is None -> %A" result)

let private handleServerSquadsMsg serverSquadsMsg state : State * Cmd<Input> =
    match serverSquadsMsg with
    | AddPlayerCmdResult result ->
        state |> handleAddPlayerCmdResult result
    | ChangePlayerNameCmdResult result ->
        state |> handleChangePlayerNameCmdResult result
    | ChangePlayerTypeCmdResult result ->
        state |> handleChangePlayerTypeCmdResult result
    | WithdrawPlayerCmdResult result ->
        state |> handleWithdrawPlayerCmdResult result
    | EliminateSquadCmdResult result ->
        state |> handleEliminateSquadCmdResult result

let handleAddPlayersInput addPlayersInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match addPlayersInput, state.AddPlayersState with
    | NewPlayerNameTextChanged newPlayerNameText, Some addPlayersState ->
        let squadId = addPlayersState.SquadId
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let newPlayerNameErrorText = validatePlayerName playerNames (PlayerName newPlayerNameText)
        let addPlayersState = { addPlayersState with NewPlayerNameText = newPlayerNameText ; NewPlayerNameErrorText = newPlayerNameErrorText }
        { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
    | NewPlayerTypeChanged newPlayerType, Some addPlayersState ->
        let addPlayersState = { addPlayersState with NewPlayerType = newPlayerType }
        { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
    | AddPlayer, Some addPlayersState -> // note: assume no need to validate NewPlayerNameText (i.e. because Squads.Render.renderAddPlayersModal will ensure that AddPlayer can only be dispatched when valid)
        let addPlayersState = { addPlayersState with AddPlayerStatus = AddPlayerPending |> Some }   
        let squadId, resultRvn = addPlayersState.SquadId, addPlayersState.ResultRvn
        let currentRvn =
            match squadId |> squadRvn squadDic with
            | Some (Rvn squadRvn) -> match resultRvn with | Some (Rvn resultRvn) when resultRvn > squadRvn -> Rvn resultRvn | Some _ | None -> Rvn squadRvn
            | None -> match resultRvn with | Some rvn -> rvn | None -> initialRvn
        let addPlayerCmdParams = squadId, currentRvn, addPlayersState.NewPlayerId, PlayerName (addPlayersState.NewPlayerNameText.Trim ()), addPlayersState.NewPlayerType
        let cmd = addPlayerCmdParams |> AddPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with AddPlayersState = addPlayersState |> Some }, cmd, true
    | CancelAddPlayers, Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPlayers when AddPlayerPending", false
        | Some (AddPlayerFailed _) | None ->
            { state with AddPlayersState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayersInput when AddPlayersState is None -> %A" addPlayersInput), false

let handleChangePlayerNameInput changePlayerNameInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match changePlayerNameInput, state.ChangePlayerNameState with
    | PlayerNameTextChanged playerNameText, Some changePlayerNameState ->
        let squadId = changePlayerNameState.SquadId
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let playerNameErrorText = validatePlayerName playerNames (PlayerName playerNameText)
        let changePlayerNameState = { changePlayerNameState with PlayerNameText = playerNameText ; PlayerNameErrorText = playerNameErrorText }
        { state with ChangePlayerNameState = changePlayerNameState |> Some }, Cmd.none, true
    | ChangePlayerName, Some changePlayerNameState -> // note: assume no need to validate PlayerNameText (i.e. because Squads.Render.renderChangePlayerNameModal will ensure that ChangePlayerName can only be dispatched when valid)
        let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = ChangePlayerNamePending |> Some }   
        let squadId = changePlayerNameState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let changePlayerNameCmdParams = squadId, currentRvn, changePlayerNameState.PlayerId, PlayerName (changePlayerNameState.PlayerNameText.Trim ())
        let cmd = changePlayerNameCmdParams |> ChangePlayerNameCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ChangePlayerNameState = changePlayerNameState |> Some }, cmd, true
    | CancelChangePlayerName, Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerName when ChangePlayerNamePending", false
        | Some (ChangePlayerNameFailed _) | None ->
            { state with ChangePlayerNameState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameInput when ChangePlayerNameState is None -> %A" changePlayerNameInput), false

let handleChangePlayerTypeInput changePlayerTypeInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match changePlayerTypeInput, state.ChangePlayerTypeState with
    | PlayerTypeChanged playerType, Some changePlayerTypeState ->
        let changePlayerTypeState = { changePlayerTypeState with PlayerType = playerType |> Some }
        { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, Cmd.none, true
    | ChangePlayerType, Some changePlayerTypeState -> // note: assume no need to validate PlayerType (i.e. because Squads.Render.renderChangePlayerTypeModal will ensure that ChangePlayerType can only be dispatched when valid)
        match changePlayerTypeState.PlayerType with
        | Some playerType ->
            let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = ChangePlayerTypePending |> Some }   
            let squadId = changePlayerTypeState.SquadId
            let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
            let changePlayerTypeCmdParams = squadId, currentRvn, changePlayerTypeState.PlayerId, playerType
            let cmd = changePlayerTypeCmdParams |> ChangePlayerTypeCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, cmd, true
        | None -> // note: should never happen
            state, Cmd.none, false
    | CancelChangePlayerType, Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerType when ChangePlayerTypePending", false
        | Some (ChangePlayerTypeFailed _) | None ->
            { state with ChangePlayerTypeState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeInput when ChangePlayerTypeState is None -> %A" changePlayerTypeInput), false

let handleWithdrawPlayerInput withdrawPlayer (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match withdrawPlayer, state.WithdrawPlayerState with
    | ConfirmWithdrawPlayer, Some withdrawPlayerState ->
        let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = WithdrawPlayerPending |> Some }   
        let squadId = withdrawPlayerState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let cmd = (squadId, currentRvn, withdrawPlayerState.PlayerId) |> WithdrawPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with WithdrawPlayerState = withdrawPlayerState |> Some }, cmd, true
    | CancelWithdrawPlayer, Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelWithdrawPlayer when WithdrawPlayerPending", false
        | Some (WithdrawPlayerFailed _) | None ->
            { state with WithdrawPlayerState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerInput when WithdrawPlayerState is None -> %A" withdrawPlayer), false

let handleEliminateSquadInput eliminateSquadInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match eliminateSquadInput, state.EliminateSquadState with
    | ConfirmEliminateSquad, Some eliminateSquadState ->
        let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = EliminateSquadPending |> Some }   
        let squadId = eliminateSquadState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let cmd = (squadId, currentRvn) |> EliminateSquadCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with EliminateSquadState = eliminateSquadState |> Some }, cmd, true
    | CancelEliminateSquad, Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            state, shouldNeverHappenCmd "Unexpected CancelEliminateSquad when EliminateSquadPending", false
        | Some (EliminateSquadFailed _) | None ->
            { state with EliminateSquadState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadInput when EliminateSquadState is None -> %A" eliminateSquadInput), false

let transition input (squadsProjection:Projection<_ * SquadDic>) state =
    let state, cmd, isUserNonApiActivity =
        match input, squadsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerSquadsMsg serverSquadsMsg, _ ->
            let state, cmd = state |> handleServerSquadsMsg serverSquadsMsg
            state, cmd, false
        | ShowGroup group, Ready (_, squadDic) ->
            { state with CurrentSquadId = group |> defaultSquadId squadDic }, Cmd.none, true
        | ShowSquad squadId, Ready _ -> // note: no need to check for unknown squadId (should never happen)
            { state with CurrentSquadId = squadId |> Some }, Cmd.none, true
        | AddToDraft (_draftId, userDraftPickBasic), Ready _ ->
            let currentDraftPicks = state.CurrentDraftPicks
            if currentDraftPicks |> List.exists (fun draftPick -> draftPick.UserDraftPickBasic = userDraftPickBasic) |> not then
                // TEMP-NMB...
                let draftPick = { UserDraftPickBasic = userDraftPickBasic ; DraftPickStatus = AddPending |> Some } // TENP-NMB
                //let draftPick = { UserDraftPickBasic = userDraftPickBasic ; DraftPickStatus = None } // TENP-NMB
                // ...NMB-TEMP

                // TODO-NEXT: SendUiAuthMsg (&c.)...

                { state with CurrentDraftPicks = draftPick :: currentDraftPicks }, "Dummy implementation of AddToDraft: not persisted" |> warningToastCmd, true
            else state, UNEXPECTED_ERROR |> errorToastCmd, true
        | RemoveFromDraft (_draftId, userDraftPickBasic), Ready _ ->
            let currentDraftPicks = state.CurrentDraftPicks
            if currentDraftPicks |> List.exists (fun draftPick -> draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not) then
                // TEMP-NMB...
                let currentDraftPicks = currentDraftPicks |> List.map (fun draftPick ->
                    if draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not then { draftPick with DraftPickStatus = RemovePending |> Some }
                    else draftPick)
                (*let currentDraftPicks = currentDraftPicks |> List.choose (fun draftPick ->
                    if draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not then None
                    else draftPick |> Some)*)
                // ...NMB-TEMP

                // TODO-NEXT: SendUiAuthMsg (&c.)...

                { state with CurrentDraftPicks = currentDraftPicks }, "Dummy implementation of RemoveFromDraft: not persisted" |> warningToastCmd, true
            else state, UNEXPECTED_ERROR |> errorToastCmd, true
        | ShowAddPlayersModal squadId, Ready _ -> // note: no need to check for unknown squadId (should never happen)
            let addPlayersState = defaultAddPlayersState squadId Goalkeeper None None
            { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
        | AddPlayersInput addPlayersInput, Ready (_, squadDic) ->
            state |> handleAddPlayersInput addPlayersInput squadDic
        | ShowChangePlayerNameModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerNameState = { SquadId = squadId ; PlayerId = playerId ; PlayerNameText = String.Empty ; PlayerNameErrorText = None ; ChangePlayerNameStatus = None }
            { state with ChangePlayerNameState = changePlayerNameState |> Some }, Cmd.none, true
        | ChangePlayerNameInput changePlayerNameInput, Ready (_, squadDic) ->
            state |> handleChangePlayerNameInput changePlayerNameInput squadDic
        | ShowChangePlayerTypeModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerTypeState = { SquadId = squadId ; PlayerId = playerId ; PlayerType = None ; ChangePlayerTypeStatus = None }
            { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, Cmd.none, true
        | ChangePlayerTypeInput changePlayerTypeInput, Ready (_, squadDic) ->
            state |> handleChangePlayerTypeInput changePlayerTypeInput squadDic
        | ShowWithdrawPlayerModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let withdrawPlayerState = { SquadId = squadId ; PlayerId = playerId ; WithdrawPlayerStatus = None }
            { state with WithdrawPlayerState = withdrawPlayerState |> Some }, Cmd.none, true
        | WithdrawPlayerInput withdrawPlayerInput, Ready (_, squadDic) ->
            state |> handleWithdrawPlayerInput withdrawPlayerInput squadDic
        | ShowEliminateSquadModal squadId, Ready _ -> // note: no need to check for unknown squadId (should never happen)
            let eliminateSquadState = { SquadId = squadId ; EliminateSquadStatus = None }
            { state with EliminateSquadState = eliminateSquadState |> Some }, Cmd.none, true
        | EliminateSquadInput eliminateSquadInput, Ready (_, squadDic) ->
            state |> handleEliminateSquadInput eliminateSquadInput squadDic
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" squadsProjection input), false
    state, cmd, isUserNonApiActivity
