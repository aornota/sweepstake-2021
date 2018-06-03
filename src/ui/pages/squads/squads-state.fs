module Aornota.Sweepstake2018.UI.Pages.Squads.State

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.Squads.Common

open System

open Elmish
open Aornota.Sweepstake2018.Common.Domain.Draft

let initialize (authUser:AuthUser option) currentSquadId : State * Cmd<Input> =
    let state = { ProjectionState = Initializing currentSquadId }
    let cmd =
        match authUser with
        | Some _ -> InitializeSquadsProjectionAuthQry |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        | None -> InitializeSquadsProjectionUnauthQry |> UiUnauthSquadsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
    state, cmd

let private defaultAddPlayersState squadId playerType addPlayerStatus resultRvn = {
    SquadId = squadId
    NewPlayerId = PlayerId.Create ()
    NewPlayerNameText = String.Empty
    NewPlayerNameErrorText = None
    NewPlayerType = playerType
    AddPlayerStatus = addPlayerStatus
    ResultRvn = resultRvn }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private player (playerDto:PlayerDto) = { PlayerName = playerDto.PlayerName ; PlayerType = playerDto.PlayerType ; PlayerStatus = playerDto.PlayerStatus }

let private squad (squadDto:SquadDto) =
    let playerDic = PlayerDic ()
    squadDto.PlayerDtos |> List.iter (fun playerDto ->
        if playerDto.PlayerId |> playerDic.ContainsKey |> not then // note: silently ignore duplicate PlayerIds (should never happen)
            (playerDto.PlayerId, playerDto |> player) |> playerDic.Add)
    let squadOnlyDto = squadDto.SquadOnlyDto
    { Rvn = squadOnlyDto.Rvn ; SquadName = squadOnlyDto.SquadName ; Group = squadOnlyDto.Group ; Seeding = squadOnlyDto.Seeding ; CoachName = squadOnlyDto.CoachName
      Eliminated = squadOnlyDto.Eliminated ; PlayerDic = playerDic }

let private updateSquad (squadOnlyDto:SquadOnlyDto) (squad:Squad) =
    { squad with Rvn = squadOnlyDto.Rvn ; SquadName = squadOnlyDto.SquadName ; Group = squadOnlyDto.Group ; Seeding = squadOnlyDto.Seeding ; CoachName = squadOnlyDto.CoachName
                 Eliminated = squadOnlyDto.Eliminated ; PlayerDic = squad.PlayerDic }

let private squadsProjection (squadsProjectionDto:SquadsProjectionDto) =
    let squadDic = SquadDic ()
    squadsProjectionDto.SquadDtos |> List.iter (fun squadDto ->
        let squadId = squadDto.SquadOnlyDto.SquadId
        if squadId |> squadDic.ContainsKey |> not then // note: silently ignore duplicate SquadIds (should never happen)
            (squadId, squadDto |> squad) |> squadDic.Add)
    { Rvn = initialRvn ; SquadDic = squadDic }

let private applySquadsDelta currentRvn deltaRvn (delta:Delta<SquadId, SquadOnlyDto>) (squadDic:SquadDic) =
    let squadDic = SquadDic squadDic // note: copy to ensure that passed-in dictionary *not* modified if error [but no need to copy PlayerDic/s since not changing those]
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (squadId, squadOnlyDto) -> if squadId |> squadDic.ContainsKey then (squadId, squadOnlyDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (squadId, squadOnlyDto) ->
            let squadDto = { SquadOnlyDto = squadOnlyDto ; PlayerDtos = [] }
            (squadId, squadDto |> squad) |> squadDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (squadId, squadOnlyDto) -> if squadId |> squadDic.ContainsKey |> not then (squadId, squadOnlyDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (squadId, squadOnlyDto) ->
            let squad = squadDic.[squadId]
            squadDic.[squadId] <- (squad |> updateSquad squadOnlyDto)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun squadId -> if squadId |> squadDic.ContainsKey |> not then squadId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (squadDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> squadDic |> Ok)

let private applyPlayersDelta currentRvn deltaRvn (delta:Delta<PlayerId, PlayerDto>) (playerDic:PlayerDic) =
    let playerDic = PlayerDic playerDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (playerId, playerDto) -> if playerId |> playerDic.ContainsKey then (playerId, playerDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (playerId, playerDto) -> (playerId, playerDto |> player) |> playerDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (playerId, playerDto) -> if playerId |> playerDic.ContainsKey |> not then (playerId, playerDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (playerId, playerDto) -> playerDic.[playerId] <- (playerDto |> player)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun playerId -> if playerId |> playerDic.ContainsKey |> not then playerId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (playerDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> playerDic |> Ok)

let private defaultSquadId group (squadDic:SquadDic) =
    let groupSquads = squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad) |> List.filter (fun (_, squad) -> squad.Group = group)
    match groupSquads |> List.sortBy (fun (_, squad) -> squad.SquadName) with | (squadId, _) :: _ -> squadId |> Some | [] -> None

let private squadIdOrDefault currentSquadId (squadDic:SquadDic) =
    match currentSquadId with
    | Some currentSquadId when currentSquadId |> squadDic.ContainsKey -> currentSquadId |> Some
    | Some _ | None -> squadDic |> defaultSquadId GroupA

let private cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
let private qryErrorText error = match error with | AuthQryJwtError _ | AuthQryAuthznError _ -> UNEXPECTED_ERROR | OtherAuthQryError (OtherError errorText) -> errorText

let private draftNotificationMessage currentDraftDto =
    let date (local:DateTime) = sprintf "%s %i%s %s" (local.DayOfWeek |> dayName) local.Day (local.Day |> suffix) (local.Month |> monthName)
    let dateAndTime (local:DateTime) = sprintf "%s at %s" (local |> date) (local.ToString ("HH:mm"))
    match currentDraftDto with
    | Some currentDraftDto ->
        let draftTextLower = currentDraftDto.DraftOrdinal |> draftTextLower
        let text =
            match currentDraftDto.DraftStatusDto with
            | PendingOpenDto (starts, ends) ->
                let starts, ends = starts.LocalDateTime, ends.LocalDateTime
                sprintf "The %s will open on %s and will close on %s" draftTextLower (starts |> dateAndTime) (ends |> dateAndTime)
            | OpenedDto ends ->
                let ends = ends.LocalDateTime
                sprintf "The %s is now open and will close on %s" draftTextLower (ends |> dateAndTime)
            | PendingProcessingDto -> sprintf "The %s will be processed soon" draftTextLower
            | FreeSelectionDto -> "There are no further drafts; please pick team/coach | goalkeeper | outfield players (as required)" // TODO-SOON: Finesse this...
        infoMessage text false |> Some
    | None -> None

let private handleAddPlayerCmdResult (result:Result<Rvn * PlayerName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.AddPlayersState with
    | Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            match result with
            | Ok (rvn, playerName) ->
                let (PlayerName playerName) = playerName
                let addPlayersState = defaultAddPlayersState addPlayersState.SquadId addPlayersState.NewPlayerType None (rvn |> Some)
                let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
                { state with ProjectionState = Active activeState }, sprintf "<strong>%s</strong> has been added" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPlayersState = { addPlayersState with AddPlayerStatus = errorText |> AddPlayerFailed |> Some }
                let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to add player" |> errorToastCmd
        | Some (AddPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersStatus is not AddPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersState is None -> %A" result)

let private handleChangePlayerNameCmdResult (result:Result<PlayerName * PlayerName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.ChangePlayerNameState with
    | Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            match result with
            | Ok (previousPlayerName, playerName) ->
                let (PlayerName previousPlayerName), (PlayerName playerName) = previousPlayerName, playerName
                let activeState = { activeState with ChangePlayerNameState = None }
                { state with ProjectionState = Active activeState }, sprintf "<strong>%s</strong> is now <strong>%s</strong>" previousPlayerName playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerNameCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = errorText |> ChangePlayerNameFailed |> Some }
                let activeState = { activeState with ChangePlayerNameState = changePlayerNameState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to edit player name" |> errorToastCmd
        | Some (ChangePlayerNameFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameStatus is not ChangePlayerNamePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameState is None -> %A" result)

let private handleChangePlayerTypeCmdResult (result:Result<PlayerName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.ChangePlayerTypeState with
    | Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                let activeState = { activeState with ChangePlayerTypeState = None }
                { state with ProjectionState = Active activeState }, sprintf "Position has been changed for <strong>%s</strong>" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerTypeCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = errorText |> ChangePlayerTypeFailed |> Some }
                let activeState = { activeState with ChangePlayerTypeState = changePlayerTypeState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to change player position" |> errorToastCmd
        | Some (ChangePlayerTypeFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeStatus is not ChangePlayerTypePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeState is None -> %A" result)

let private handleWithdrawPlayerCmdResult (result:Result<PlayerName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.WithdrawPlayerState with
    | Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                let activeState = { activeState with WithdrawPlayerState = None }
                { state with ProjectionState = Active activeState }, sprintf "<strong>%s</strong> has been withdrawn" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "WithdrawPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = errorText |> WithdrawPlayerFailed |> Some }
                let activeState = { activeState with WithdrawPlayerState = withdrawPlayerState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to withdraw player" |> errorToastCmd
        | Some (WithdrawPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerStatus is not WithdrawPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerState is None -> %A" result)

let private handleEliminateSquadCmdResult (result:Result<SquadName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.EliminateSquadState with
    | Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            match result with
            | Ok squadName ->
                let (SquadName squadName) = squadName
                let activeState = { activeState with EliminateSquadState = None }
                { state with ProjectionState = Active activeState }, sprintf "<strong>%s</strong> has been eliminated" squadName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "EliminateSquadCmdResult error -> %A" error) (error |> cmdErrorText)
                let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = errorText |> EliminateSquadFailed |> Some }
                let activeState = { activeState with EliminateSquadState = eliminateSquadState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to eliminate team" |> errorToastCmd
        | Some (EliminateSquadFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadStatus is not EliminateSquadPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadState is None -> %A" result)

let private handleServerSquadsMsg serverSquadsMsg authUser state : State * Cmd<Input> =
    match serverSquadsMsg, state.ProjectionState with
    | InitializeSquadsProjectionUnauthQryResult (Ok squadsProjectionDto), Initializing currentSquadId ->
        let squadsProjection = squadsProjectionDto |> squadsProjection
        let activeState = {
            SquadsProjection = squadsProjection
            CurrentDraftDto = None
            CurrentDraftNotificationId = None
            CurrentDraftPicks = []
            CurrentSquadId = squadsProjection.SquadDic |> squadIdOrDefault currentSquadId
            AddPlayersState = None
            ChangePlayerNameState = None
            ChangePlayerTypeState = None
            WithdrawPlayerState = None
            EliminateSquadState = None }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeSquadsProjectionUnauthQryResult (Error (OtherError errorText)), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | InitializeSquadsProjectionAuthQryResult (Ok (squadsProjectionDto, currentDraftDto)), Initializing currentSquadId ->
        let squadsProjection = squadsProjectionDto |> squadsProjection
        let draftNotificationCmd, draftNotificationId =
            match currentDraftDto |> draftNotificationMessage with
            | Some draftNotificationMessage -> draftNotificationMessage |> AddNotificationMessage |> Cmd.ofMsg, draftNotificationMessage.NotificationId |> Some
            | None -> Cmd.none, None

        // TODO-SOON: Other "initialization" (e.g. Ok might include some sort of CurrentUserDraftPickDic?...

        let activeState = {
            SquadsProjection = squadsProjection
            CurrentDraftDto = currentDraftDto
            CurrentDraftNotificationId = draftNotificationId
            CurrentDraftPicks = []
            CurrentSquadId = squadsProjection.SquadDic |> squadIdOrDefault currentSquadId
            AddPlayersState = None
            ChangePlayerNameState = None
            ChangePlayerTypeState = None
            WithdrawPlayerState = None
            EliminateSquadState = None }
        { state with ProjectionState = Active activeState }, draftNotificationCmd
    | InitializeSquadsProjectionAuthQryResult (Error error), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | AddPlayerCmdResult result, Active activeState ->
        state |> handleAddPlayerCmdResult result activeState
    | ChangePlayerNameCmdResult result, Active activeState ->
        state |> handleChangePlayerNameCmdResult result activeState
    | ChangePlayerTypeCmdResult result, Active activeState ->
        state |> handleChangePlayerTypeCmdResult result activeState
    | WithdrawPlayerCmdResult result, Active activeState ->
        state |> handleWithdrawPlayerCmdResult result activeState
    | EliminateSquadCmdResult result, Active activeState ->
        state |> handleEliminateSquadCmdResult result activeState
    | SquadsProjectionMsg (SquadsDeltaMsg (deltaRvn, squadOnlyDtoDelta)), Active activeState ->
        let squadsProjection = activeState.SquadsProjection
        match squadsProjection.SquadDic |> applySquadsDelta squadsProjection.Rvn deltaRvn squadOnlyDtoDelta with
        | Ok squadDic ->
            let squadsProjection = { squadsProjection with Rvn = deltaRvn ; SquadDic = squadDic }
            let activeState = { activeState with SquadsProjection = squadsProjection }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" squadOnlyDtoDelta squadsProjection.SquadDic error)
            let state, cmd = initialize authUser activeState.CurrentSquadId
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | SquadsProjectionMsg (PlayersDeltaMsg (deltaRvn, squadId, squadRvn, playerDtoDelta)), Active activeState ->
        let squadsProjection = activeState.SquadsProjection
        let squadDic = squadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        match squad with
        | Some squad ->
            match squad.PlayerDic |> applyPlayersDelta squadsProjection.Rvn deltaRvn playerDtoDelta with
            | Ok playerDic ->
                let squad = { squad with Rvn = squadRvn ; PlayerDic = playerDic }
                squadDic.[squadId] <- squad
                let squadsProjection = { squadsProjection with Rvn = deltaRvn }
                let addPlayersState =
                    match activeState.AddPlayersState with
                    | Some addPlayersState when addPlayersState.SquadId = squadId ->
                        let newPlayerNameText = addPlayersState.NewPlayerNameText
                        if String.IsNullOrWhiteSpace newPlayerNameText |> not then
                            let newPlayerNameErrorText = validatePlayerName (squad.PlayerDic |> playerNames) (PlayerName newPlayerNameText)
                            { addPlayersState with NewPlayerNameErrorText = newPlayerNameErrorText } |> Some
                        else addPlayersState |> Some
                    | Some _ | None -> None
                let changePlayerNameState =
                    match activeState.ChangePlayerNameState with
                    | Some changePlayerNameState when changePlayerNameState.SquadId = squadId ->
                        let playerNameText = changePlayerNameState.PlayerNameText
                        if String.IsNullOrWhiteSpace playerNameText |> not then
                            let playerNameErrorText = validatePlayerName (squad.PlayerDic |> playerNames) (PlayerName playerNameText)
                            { changePlayerNameState with PlayerNameErrorText = playerNameErrorText } |> Some
                        else changePlayerNameState |> Some
                    | Some _ | None -> None
                let activeState = { activeState with SquadsProjection = squadsProjection ; AddPlayersState = addPlayersState ; ChangePlayerNameState = changePlayerNameState }
                { state with ProjectionState = Active activeState }, Cmd.none
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" playerDtoDelta squad.PlayerDic error)
                let state, cmd = initialize authUser activeState.CurrentSquadId
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | None -> // note: silently ignore unknown squadId (should never happen)
            state, Cmd.none
    | SquadsProjectionMsg (CurrentDraftChangedMsg currentDraftDto), Active activeState ->
        let removeNotificationCmd = match activeState.CurrentDraftNotificationId with | Some notificationId -> notificationId |> RemoveNotificationMessage |> Cmd.ofMsg | None -> Cmd.none
        let draftNotificationCmd, draftNotificationId =
            match currentDraftDto |> draftNotificationMessage with
            | Some draftNotificationMessage -> draftNotificationMessage |> AddNotificationMessage |> Cmd.ofMsg, draftNotificationMessage.NotificationId |> Some
            | None -> Cmd.none, None
        let activeState = { activeState with CurrentDraftDto = currentDraftDto ; CurrentDraftNotificationId = draftNotificationId ; CurrentDraftPicks = [] }
        { state with ProjectionState = Active activeState }, Cmd.batch [ removeNotificationCmd ; draftNotificationCmd ]
    | SquadsProjectionMsg _, _ -> // note: silently ignore SquadsProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerSquadsMsg when %A -> %A" state.ProjectionState serverSquadsMsg)

let handleAddPlayersInput addPlayersInput activeState state : State * Cmd<Input> * bool =
    match addPlayersInput, activeState.AddPlayersState with
    | NewPlayerNameTextChanged newPlayerNameText, Some addPlayersState ->
        let squadId, squadDic = addPlayersState.SquadId, activeState.SquadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let newPlayerNameErrorText = validatePlayerName playerNames (PlayerName newPlayerNameText)
        let addPlayersState = { addPlayersState with NewPlayerNameText = newPlayerNameText ; NewPlayerNameErrorText = newPlayerNameErrorText }
        let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | NewPlayerTypeChanged newPlayerType, Some addPlayersState ->
        let addPlayersState = { addPlayersState with NewPlayerType = newPlayerType }
        let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | AddPlayer, Some addPlayersState -> // note: assume no need to validate NewPlayerNameText (i.e. because Squads.Render.renderAddPlayersModal will ensure that AddPlayer can only be dispatched when valid)
        let addPlayersState = { addPlayersState with AddPlayerStatus = AddPlayerPending |> Some }   
        let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
        let squadId, squadDic, resultRvn = addPlayersState.SquadId, activeState.SquadsProjection.SquadDic, addPlayersState.ResultRvn
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let currentRvn =
            match squad with
            | Some squad ->
                let (Rvn squadRvn) = squad.Rvn
                match resultRvn with | Some (Rvn resultRvn) when resultRvn > squadRvn -> Rvn resultRvn | Some _ | None -> Rvn squadRvn
            | None -> match resultRvn with | Some rvn -> rvn | None -> initialRvn
        let addPlayerCmdParams = squadId, currentRvn, addPlayersState.NewPlayerId, PlayerName (addPlayersState.NewPlayerNameText.Trim ()), addPlayersState.NewPlayerType
        let cmd = addPlayerCmdParams |> AddPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelAddPlayers, Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPlayers when AddPlayerPending", false
        | Some (AddPlayerFailed _) | None ->
            let activeState = { activeState with AddPlayersState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayersInput when AddPlayersState is None -> %A" addPlayersInput), false

let handleChangePlayerNameInput changePlayerNameInput activeState state : State * Cmd<Input> * bool =
    match changePlayerNameInput, activeState.ChangePlayerNameState with
    | PlayerNameTextChanged playerNameText, Some changePlayerNameState ->
        let squadId, squadDic = changePlayerNameState.SquadId, activeState.SquadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let playerNameErrorText = validatePlayerName playerNames (PlayerName playerNameText)
        let changePlayerNameState = { changePlayerNameState with PlayerNameText = playerNameText ; PlayerNameErrorText = playerNameErrorText }
        let activeState = { activeState with ChangePlayerNameState = changePlayerNameState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | ChangePlayerName, Some changePlayerNameState -> // note: assume no need to validate PlayerNameText (i.e. because Squads.Render.renderChangePlayerNameModal will ensure that ChangePlayerName can only be dispatched when valid)
        let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = ChangePlayerNamePending |> Some }   
        let activeState = { activeState with ChangePlayerNameState = changePlayerNameState |> Some }
        let squadId, squadDic = changePlayerNameState.SquadId, activeState.SquadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let currentRvn = match squad with | Some squad -> squad.Rvn | None -> initialRvn
        let changePlayerNameCmdParams = squadId, currentRvn, changePlayerNameState.PlayerId, PlayerName (changePlayerNameState.PlayerNameText.Trim ())
        let cmd = changePlayerNameCmdParams |> ChangePlayerNameCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelChangePlayerName, Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerName when ChangePlayerNamePending", false
        | Some (ChangePlayerNameFailed _) | None ->
            let activeState = { activeState with ChangePlayerNameState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameInput when ChangePlayerNameState is None -> %A" changePlayerNameInput), false

let handleChangePlayerTypeInput changePlayerTypeInput activeState state : State * Cmd<Input> * bool =
    match changePlayerTypeInput, activeState.ChangePlayerTypeState with
    | PlayerTypeChanged playerType, Some changePlayerTypeState ->
        let changePlayerTypeState = { changePlayerTypeState with PlayerType = playerType |> Some }
        let activeState = { activeState with ChangePlayerTypeState = changePlayerTypeState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | ChangePlayerType, Some changePlayerTypeState -> // note: assume no need to validate PlayerType (i.e. because Squads.Render.renderChangePlayerTypeModal will ensure that ChangePlayerType can only be dispatched when valid)
        match changePlayerTypeState.PlayerType with
        | Some playerType ->
            let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = ChangePlayerTypePending |> Some }   
            let activeState = { activeState with ChangePlayerTypeState = changePlayerTypeState |> Some }
            let squadId, squadDic = changePlayerTypeState.SquadId, activeState.SquadsProjection.SquadDic
            let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
            let currentRvn = match squad with | Some squad -> squad.Rvn | None -> initialRvn
            let changePlayerTypeCmdParams = squadId, currentRvn, changePlayerTypeState.PlayerId, playerType
            let cmd = changePlayerTypeCmdParams |> ChangePlayerTypeCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ProjectionState = Active activeState }, cmd, true
        | None -> // note: should never happen
            state, Cmd.none, false
    | CancelChangePlayerType, Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerType when ChangePlayerTypePending", false
        | Some (ChangePlayerTypeFailed _) | None ->
            let activeState = { activeState with ChangePlayerTypeState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeInput when ChangePlayerTypeState is None -> %A" changePlayerTypeInput), false

let handleWithdrawPlayerInput withdrawPlayer activeState state : State * Cmd<Input> * bool =
    match withdrawPlayer, activeState.WithdrawPlayerState with
    | ConfirmWithdrawPlayer, Some withdrawPlayerState ->
        let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = WithdrawPlayerPending |> Some }   
        let activeState = { activeState with WithdrawPlayerState = withdrawPlayerState |> Some }
        let squadId, squadDic = withdrawPlayerState.SquadId, activeState.SquadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let currentRvn = match squad with | Some squad -> squad.Rvn | None -> initialRvn
        let cmd = (squadId, currentRvn, withdrawPlayerState.PlayerId) |> WithdrawPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelWithdrawPlayer, Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelWithdrawPlayer when WithdrawPlayerPending", false
        | Some (WithdrawPlayerFailed _) | None ->
            let activeState = { activeState with WithdrawPlayerState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerInput when WithdrawPlayerState is None -> %A" withdrawPlayer), false

let handleEliminateSquadInput eliminateSquadInput activeState state : State * Cmd<Input> * bool =
    match eliminateSquadInput, activeState.EliminateSquadState with
    | ConfirmEliminateSquad, Some eliminateSquadState ->
        let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = EliminateSquadPending |> Some }   
        let activeState = { activeState with EliminateSquadState = eliminateSquadState |> Some }
        let squadId, squadDic = eliminateSquadState.SquadId, activeState.SquadsProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let currentRvn = match squad with | Some squad -> squad.Rvn | None -> initialRvn
        let cmd = (squadId, currentRvn) |> EliminateSquadCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelEliminateSquad, Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            state, shouldNeverHappenCmd "Unexpected CancelEliminateSquad when EliminateSquadPending", false
        | Some (EliminateSquadFailed _) | None ->
            let activeState = { activeState with EliminateSquadState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadInput when EliminateSquadState is None -> %A" eliminateSquadInput), false

let transition input authUser state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | RemoveNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiUnauthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerSquadsMsg serverSquadsMsg, _ ->
            let state, cmd = state |> handleServerSquadsMsg serverSquadsMsg authUser
            state, cmd, false
        | ShowGroup group, Active activeState ->
            let activeState = { activeState with CurrentSquadId = activeState.SquadsProjection.SquadDic |> defaultSquadId group }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowSquad squadId, Active activeState -> // note: no need to check for unknown squadId (should never happen)
            let activeState = { activeState with CurrentSquadId = squadId |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | AddToDraft (_draftId, userDraftPickBasic), Active activeState ->
            let currentDraftPicks = activeState.CurrentDraftPicks
            if currentDraftPicks |> List.exists (fun draftPick -> draftPick.UserDraftPickBasic = userDraftPickBasic) |> not then
                // TEMP-NMB...
                let draftPick = { UserDraftPickBasic = userDraftPickBasic ; DraftPickStatus = AddPending |> Some } // TENP-NMB
                //let draftPick = { UserDraftPickBasic = userDraftPickBasic ; DraftPickStatus = None } // TENP-NMB
                // ...NMB-TEMP
                let activeState = { activeState with CurrentDraftPicks = draftPick :: currentDraftPicks }

                // TODO-NEXT: SendUiAuthMsg (&c.)...

                { state with ProjectionState = Active activeState }, "Dummy implementation of AddToDraft: not persisted" |> warningToastCmd, true
            else state, UNEXPECTED_ERROR |> errorToastCmd, true
        | RemoveFromDraft (_draftId, userDraftPickBasic), Active activeState ->
            let currentDraftPicks = activeState.CurrentDraftPicks
            if currentDraftPicks |> List.exists (fun draftPick -> draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not) then
                // TEMP-NMB...
                let currentDraftPicks = currentDraftPicks |> List.map (fun draftPick ->
                    if draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not then { draftPick with DraftPickStatus = RemovePending |> Some }
                    else draftPick)
                (*let currentDraftPicks = currentDraftPicks |> List.choose (fun draftPick ->
                    if draftPick.UserDraftPickBasic = userDraftPickBasic && draftPick |> isRemovePending |> not then None
                    else draftPick |> Some)*)
                // ...NMB-TEMP
                let activeState = { activeState with CurrentDraftPicks = currentDraftPicks }

                // TODO-NEXT: SendUiAuthMsg (&c.)...

                { state with ProjectionState = Active activeState }, "Dummy implementation of RemoveFromDraft: not persisted" |> warningToastCmd, true
            else state, UNEXPECTED_ERROR |> errorToastCmd, true
        | ShowAddPlayersModal squadId, Active activeState -> // note: no need to check for unknown squadId (should never happen)
            let addPlayersState = defaultAddPlayersState squadId Goalkeeper None None
            let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | AddPlayersInput addPlayersInput, Active activeState ->
            state |> handleAddPlayersInput addPlayersInput activeState
        | ShowChangePlayerNameModal (squadId, playerId), Active activeState -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerNameState = { SquadId = squadId ; PlayerId = playerId ; PlayerNameText = String.Empty ; PlayerNameErrorText = None ; ChangePlayerNameStatus = None }
            let activeState = { activeState with ChangePlayerNameState = changePlayerNameState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ChangePlayerNameInput changePlayerNameInput, Active activeState ->
            state |> handleChangePlayerNameInput changePlayerNameInput activeState
        | ShowChangePlayerTypeModal (squadId, playerId), Active activeState -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerTypeState = { SquadId = squadId ; PlayerId = playerId ; PlayerType = None ; ChangePlayerTypeStatus = None }
            let activeState = { activeState with ChangePlayerTypeState = changePlayerTypeState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ChangePlayerTypeInput changePlayerTypeInput, Active activeState ->
            state |> handleChangePlayerTypeInput changePlayerTypeInput activeState
        | ShowWithdrawPlayerModal (squadId, playerId), Active activeState -> // note: no need to check for unknown squadId / playerId (should never happen)
            let withdrawPlayerState = { SquadId = squadId ; PlayerId = playerId ; WithdrawPlayerStatus = None }
            let activeState = { activeState with WithdrawPlayerState = withdrawPlayerState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | WithdrawPlayerInput withdrawPlayerInput, Active activeState ->
            state |> handleWithdrawPlayerInput withdrawPlayerInput activeState
        | ShowEliminateSquadModal squadId, Active activeState -> // note: no need to check for unknown squadId (should never happen)
            let eliminateSquadState = { SquadId = squadId ; EliminateSquadStatus = None }
            let activeState = { activeState with EliminateSquadState = eliminateSquadState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | EliminateSquadInput eliminateSquadInput, Active activeState ->
            state |> handleEliminateSquadInput eliminateSquadInput activeState
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
