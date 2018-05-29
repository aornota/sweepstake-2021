module Aornota.Sweepstake2018.UI.Pages.Squads.State

open Aornota.Common.Delta
open Aornota.Common.IfDebug
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

let private player (playerDto:PlayerDto) = { PlayerName = playerDto.PlayerName ; PlayerType = playerDto.PlayerType ; Withdrawn = playerDto.Withdrawn }

let private squad (squadDto:SquadDto) =
    let playerDic = PlayerDic ()
    squadDto.PlayerDtos |> List.iter (fun playerDto ->
        if playerDto.PlayerId |> playerDic.ContainsKey |> not then // note: silently ignore duplicate PlayerIds (should never happen)
            (playerDto.PlayerId, playerDto |> player) |> playerDic.Add)
    let squadOnlyDto = squadDto.SquadOnlyDto
    { Rvn = squadOnlyDto.Rvn ; SquadName = squadOnlyDto.SquadName ; Group = squadOnlyDto.Group ; Seeding = squadOnlyDto.Seeding ; CoachName = squadOnlyDto.CoachName
      Eliminated = squadOnlyDto.Eliminated ; PlayerDic = playerDic }

let private squadsProjection (squadsProjectionDto:SquadsProjectionDto) =
    let squadDic = SquadDic ()
    squadsProjectionDto.SquadDtos |> List.iter (fun squadDto ->
        let squadId = squadDto.SquadOnlyDto.SquadId
        if squadId |> squadDic.ContainsKey |> not then // note: silently ignore duplicate SquadIds (should never happen)
            (squadId, squadDto |> squad) |> squadDic.Add)
    { Rvn = initialRvn ; SquadDic = squadDic }

let private applyPlayerDelta currentRvn deltaRvn (delta:Delta<PlayerId, PlayerDto>) (playerDic:PlayerDic) =
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

let private handleAddPlayerCmdResult (result:Result<Rvn, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.AddPlayersState with
    | Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            match result with
            | Ok rvn ->
                let addPlayersState = defaultAddPlayersState addPlayersState.SquadId addPlayersState.NewPlayerType None (rvn |> Some)
                let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
                { state with ProjectionState = Active activeState }, Cmd.none
            | Error error ->
                let errorText = ifDebug (sprintf "AddPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPlayersState = { addPlayersState with AddPlayerStatus = errorText |> AddPlayerFailed |> Some }
                let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to add player" |> errorToastCmd
        | Some (AddPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersStatus is not AddPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersState is None -> %A" result)

let private handleServerSquadsMsg serverSquadsMsg authUser state : State * Cmd<Input> =
    match serverSquadsMsg, state.ProjectionState with
    | InitializeSquadsProjectionUnauthQryResult (Ok squadsProjectionDto), Initializing currentSquadId ->
        let squadsProjection = squadsProjectionDto |> squadsProjection
        let activeState = {
            SquadsProjection = squadsProjection
            CurrentSquadId = squadsProjection.SquadDic |> squadIdOrDefault currentSquadId
            AddPlayersState = None }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeSquadsProjectionUnauthQryResult (Error (OtherError errorText)), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | InitializeSquadsProjectionAuthQryResult (Ok squadsProjectionDto), Initializing currentSquadId ->
        let squadsProjection = squadsProjectionDto |> squadsProjection
        let activeState = {
            SquadsProjection = squadsProjection
            CurrentSquadId = squadsProjection.SquadDic |> squadIdOrDefault currentSquadId
            AddPlayersState = None }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeSquadsProjectionAuthQryResult (Error error), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | AddPlayerCmdResult result, Active activeState ->
        state |> handleAddPlayerCmdResult result activeState
    | SquadsProjectionMsg (PlayersDeltaMsg (deltaRvn, squadId, squadRvn, playerDtoDelta)), Active activeState ->
        let squadProjection = activeState.SquadsProjection
        let squadDic = squadProjection.SquadDic
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        match squad with
        | Some squad ->
            match squad.PlayerDic |> applyPlayerDelta squadProjection.Rvn deltaRvn playerDtoDelta with
            | Ok playerDic ->
                let squad = { squad with Rvn = squadRvn ; PlayerDic = playerDic }
                squadDic.[squadId] <- squad
                let squadsProjection = { squadProjection with Rvn = deltaRvn }
                let addPlayersState =
                    match activeState.AddPlayersState with
                    | Some addPlayersState when addPlayersState.SquadId = squadId ->
                        let newPlayerNameText = addPlayersState.NewPlayerNameText
                        if String.IsNullOrWhiteSpace newPlayerNameText |> not then
                            let newPlayerNameErrorText = validatePlayerName (squad.PlayerDic |> playerNames) (PlayerName newPlayerNameText)
                            { addPlayersState with NewPlayerNameErrorText = newPlayerNameErrorText } |> Some
                        else addPlayersState |> Some
                    | Some _ | None -> None
                let activeState = { activeState with SquadsProjection = squadsProjection ; AddPlayersState = addPlayersState }
                { state with ProjectionState = Active activeState }, Cmd.none
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" playerDtoDelta squad.PlayerDic error)
                let state, cmd = initialize authUser activeState.CurrentSquadId
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | None -> // note: silently ignore unknown SquadId (should never happen)
            state, Cmd.none
    | SquadsProjectionMsg _, _ -> // note: silently ignore SquadsProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerSquadsMsg when %A -> %A" state.ProjectionState serverSquadsMsg)

let handleAddPlayersInput addPlayersInput activeState state : State * Cmd<Input> * bool =
    match addPlayersInput, activeState.AddPlayersState with
    | CancelAddPlayers, Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPlayers when AddPlayerPending", false
        | Some (AddPlayerFailed _) | None ->
            let activeState = { activeState with AddPlayersState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
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
        let addPlayerCmdParams = addPlayersState.SquadId, currentRvn, addPlayersState.NewPlayerId, (PlayerName addPlayersState.NewPlayerNameText), addPlayersState.NewPlayerType
        let cmd = addPlayerCmdParams |> AddPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayersInput when AddPlayersState is None -> %A" addPlayersInput), false

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
            let activeState = { activeState with CurrentSquadId = activeState.SquadsProjection.SquadDic |> defaultSquadId group }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowSquad squadId, Active activeState -> // note: no need to check for unknown SquadId (should never happen)
            let activeState = { activeState with CurrentSquadId = squadId |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ShowAddPlayersModal squadId, Active activeState -> // note: no need to check for unknown SquadId (should never happen)
            let addPlayersState = defaultAddPlayersState squadId Goalkeeper None None
            let activeState = { activeState with AddPlayersState = addPlayersState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | AddPlayersInput addPlayersInput, Active activeState ->
            state |> handleAddPlayersInput addPlayersInput activeState
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
