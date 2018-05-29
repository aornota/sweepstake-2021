module Aornota.Sweepstake2018.UI.Pages.Squads.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System.Collections.Generic

type AddPlayersInput =
    | NewPlayerNameTextChanged of newPlayerNameText : string
    | NewPlayerTypeChanged of newPlayerType : PlayerType
    | AddPlayer
    | CancelAddPlayers

type ChangePlayerNameInput =
    | PlayerNameTextChanged of newPlayerNameText : string
    | ChangePlayerName
    | CancelChangePlayerName

type ChangePlayerTypeInput =
    | PlayerTypeChanged of playerType : PlayerType
    | ChangePlayerType
    | CancelChangePlayerType

type EliminateSquadInput =
    | ConfirmEliminateSquad
    | CancelEliminateSquad

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg
    | ShowGroup of group : Group
    | ShowSquad of squadId : SquadId
    | ShowAddPlayersModal of squadId : SquadId
    | AddPlayersInput of addPlayersInput : AddPlayersInput
    | ShowChangePlayerNameModal of squadId : SquadId * playerId : PlayerId
    | ChangePlayerNameInput of changePlayerNameInput : ChangePlayerNameInput
    | ShowChangePlayerTypeModal of squadId : SquadId * playerId : PlayerId
    | ChangePlayerTypeInput of changePlayerTypeInput : ChangePlayerTypeInput
    | ShowWithdrawPlayerModal of squadId : SquadId * playerId : PlayerId
    //| WithdrawPlayerInput...
    | ShowEliminateSquadModal of squadId : SquadId
    | EliminateSquadInput of eliminateSquadInput : EliminateSquadInput

type Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; Withdrawn : bool } // TODO-NMB-MEDIUM: dateWithdrawn? draftedBy? pickedBy? score?...
type PlayerDic = Dictionary<PlayerId, Player>

type Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic }
type SquadDic = Dictionary<SquadId, Squad>

type SquadsProjection = { Rvn : Rvn ; SquadDic : SquadDic }

type AddPlayerStatus =
    | AddPlayerPending
    | AddPlayerFailed of errorText : string

type AddPlayersState = {
    SquadId : SquadId   
    NewPlayerId : PlayerId
    NewPlayerNameText : string
    NewPlayerNameErrorText : string option
    NewPlayerType : PlayerType
    AddPlayerStatus : AddPlayerStatus option
    ResultRvn : Rvn option }

type ChangePlayerNameStatus =
    | ChangePlayerNamePending
    | ChangePlayerNameFailed of errorText : string

type ChangePlayerNameState = {
    SquadId : SquadId   
    PlayerId : PlayerId
    PlayerNameText : string
    PlayerNameErrorText : string option
    ChangePlayerNameStatus : ChangePlayerNameStatus option }

type ChangePlayerTypeStatus =
    | ChangePlayerTypePending
    | ChangePlayerTypeFailed of errorText : string

type ChangePlayerTypeState = {
    SquadId : SquadId   
    PlayerId : PlayerId
    PlayerType : PlayerType option
    ChangePlayerTypeStatus : ChangePlayerTypeStatus option }

type EliminateSquadStatus =
    | EliminateSquadPending
    | EliminateSquadFailed of errorText : string

type EliminateSquadState = {
    SquadId : SquadId
    EliminateSquadStatus : EliminateSquadStatus option }

type ActiveState = {
    SquadsProjection : SquadsProjection
    CurrentSquadId : SquadId option
    AddPlayersState : AddPlayersState option
    ChangePlayerNameState : ChangePlayerNameState option
    ChangePlayerTypeState : ChangePlayerTypeState option
    EliminateSquadState : EliminateSquadState option }

type ProjectionState =
    | Initializing of currentSquadId : SquadId option
    | InitializationFailed
    | Active of activeState : ActiveState

type State = { ProjectionState : ProjectionState }

let playerNames (playerDic:PlayerDic) = playerDic |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)
