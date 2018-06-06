module Aornota.Sweepstake2018.UI.Pages.Squads.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

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

type WithdrawPlayerInput =
    | ConfirmWithdrawPlayer
    | CancelWithdrawPlayer

type EliminateSquadInput =
    | ConfirmEliminateSquad
    | CancelEliminateSquad

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg
    | ShowGroup of group : Group
    | ShowSquad of squadId : SquadId
    | AddToDraft of draftId : DraftId * userDraftPickBasic : UserDraftPickBasic
    | RemoveFromDraft of draftId : DraftId * userDraftPickBasic : UserDraftPickBasic
    | ShowAddPlayersModal of squadId : SquadId
    | AddPlayersInput of addPlayersInput : AddPlayersInput
    | ShowChangePlayerNameModal of squadId : SquadId * playerId : PlayerId
    | ChangePlayerNameInput of changePlayerNameInput : ChangePlayerNameInput
    | ShowChangePlayerTypeModal of squadId : SquadId * playerId : PlayerId
    | ChangePlayerTypeInput of changePlayerTypeInput : ChangePlayerTypeInput
    | ShowWithdrawPlayerModal of squadId : SquadId * playerId : PlayerId
    | WithdrawPlayerInput of withdrawPlayerInput : WithdrawPlayerInput
    | ShowEliminateSquadModal of squadId : SquadId
    | EliminateSquadInput of eliminateSquadInput : EliminateSquadInput

type DraftPickStatus = | AddPending | RemovePending

// TODO-SOON: Add Rank [option?], i.e. so can show in "Selected for...draft" tag?...
type DraftPick = { UserDraftPickBasic : UserDraftPickBasic ; DraftPickStatus : DraftPickStatus option }

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

type WithdrawPlayerStatus =
    | WithdrawPlayerPending
    | WithdrawPlayerFailed of errorText : string

type WithdrawPlayerState = {
    SquadId : SquadId
    PlayerId : PlayerId
    WithdrawPlayerStatus : WithdrawPlayerStatus option }

type EliminateSquadStatus =
    | EliminateSquadPending
    | EliminateSquadFailed of errorText : string

type EliminateSquadState = {
    SquadId : SquadId
    EliminateSquadStatus : EliminateSquadStatus option }

type State = {
    CurrentDraftPicks : DraftPick list
    CurrentSquadId : SquadId option
    AddPlayersState : AddPlayersState option
    ChangePlayerNameState : ChangePlayerNameState option
    ChangePlayerTypeState : ChangePlayerTypeState option
    WithdrawPlayerState : WithdrawPlayerState option
    EliminateSquadState : EliminateSquadState option }

let isAddPending draftPick =
    match draftPick.DraftPickStatus with
    | Some draftPickStatus -> match draftPickStatus with | AddPending -> true | RemovePending -> false
    | None -> false
let isRemovePending draftPick =
    match draftPick.DraftPickStatus with
    | Some draftPickStatus -> match draftPickStatus with | AddPending -> false | RemovePending -> true
    | None -> false
