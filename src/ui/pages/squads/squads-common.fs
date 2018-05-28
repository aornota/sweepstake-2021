module Aornota.Sweepstake2018.UI.Pages.Squads.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System.Collections.Generic

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg
    | ShowGroup of group : Group

// TODO-NEXT: Xyz/s | XyzDic/s | ...

type SquadsProjection = { Rvn : Rvn } // TODO-NEXT: XyzDic/s...

type AddPlayerStatus =
    | AddPlayerPending
    | AddPlayerFailed of errorText : string

type AddPlayerState = {
    SquadId : SquadId   
    NewPlayerId : PlayerId
    NewPlayerNameText : string
    NewPlayerNameErrorText : string option
    NewPlayerType : PlayerType option
    NewPlayerTypeErrorText : string option
    AddPlayerStatus : AddPlayerStatus option
    ResultRvn : Rvn option }

type ActiveState = {
    SquadsProjection : SquadsProjection
    CurrentGroup : Group option
    CurrentSquadId : SquadId option
    AddPlayerState : AddPlayerState option }

type ProjectionState =
    | Initializing
    | InitializationFailed
    | Active of activeState : ActiveState

type State = { ProjectionState : ProjectionState }
