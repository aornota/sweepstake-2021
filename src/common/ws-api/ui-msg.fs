module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
(*open Aornota.Sweepstake2018.Common.Domain.Squad*)
open Aornota.Sweepstake2018.Common.Domain.User

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of sessionId : SessionId * jwt : Jwt

(*type UiUnauthSquadsMsg =
    | InitializeSquadsProjectionQry*)

type UiUnauthMsg =
    | UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg
    (*| UiUnauthSquadsMsg of uiUnauthSquadsMsg : UiUnauthSquadsMsg*)

type UiAuthAppMsg =
    | ChangePasswordCmd of currentRvn : Rvn * password : Password
    | SignOutCmd

(*type UiAuthUserAdminMsg =
    | InitializeUserAdminProjectionQry
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType*)

(*type UiAuthSquadsAdminMsg =
    | AddPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | ChangePlayerNameCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | ChangePlayerTypeCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | WithdrawPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
    | EliminateSquadCmd of squadId : SquadId * currentRvn : Rvn*)

type UiAuthChatMsg =
    // TODO-NMB-HIGH...| InitializeChatProjectionQry
    | SendChatMessageCmd of chatMessage : ChatMessageOLD

type UiAuthMsg =
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    (*| UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg*)
    (*| UiAuthSquadsAdminMsg of uiAuthSquadsAdminMsg : UiAuthSquadsAdminMsg*)
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | Ping
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
