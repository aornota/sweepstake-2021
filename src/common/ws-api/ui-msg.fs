module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Common.Markdown
open Aornota.Common.Revision

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
    | SignOutCmd
    | ChangePasswordCmd of currentRvn : Rvn * password : Password

(*type UiAuthUserAdministrationMsg =
    | InitializeUserAdministrationProjectionQry
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType*)

(*type UiAuthSquadsAdministrationMsg =
    | AddPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | ChangePlayerNameCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | ChangePlayerTypeCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | WithdrawPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
    | EliminateSquadCmd of squadId : SquadId * currentRvn : Rvn*)

type UiAuthChatMsg =
    | InitializeChatProjectionQry
    | SendChatMessageCmd of chatMessageId : ChatMessageId * messageText : Markdown

type UiAuthMsg =
    | UserNonApiActivity
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    (*| UiAuthUserAdministrationMsg of uiAuthUserAdministrationMsg : UiAuthUserAdministrationMsg*)
    (*| UiAuthSquadsAdministrationMsg of uiAuthSquadsAdministrationMsg : UiAuthSquadsAdministrationMsg*)
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | Wiff
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
