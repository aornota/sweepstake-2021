module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Common.Markdown
open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of sessionId : SessionId * jwt : Jwt

type UiUnauthSquadsMsg =
    | InitializeSquadsProjectionUnauthQry

type UiUnauthMsg =
    | UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg
    | UiUnauthSquadsMsg of uiUnauthSquadsMsg : UiUnauthSquadsMsg

type UiAuthAppMsg =
    | SignOutCmd
    | ChangePasswordCmd of currentRvn : Rvn * password : Password

(*type UiAuthUserAdminMsg =
    | InitializeUserAdminProjectionQry
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType*)

type UiAuthSquadsMsg =
    | InitializeSquadsProjectionAuthQry
    | AddPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    // TODO-NEXT... | ChangePlayerNameCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    // TODO-NEXT... | ChangePlayerTypeCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    // TODO-NEXT... | WithdrawPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
    // TODO-NEXT... | EliminateSquadCmd of squadId : SquadId * currentRvn : Rvn

type UiAuthChatMsg =
    | InitializeChatProjectionQry
    | MoreChatMessagesQry
    | SendChatMessageCmd of chatMessageId : ChatMessageId * messageText : Markdown

type UiAuthMsg =
    | UserNonApiActivity
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    (*| UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg*)
    | UiAuthSquadsMsg of uiAuthSquadsMsg : UiAuthSquadsMsg
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | Wiff
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
