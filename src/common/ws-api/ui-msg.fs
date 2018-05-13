module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of sessionId : SessionId * jwt : Jwt

type UiUnauthMsg =
    | UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg

type UiAuthAppMsg =
    | ChangePasswordCmd of currentRvn : Rvn * password : Password
    | SignOutCmd

(*type UiAuthUserAdminMsg =
    | InitializeUserAdminProjectionQry
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType*)

type UiAuthChatMsg =
    // TODO-NMB-HIGH...| InitializeChatProjectionQry
    | SendChatMessageCmd of chatMessage : ChatMessageOLD

type UiAuthMsg =
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    (*| UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg*)
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
