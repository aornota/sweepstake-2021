module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core

(*type Password = | Password of password : string

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of jwt : Jwt*)

type UiUnauthMsg =
    (*| UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg*)
    | SignInMsgOLD of sessionId : SessionId * userName : string * password : string // TODO-NMB-HIGH: Retire this...
    | AutoSignInMsgOLD of jwt : Jwt // TODO-NMB-HIGH: Retire this...

(*type UiAuthAppMsg =
    | ChamgePasswordCmd of password : Password
    | SignOutCmd

type UiAuthUserAdminMsg =
    | InitializeUserAdminProjectionQry
    | ResetPasswordCmd of userId : UserId * rvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * rvn : Rvn * userType : UserType

type UiAuthChatMsg =
    | InitializeChatProjectionQry
    | SendChatMessageCmd of chatMessage : ChatMessage*)

type UiAuthMsg =
    (*| UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    | UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg*)
    | SignOutMsgOLD // TODO-NMB-HIGH: Retire this...
    | SendChatMessageMsgOLD of chatMessage : ChatMessageOLD // TODO-NMB-HIGH: Retire this...

type UiMsg =
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
