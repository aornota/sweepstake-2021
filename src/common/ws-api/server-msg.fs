module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

(*open Aornota.Common.Projection*)

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core

type JwtError = | InvalidJwt // note: used for various cases, e.g. cannot decrypt Jwt | decrypted does not match cached | &c.

type AuthznError = | NotAuthorized // note: used for various cases

type PersistenceError = | PersistenceError of errorText : string

type OtherError<'a> = | OtherError of otherError : 'a

type AuthCmdError<'a> =
    | CmdJwtError of jwtError : JwtError
    | CmdAuthznError of authznError : AuthznError
    | CmdPersistenceError of persistenceError : PersistenceError
    | OtherCmdError of otherError : OtherError<'a>

type AuthQryError<'a> =
    | QtyJwtError of jwtError : JwtError
    | QtyAuthznError of authznError : AuthznError
    | OtherQryError of otherError : OtherError<'a>

type ServerUiMsgError =
    | ReceiveUiMsgError of errorText : string
    | DeserializeUiMsgError of errorText : string

type SignInCmdError = | InvalidCredentials // note: used for various cases, e.g. unknown userName | multiple userName matches [should never happen] | incorrect password | PersonaNonGrata

(*type AutoSignOutReason =
    // TODO-NMB-LOW?... | SessionExpired
    | PasswordReset
    | PermissionsChanged of isPersonaNonGrata : bool*)

type ServerAppMsg =
    | ServerUiMsgErrorMsg of serverUiMsgError : ServerUiMsgError
    | ConnectedMsg of otherConnections : int * signedIn : int
    | SignInResultMsg of result : Result<AuthUser, string>
    (*| SignInCmdResult of result : Result<AuthUser, SignInCmdError>*)
    | AutoSignInResultMsg of result : Result<AuthUser, string>
    (*| AutoSignInCmdResult of result : Result<AuthUser, JwtError>*)
    (*| ChangePasswordCmdResult of result : Result<unit, AuthCmdError<string>>*)
    | SignOutResultMsg of result : Result<SessionId, string> // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), use unit?...
    (*| SignOutCmdResult of result : Result<SessionId, AuthCmdError<string>> // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), use unit?...*)
    (*| AutoSignOutMsg of sessionId : SessionId * reason : AutoSignOutReason option // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), remove it...*)
    | AutoSignOutMsgOLD of sessionId : SessionId // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedInMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedOutMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...

(*type UserAdminProjectionMsg =
    | UserAdminProjectionQryInitialized of userAdmins : Projection<UserAdminDto> // TODO-NMB-HIGH: What if cannot initialize?...
    | UserAdminsDeltaMsg of delta : Delta<UserAdminDto>

type ServerUserAdminMsg =
    | UserAdminProjectionMsg of userAdminProjectionMsg : UserAdminProjectionMsg
    | CreateUserCmdResult of result : Result<UserId, UserId * AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<UserId, UserId * AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<UserId, UserId * AuthCmdError<string>>

type ChatProjectionMsg =
    | ChatProjectionQryInitialized of chatUsers : Projection<ChatUserDto> * chatMessages : Projection<ChatMessageDto> // TODO-NMB-HIGH: What if cannot initialize?...
    | ChatUsersDeltaMsg of delta : Delta<ChatUserDto>
    | ChatMessagesDeltaMsg of delta : Delta<ChatMessageDto>
    | OtherUserSignedInMsg of userName : UserName
    | OtherUserSignedOutMsg of userName : UserName*)

type ServerChatMsg =
    (*| ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg
    | SendChatMessageCmdResult of result : Result<ChatMessageId, ChatMessageId * AuthCmdError<string>>*)
    | SendChatMessageResultMsgOLD of result : Result<ChatMessageOLD, ChatMessageId * string> // TODO-NMB-HIGH: Retire this...
    | OtherUserChatMessageMsgOLD of chatMessage : ChatMessageOLD // TODO-NMB-HIGH: Retire this...

type ServerMsg =
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    (*| ServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError errorSource errorText = Error (OtherError (sprintf "%s: %s" errorSource errorText))
let otherCmdError errorSource errorText = Error (OtherCmdError (OtherError (sprintf "%s: %s" errorSource errorText)))
let otherQryError errorSource errorText = Error (OtherQryError (OtherError (sprintf "%s: %s" errorSource errorText)))
