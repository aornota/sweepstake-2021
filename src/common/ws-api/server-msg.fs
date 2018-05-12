module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open Aornota.Common.IfDebug
(*open Aornota.Common.Projection*)

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core

type ServerUiMsgError =
    | ReceiveUiMsgError of errorText : string
    | DeserializeUiMsgError of errorText : string

type JwtError = | JwtError of errorText : string

type AuthznError = | NotAuthorized

type PersistenceError = | PersistenceError of errorText : string

type OtherError<'a> = | OtherError of otherError : 'a

type SignInCmdError<'a> =
    | InvalidCredentials of errorText : string option
    | SignInCmdJwtError of jwtError : JwtError
    | OtherSignInCmdError of otherError : OtherError<'a>

type AutoSignInCmdError<'a> =
    | AutoSignInCmdJwtError of jwtError : JwtError
    | OtherAutoSignInCmdError of otherError : OtherError<'a>

type AuthCmdError<'a> =
    | CmdJwtError of jwtError : JwtError
    | CmdAuthznError of authznError : AuthznError
    | CmdPersistenceError of persistenceError : PersistenceError
    | OtherCmdError of otherError : OtherError<'a>

type AuthQryError<'a> =
    | QryJwtError of jwtError : JwtError
    | QryAuthznError of authznError : AuthznError
    | OtherQryError of otherError : OtherError<'a>

type AutoSignOutReason =
    // TODO-NMB-LOW?...| SessionExpired
    | PasswordReset of resetBy : UserName
    | PermissionsChanged of isPersonaNonGrata : bool

type ServerAppMsg =
    | ServerUiMsgErrorMsg of serverUiMsgError : ServerUiMsgError
    | ConnectedMsg of otherConnections : int * signedIn : int
    | SignInCmdResult of result : Result<AuthUser, SignInCmdError<string>>
    | AutoSignInCmdResult of result : Result<AuthUser, AutoSignInCmdError<string>>
    (*| ChangePasswordCmdResult of result : Result<unit, AuthCmdError<string>>*)
    | SignOutCmdResult of result : Result<unit, AuthCmdError<string>>
    | AutoSignOutMsg of reason : AutoSignOutReason option
    | OtherUserSignedInMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedOutMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...

(*type UserAdminProjectionMsg =
    | UserAdminProjectionQryInitialized of userAdmins : Projection<UserAdminDto>
    | UserAdminsDeltaMsg of delta : Delta<UserAdminDto>

type ServerUserAdminMsg =
    | InitializeUserAdminProjectionQryResult of result : Result<unit, AuthQryError<string>>
    | UserAdminProjectionMsg of userAdminProjectionMsg : UserAdminProjectionMsg
    | CreateUserCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<unit, UserId * AuthCmdError<string>>

type ChatProjectionMsg =
    | ChatProjectionQryInitialized of chatUsers : Projection<ChatUserDto> * chatMessages : Projection<ChatMessageDto> // TODO-NMB-HIGH: What if cannot initialize?...
    | ChatUsersDeltaMsg of delta : Delta<ChatUserDto>
    | ChatMessagesDeltaMsg of delta : Delta<ChatMessageDto>
    | OtherUserSignedInMsg of userName : UserName
    | OtherUserSignedOutMsg of userName : UserName*)

type ServerChatMsg =
    (*| InitializeChatProjectionQryResult of result : Result<unit, AuthQryError<string>>
    | ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg
    | SendChatMessageCmdResult of result : Result<unit, ChatMessageId * AuthCmdError<string>>*)
    | SendChatMessageResultMsgOLD of result : Result<ChatMessageOLD, ChatMessageId * string> // TODO-NMB-HIGH: Retire this...
    | OtherUserChatMessageMsgOLD of chatMessage : ChatMessageOLD // TODO-NMB-HIGH: Retire this...

type ServerMsg =
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    (*| ServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> Error
let otherCmdError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> OtherCmdError |> Error
let otherQryError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> OtherQryError |> Error
