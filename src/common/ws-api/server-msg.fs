module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open Aornota.Common.IfDebug
(*open Aornota.Common.Projection*)

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
(*open Aornota.Sweepstake2018.Common.Domain.Squad*)
open Aornota.Sweepstake2018.Common.Domain.User

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
    | AuthCmdJwtError of jwtError : JwtError
    | AuthCmdAuthznError of authznError : AuthznError
    | AuthCmdPersistenceError of persistenceError : PersistenceError
    | OtherAuthCmdError of otherError : OtherError<'a>

type AuthQryError<'a> =
    | AuthQryJwtError of jwtError : JwtError
    | AuthQryAuthznError of authznError : AuthznError
    | OtherAuthQryError of otherError : OtherError<'a>

type AutoSignOutReason =
    // TODO-NMB-LOW?...| SessionExpired
    | PasswordReset
    | PermissionsChanged of isPersonaNonGrata : bool

type ServerAppMsg =
    | ServerUiMsgErrorMsg of serverUiMsgError : ServerUiMsgError
    | ConnectedMsg of otherConnectionCount : int * signedInUserCount : int
    | SignInCmdResult of result : Result<AuthUser, SignInCmdError<string>>
    | AutoSignInCmdResult of result : Result<AuthUser, AutoSignInCmdError<string>>
    | ChangePasswordCmdResult of result : Result<Rvn, AuthCmdError<string>>
    | SignOutCmdResult of result : Result<unit, AuthCmdError<string>>
    | AutoSignOutMsg of reason : AutoSignOutReason option
    | OtherUserSignedInMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedOutMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...

(*type UserAdministrationProjectionMsg =
    | UserAdministrationProjectionQryInitialized of userAdmins : Projection<UserAdminDto>
    | UserAdministrationDeltaMsg of delta : Delta<UserAdminDto>*)

(*type ServerUserAdministrationMsg =
    | InitializeUserAdministrationProjectionQryResult of result : Result<unit, AuthQryError<string>>
    | UserAdministrationProjectionMsg of userAdministrationProjectionMsg : UserAdministrationProjectionMsg
    | CreateUserCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<unit, UserId * AuthCmdError<string>>*)

(*type SquadsProjectionMsg =
    | SquadsProjectionQryInitialized of squads : Projection<_>
    | SquadsProjectionDeltaMsg of delta : Delta<_>*)

(*type ServerSquadsMsg =
    | InitializeSquadsProjectionQryResult of result : Result<unit, AuthQryError<string>>
    | SquadsProjectionMsg of squadsProjectionMsg : SquadsProjectionMsg
    | ChangePlayerNameCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | ChangePlayerTypeCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | WithdrawPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | AddPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | EliminateSquadCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>*)

(*type ChatProjectionMsg =
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
    (*| ServerUserAdministrationMsg of serverUserAdministrationMsg : ServerUserAdministrationMsg*)
    (*| ServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> Error
let otherCmdError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> OtherAuthCmdError |> Error
let otherQryError debugSource errorText = ifDebugSource debugSource errorText |> OtherError |> OtherAuthQryError |> Error
