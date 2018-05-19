module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open Aornota.Common.IfDebug
(*open Aornota.Common.Projection*)

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
(*open Aornota.Sweepstake2018.Common.Domain.Squad*)
open Aornota.Sweepstake2018.Common.Domain.User

open System

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
    | ConnectedMsg of startedOffset : DateTimeOffset * otherConnectionCount : int * signedInUserCount : int
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
    | SquadsProjectionDeltaMsg of delta : Delta<_>*)

(*type ServerSquadsMsg =
    | InitializeSquadsProjectionQryResult of result : Result<TODO, AuthQryError<string>>
    | SquadsProjectionMsg of squadsProjectionMsg : SquadsProjectionMsg
    | ChangePlayerNameCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | ChangePlayerTypeCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | WithdrawPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | AddPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | EliminateSquadCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>*)

(*type ChatProjectionMsg =
    | ChatUsersDeltaMsg of delta : Delta<ChatUserDto>
    | ChatMessagesDeltaMsg of delta : Delta<ChatMessageDto>
    | OtherUserSignedInMsg of userName : UserName
    | OtherUserSignedOutMsg of userName : UserName*)

type ServerChatMsg =
    | InitializeChatProjectionQryResult of result : Result<unit, AuthQryError<string>> // TODO-NEXT: Something other than unit...
    (*| ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg*)
    | SendChatMessageCmdResult of result : Result<unit, ChatMessageId * AuthCmdError<string>>
    | SendChatMessageResultMsgOLD of result : Result<ChatMessageOLD, ChatMessageId * string> // TODO-REMOVE: Once no longer used...
    | OtherUserChatMessageMsgOLD of chatMessage : ChatMessageOLD // TODO-REMOVE: Once no longer used...

type ServerMsg =
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    (*| ServerUserAdministrationMsg of serverUserAdministrationMsg : ServerUserAdministrationMsg*)
    (*| ServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError source errorText = ifDebugSource source errorText |> OtherError |> Error
let otherCmdError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthCmdError |> Error
let otherQryError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthQryError |> Error
