module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Chat
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

(*type UserAdministrationProjectionMsg =
    | UserAdministrationDeltaMsg of deltaRvn : Rvn * delta : Delta<UserId, UserAdministrationDto>*)

(*type ServerUserAdministrationMsg =
    | InitializeUserAdministrationProjectionQryResult of result : Result<UserAdministrationProjectionDto, AuthQryError<string>>
    | UserAdministrationProjectionMsg of userAdministrationProjectionMsg : UserAdministrationProjectionMsg
    | CreateUserCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<unit, UserId * AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<unit, UserId * AuthCmdError<string>>*)

(*type SquadsProjectionMsg =
    | SquadsDeltaMsg of deltaRvn : Rvn * delta : Delta<SquadId, SquadDto>
    | PlayersDeltaMsg of deltaRvn : Rvn * squadId : SquadId * Delta<PlayerId, PlayerDto>*)

(*type ServerSquadsMsg =
    | InitializeSquadsProjectionQryResult of result : Result<SquadsProjectionDto, AuthQryError<string>>
    | SquadsProjectionMsg of squadsProjectionMsg : SquadsProjectionMsg
    | ChangePlayerNameCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | ChangePlayerTypeCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | WithdrawPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | AddPlayerCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>
    | EliminateSquadCmdResult of result : Result<unit, SquadId * AuthCmdError<string>>*)

type ChatProjectionMsg =
    | ChatUsersDeltaMsg of deltaRvn : Rvn * delta : Delta<UserId, ChatUserDto>
    | ChatMessagesDeltaMsg of deltaRvn : Rvn * delta : Delta<ChatMessageId, ChatMessageDto>
    | UserSignedInMsg of userName : UserName
    | UserSignedOutMsg of userName : UserName

type ServerChatMsg =
    | InitializeChatProjectionQryResult of result : Result<ChatProjectionDto, AuthQryError<string>>
    | ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg
    | SendChatMessageCmdResult of result : Result<unit, ChatMessageId * AuthCmdError<string>>

type ServerMsg =
    | Waff
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    (*| ServerUserAdministrationMsg of serverUserAdministrationMsg : ServerUserAdministrationMsg*)
    (*| ServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError source errorText = ifDebugSource source errorText |> OtherError |> Error
let otherCmdError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthCmdError |> Error
let otherQryError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthQryError |> Error
