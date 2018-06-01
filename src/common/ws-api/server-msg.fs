module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.Domain.UserAdmin

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
    // TODO-NMB-LOW?... | SessionExpired
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

type UserAdminProjectionMsg =
    | Users4AdminDeltaMsg of deltaRvn : Rvn * delta : Delta<UserId, User4AdminDto>

type ServerUserAdminMsg =
    | InitializeUserAdminProjectionQryResult of result : Result<UserAdminProjectionDto, AuthQryError<string>>
    | CreateUserCmdResult of result : Result<UserName, AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<UserName, AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<UserName, AuthCmdError<string>>
    | UserAdminProjectionMsg of userAdminProjectionMsg : UserAdminProjectionMsg

type NewsProjectionMsg =
    | PostsDeltaMsg of deltaRvn : Rvn * delta : Delta<PostId, PostDto> * hasMorePosts : bool

type ServerNewsMsg =
    | InitializeNewsProjectionQryResult of result : Result<NewsProjectionDto * bool, OtherError<string>>
    | MorePostsQryResult of result : Result<Rvn * PostDto list * bool, OtherError<string>>
    | CreatePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | ChangePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | RemovePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | NewsProjectionMsg of newsProjectionMsg : NewsProjectionMsg

type SquadsProjectionMsg =
    | SquadsDeltaMsg of deltaRvn : Rvn * delta : Delta<SquadId, SquadOnlyDto>
    | PlayersDeltaMsg of deltaRvn : Rvn * squadId : SquadId * squadRvn : Rvn * Delta<PlayerId, PlayerDto>

type ServerSquadsMsg =
    | InitializeSquadsProjectionUnauthQryResult of result : Result<SquadsProjectionDto, OtherError<string>>
    | InitializeSquadsProjectionAuthQryResult of result : Result<SquadsProjectionDto, AuthQryError<string>>
    | AddPlayerCmdResult of result : Result<Rvn * PlayerName, AuthCmdError<string>>
    | ChangePlayerNameCmdResult of result : Result<PlayerName * PlayerName, AuthCmdError<string>>
    | ChangePlayerTypeCmdResult of result : Result<PlayerName, AuthCmdError<string>>
    | WithdrawPlayerCmdResult of result : Result<PlayerName, AuthCmdError<string>>
    | EliminateSquadCmdResult of result : Result<SquadName, AuthCmdError<string>>
    | SquadsProjectionMsg of squadsProjectionMsg : SquadsProjectionMsg

type ChatProjectionMsg =
    | ChatUsersDeltaMsg of deltaRvn : Rvn * delta : Delta<UserId, ChatUserDto>
    | ChatMessagesDeltaMsg of deltaRvn : Rvn * delta : Delta<ChatMessageId, ChatMessageDto> * hasMoreChatMessages : bool
    | UserSignedInMsg of userName : UserName
    | UserSignedOutMsg of userName : UserName

type ServerChatMsg =
    | InitializeChatProjectionQryResult of result : Result<ChatProjectionDto * bool, AuthQryError<string>>
    | MoreChatMessagesQryResult of result : Result<Rvn * ChatMessageDto list * bool, AuthQryError<string>>
    | SendChatMessageCmdResult of result : Result<unit, AuthCmdError<string>>
    | ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg

type ServerMsg =
    | Waff
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    | ServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg
    | ServerNewsMsg of serverNewsMsg : ServerNewsMsg
    | ServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError source errorText = ifDebugSource source errorText |> OtherError |> Error
let otherCmdError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthCmdError |> Error
let otherQryError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthQryError |> Error
