module Aornota.Sweepstake2018.Common.WsApi.UiMsg

open Aornota.Common.Markdown
open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of sessionId : SessionId * jwt : Jwt

type UiUnauthNewsMsg =
    | InitializeNewsProjectionQry
    | MorePostsQry

type UiUnauthSquadsMsg = | InitializeSquadsProjectionUnauthQry

type UiUnauthFixturesMsg = | InitializeFixturesProjectionQry

type UiUnauthMsg =
    | UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg
    | UiUnauthNewsMsg of uiUnauthNewsMsg : UiUnauthNewsMsg
    | UiUnauthSquadsMsg of uiUnauthSquadsMsg : UiUnauthSquadsMsg
    | UiUnauthFixturesMsg of uiUnauthFixturesMsg : UiUnauthFixturesMsg

type UiAuthAppMsg =
    | SignOutCmd
    | ChangePasswordCmd of currentRvn : Rvn * password : Password

type UiAuthUserAdminMsg =
    | InitializeUserAdminProjectionQry
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType

type UiAuthNewsMsg =
    | CreatePostCmd of postId : PostId * postType : PostType * messageText : Markdown
    | ChangePostCmd of postId : PostId * currentRvn : Rvn * messageText : Markdown
    | RemovePostCmd of postId : PostId * currentRvn : Rvn

type UiAuthSquadsMsg =
    | InitializeSquadsProjectionAuthQry
    | AddPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | ChangePlayerNameCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | ChangePlayerTypeCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | WithdrawPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
    | EliminateSquadCmd of squadId : SquadId * currentRvn : Rvn

type UiAuthFixturesMsg = | ConfirmParticipantCmd of fixtureId : FixtureId * currentRvn : Rvn * role : Role * squadId : SquadId

type UiAuthChatMsg =
    | InitializeChatProjectionQry
    | MoreChatMessagesQry
    | SendChatMessageCmd of chatMessageId : ChatMessageId * messageText : Markdown

type UiAuthMsg =
    | UserNonApiActivity
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    | UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg
    | UiAuthNewsMsg of uiAuthNewsMsg : UiAuthNewsMsg
    | UiAuthSquadsMsg of uiAuthSquadsMsg : UiAuthSquadsMsg
    | UiAuthFixturesMsg of uiAuthFixturesMsg : UiAuthFixturesMsg
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | Wiff
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
