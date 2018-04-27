module Aornota.Sweepstake2018.Common.WsApi.ServerMsg

(*open Aornota.Common.Projection*)

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.Core

type ServerError<'a> = 
    | NotAuthorizedError
    | PersistenceError of errorText : string
    | OtherError of otherError : 'a

type ServerUiMsgError =
    | ReceiveUiMsgError of errorText : string
    | DeserializeUiMsgError of errorText : string

// TODO-NMB-HIGH: More Error types (i.e. instead of using Result<_, string>)?...

// TODO-NMB-HIGH?... type ServerAuthznError = | NotAuthorized

(*type AutoSignOutReason =
    // TODO-NMB-LOW?... | SessionExpired
    | PasswordReset
    | PermissionsChanged of isPersonaNonGrata : bool*)

type ServerAppMsg =
    | ServerUiMsgErrorMsg of serverUiMsgError : ServerUiMsgError
    | ConnectedMsg of otherConnections : int * signedIn : int
    | SignInResultMsg of result : Result<AuthUser, string>
    (*| SignInCmdResult of result : Result<AuthUser, string>*)
    | AutoSignInResultMsg of result : Result<AuthUser, string>
    (*| AutoSignInCmdResult of result : Result<AuthUser, string>*)
    | SignOutResultMsg of result : Result<SessionId, string> // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), use unit?...
    (*| SignOutCmdResult of result : Result<SessionId, string> // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), use unit?...*)
    (*| AutoSignOutMsg of sessionId : SessionId * reason : AutoSignOutReason option // TODO-NMB-HIGH: If no need for SessionId (e.g. as a sanity-check), remove it...*)
    | AutoSignOutMsgOLD of sessionId : SessionId // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedInMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...
    | OtherUserSignedOutMsgOLD of userName : string // TODO-NMB-HIGH: Retire this...

(*type UserAdminProjectionMsg =
    | UserAdminProjectionQryInitialized of userAdmins : Projection<UserAdminDto>
    | UserAdminsDeltaMsg of delta : Delta<UserAdminDto>

type ServerUserAdminMsg =
    | UserAdminProjectionMsg of userAdminProjectionMsg : UserAdminProjectionMsg
    | ResetPasswordCmdResult of result : Result<UserId, string>
    | ChangeUserTypeCmdResult of result : Result<UserId, string>

type ChatProjectionMsg =
    | ChatProjectionQryInitialized of chatUsers : Projection<ChatUserDto> * chatMessages : Projection<ChatMessageDto>
    | ChatUsersDeltaMsg of delta : Delta<ChatUserDto>
    | ChatMessagesDeltaMsg of delta : Delta<ChatMessageDto>
    | OtherUserSignedInMsg of userName : UserName
    | OtherUserSignedOutMsg of userName : UserName*)

type ServerChatMsg =
    (*| ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg
    | SendChatMessageCmdResult of result : Result<ChatMessageId, ChatMessageId * string>*)
    | SendChatMessageResultMsgOLD of result : Result<ChatMessageOLD, ChatMessageId * string> // TODO-NMB-HIGH: Retire this...
    | OtherUserChatMessageMsgOLD of chatMessage : ChatMessageOLD // TODO-NMB-HIGH: Retire this...

type ServerMsg =
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    (*| ServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg*)
    | ServerChatMsg of serverChatMsg : ServerChatMsg
