module Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.Common.Markdown
open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System
open System.Collections.Generic

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | ShowMarkdownSyntaxModal
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerChatMsg of serverChatMsg : ServerChatMsg
    | ToggleChatIsCurrentPage of isCurrentPage : bool
    | DismissChatMessage of chatMessageId : ChatMessageId
    | NewMessageTextChanged of newMessageText : string
    | MoreChatMessages
    | SendChatMessage

type SendChatMessageStatus =
    | SendChatMessagePending
    | SendChatMessageFailed of errorText : string

type NewChatMessageState = {
    NewChatMessageId : ChatMessageId
    NewMessageText : string
    NewMessageErrorText : string option
    SendChatMessageStatus : SendChatMessageStatus option }

type ChatUser = { UserName : UserName ; LastActivity : DateTimeOffset option }
type ChatUserDic = Dictionary<UserId, ChatUser>

type ChatMessage = { UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Expired : bool }
type ChatMessageDic = Dictionary<ChatMessageId, ChatMessage>

type ChatProjection = { Rvn : Rvn ; ChatUserDic : ChatUserDic ; ChatMessageDic : ChatMessageDic }

type ActiveState = {
    ChatProjection : ChatProjection
    HasMoreChatMessages : bool
    MoreChatMessagesPending : bool
    NewChatMessageState : NewChatMessageState }

type ProjectionState =
    | Initializing
    | InitializationFailed
    | Active of activeState : ActiveState

type State = {
    AuthUser : AuthUser
    ProjectionState : ProjectionState
    IsCurrentPage : bool
    UnseenCount : int }
