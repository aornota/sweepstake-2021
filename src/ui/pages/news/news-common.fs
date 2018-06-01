module Aornota.Sweepstake2018.UI.Pages.News.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System
open System.Collections.Generic

type AddPostInput =
    | NewMessageTextChanged of newMessageText : string
    | AddPost
    | CancelAddPost

type EditPostInput =
    | MessageTextChanged of newMessageText : string
    | EditPost
    | CancelEditPost

type RemovePostInput =
    | ConfirmRemovePost
    | CancelRemovePost

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | ShowMarkdownSyntaxModal
    | SendUiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerNewsMsg of serverNewsMsg : ServerNewsMsg
    | ToggleNewsIsCurrentPage of isCurrentPage : bool
    | DismissPost of postId : PostId
    | MorePosts
    | ShowAddPostModal
    | AddPostInput of addPostInput : AddPostInput
    | ShowEditPostModal of postId : PostId
    | EditPostInput of editPostInput : EditPostInput
    | ShowRemovePostModal of postId : PostId
    | RemovePostInput of removePostInput : RemovePostInput

type Post = { Rvn : Rvn ; UserId : UserId ; UserName : UserName ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset ; Removed : bool }
type PostDic = Dictionary<PostId, Post>

type NewsProjection = { Rvn : Rvn ; PostDic : PostDic }

type AddPostStatus =
    | AddPostPending
    | AddPostFailed of errorText : string

type AddPostState = {
    NewPostId : PostId
    NewMessageText : string
    NewMessageErrorText : string option
    AddPostStatus : AddPostStatus option }

type EditPostStatus =
    | EditPostPending
    | EditPostFailed of errorText : string

type EditPostState = {
    PostId : PostId
    MessageText : string
    MessageErrorText : string option
    EditPostStatus : EditPostStatus option }

type RemovePostStatus =
    | RemovePostPending
    | RemovePostFailed of errorText : string

type RemovePostState = {
    PostId : PostId    
    RemovePostStatus : RemovePostStatus option }

type ActiveState = {
    NewsProjection : NewsProjection
    HasMorePosts : bool
    MorePostsPending : bool
    AddPostState : AddPostState option
    EditPostState : EditPostState option
    RemovePostState : RemovePostState option }

type ProjectionState =
    | Initializing
    | InitializationFailed
    | Active of activeState : ActiveState

type State = {
    ProjectionState : ProjectionState
    IsCurrentPage : bool
    UnseenCount : int }
