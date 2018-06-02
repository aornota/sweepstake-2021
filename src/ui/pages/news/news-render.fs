module Aornota.Sweepstake2018.UI.Pages.News.Render

open Aornota.Common.Markdown
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.Render.Markdown
#if TICK
open Aornota.UI.Common.TimestampHelper
#endif
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.News.Common

open System

module Rct = Fable.Helpers.React

let [<Literal>] private REMOVED_MARKDOWN = "_This post has been removed_"

let private renderAddPostModal (useDefaultTheme, addPostState:AddPostState) dispatch =
    let theme = getTheme useDefaultTheme
    let onDismiss = match addPostState.AddPostStatus with | Some AddPostPending -> None | Some _ | None -> (fun _ -> CancelAddPost |> AddPostInput |> dispatch) |> Some
    let isAddingPost, addPostInteraction =
        match addPostState.AddPostStatus with
        | Some AddPostPending -> true, Loading
        | Some (AddPostFailed _) | None ->
            match validatePostMessageText (Markdown addPostState.NewMessageText) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> AddPost |> AddPostInput |> dispatch), None)
    let errorText = match addPostState.AddPostStatus with | Some (AddPostFailed errorText) -> errorText |> Some | Some AddPostPending | None -> None
    let (PostId newPostKey), newMessageText = addPostState.NewPostId, addPostState.NewMessageText
    let helpInfo = [
        str "News posts are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme newPostKey newMessageText addPostState.NewMessageErrorText helpInfo true isAddingPost (NewMessageTextChanged >> AddPostInput >> dispatch)
            if String.IsNullOrWhiteSpace newMessageText |> not then
                yield notification theme notificationBlack [ Markdown newMessageText |> notificationContentFromMarkdown theme ] ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Add post" ] |> button theme { buttonLinkSmall with Interaction = addPostInteraction } ] ]
    cardModal theme [ [ bold "Add post" ] |> para theme paraCentredSmall ] onDismiss body

let private renderEditPostModal (useDefaultTheme, editPostState:EditPostState) dispatch =
    let theme = getTheme useDefaultTheme
    let onDismiss = match editPostState.EditPostStatus with | Some EditPostPending -> None | Some _ | None -> (fun _ -> CancelEditPost |> EditPostInput |> dispatch) |> Some
    let isEditingPost, editPostInteraction =
        match editPostState.EditPostStatus with
        | Some EditPostPending -> true, Loading
        | Some (EditPostFailed _) | None ->
            match validatePostMessageText (Markdown editPostState.MessageText) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> EditPost |> EditPostInput |> dispatch), None)
    let errorText = match editPostState.EditPostStatus with | Some (EditPostFailed errorText) -> errorText |> Some | Some EditPostPending | None -> None
    let (PostId postKey), messageText = editPostState.PostId, editPostState.MessageText
    let helpInfo = [
        str "News posts are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme postKey messageText editPostState.MessageErrorText helpInfo true isEditingPost (MessageTextChanged >> EditPostInput >> dispatch)
            if String.IsNullOrWhiteSpace messageText |> not then
                yield notification theme notificationBlack [ Markdown messageText |> notificationContentFromMarkdown theme ] ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Edit post" ] |> button theme { buttonLinkSmall with Interaction = editPostInteraction } ] ]
    cardModal theme [ [ bold "Edit post" ] |> para theme paraCentredSmall ] onDismiss body

let private renderRemovePostModal (useDefaultTheme, postDic:PostDic, removePostState:RemovePostState) dispatch =
    let theme = getTheme useDefaultTheme
    let postId = removePostState.PostId
    let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
    let messageText =
        match post with
        | Some post -> match post.PostTypeDto with | StandardDto messageText -> messageText
        | None -> Markdown String.Empty
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmRemovePost |> dispatch)
        let cancel = (fun _ -> CancelRemovePost |> dispatch)
        match removePostState.RemovePostStatus with
        | Some RemovePostPending -> Loading, None
        | Some (RemovePostFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match removePostState.RemovePostStatus with | Some (RemovePostFailed errorText) -> errorText |> Some | Some RemovePostPending | None -> None
    let warning = [
        [ str "Are you sure you want to remove this post?" ] |> para theme paraCentredSmallest
        [ str "Please note that this action is irreversible" ] |> para theme paraCentredSmallest ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield notification theme notificationLight [ messageText |> notificationContentFromMarkdown theme ]
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove post" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme [ [ bold "Remove post" ] |> para theme paraCentredSmall ] onDismiss body

let private renderPost theme authUser dispatch (postId, post) =
    let editOrRemovePost =
        match post.Removed, authUser with
        | false, Some authUser ->
            match authUser.Permissions.NewsPermissions with
            | Some newsPermissions ->
                match newsPermissions.EditOrRemovePostPermission with
                | Some userId when userId = post.UserId ->
                    let editPost = [ [ str "Edit post" ] |> para theme paraDefaultSmallest ] |> link theme (ClickableLink (fun _ -> postId |> ShowEditPostModal |> dispatch))
                    let removePost = [ [ str "Remove post" ] |> para theme paraDefaultSmallest ] |> link theme (ClickableLink (fun _ -> postId |> ShowRemovePostModal |> dispatch))
                    (editPost, removePost) |> Some
                | Some _ | None -> None
            | None -> None
        | _ -> None
    let renderChildren post = [
        let rightItem =
            let timestampText =
#if TICK
                ago post.Timestamp.LocalDateTime
#else
                post.Timestamp.LocalDateTime.ToString ("HH:mm:ss")
#endif
            [ str timestampText ] |> para theme paraDefaultSmallest
        let (UserName userName) = post.UserName
        let messageText = if post.Removed then Markdown REMOVED_MARKDOWN else match post.PostTypeDto with | StandardDto messageText -> messageText
        yield level true [
            levelLeft [ levelItem [ [ bold userName ; str " posted" ] |> para theme paraDefaultSmallest ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield messageText |> notificationContentFromMarkdown theme
        match editOrRemovePost with
        | Some (editPost, removePost) ->
        yield level true [ levelLeft [ levelItem [ editPost ] ] ; levelRight [ levelItem [ removePost ] ] ]
        | None -> () ]
    let semantic = if post.Removed then Light else Black
    let children = renderChildren post
    let onDismissNotification = if post.Removed then (fun _ -> postId |> DismissPost |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        notification theme { notificationDefault with NotificationSemantic = semantic |> Some ; OnDismissNotification = onDismissNotification } children
    ]

let private addPost theme authUser dispatch =
    match authUser with
    | Some authUser ->
        match authUser.Permissions.NewsPermissions with
        | Some newsPermissions ->
            if newsPermissions.CreatePostPermission then
                [ [ str "Add post" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink (fun _ -> ShowAddPostModal |> dispatch)) |> Some
            else None
        | None -> None
    | None -> None

let render (useDefaultTheme, state, authUser:AuthUser option, hasModal, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "News" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ProjectionState with
        | Initializing ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | InitializationFailed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Active activeState ->
            let postDic = activeState.NewsProjection.PostDic
            let morePosts =
                let paraMore = { paraDefaultSmallest with ParaAlignment = RightAligned }
                if activeState.MorePostsPending then
                    [ br ; [ str "Retrieving more posts... " ; icon iconSpinnerPulseSmall ] |> para theme paraMore ]
                else if activeState.HasMorePosts then
                    [ br ; [ [ str "More posts" ] |> link theme (ClickableLink (fun _ -> MorePosts |> dispatch)) ] |> para theme paraMore ]
                else []
            match hasModal, activeState.AddPostState with
            | false, Some addPostState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddPostModal (useDefaultTheme, addPostState) dispatch ]
            | _ -> ()
            match hasModal, activeState.EditPostState with
            | false, Some editPostState ->
                yield div divDefault [ lazyViewOrHMR2 renderEditPostModal (useDefaultTheme, editPostState) dispatch ]
            | _ -> ()
            match hasModal, activeState.RemovePostState with
            | false, Some removePostState ->
                yield div divDefault [ lazyViewOrHMR2 renderRemovePostModal (useDefaultTheme, postDic, removePostState) (RemovePostInput >> dispatch) ]
            | _ -> ()
            yield Rct.ofOption (addPost theme authUser dispatch)           
            yield! activeState.NewsProjection.PostDic
                |> List.ofSeq
                |> List.map (fun (KeyValue (postId, post)) -> (postId, post))
                |> List.sortBy (fun (_, post) -> post.Timestamp)
                |> List.rev
                |> List.map (renderPost theme authUser dispatch)
                |> List.collect id
            yield! morePosts ]
