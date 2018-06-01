module Aornota.Sweepstake2018.UI.Pages.News.State

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.News.Common

open System

open Elmish

let initialize isCurrentPage : State * Cmd<Input> =
    let state = {
        ProjectionState = Initializing
        IsCurrentPage = isCurrentPage
        UnseenCount = 0 }
    let cmd = InitializeNewsProjectionQry |> UiUnauthNewsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
    state, cmd

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private post (postDto:PostDto) =
    { Rvn = postDto.Rvn ; UserId = postDto.UserId ; UserName = postDto.UserName ; PostTypeDto = postDto.PostTypeDto ; Timestamp = postDto.Timestamp ; Removed = false }

let private newsProjection (newsProjectionDto:NewsProjectionDto) =
    let postDic = PostDic ()
    newsProjectionDto.PostDtos |> List.iter (fun postDto ->
        if postDto.PostId |> postDic.ContainsKey |> not then // note: silently ignore duplicate PostIds (should never happer)
            (postDto.PostId, postDto |> post) |> postDic.Add)
    { Rvn = initialRvn ; PostDic = postDic }

let private applyPostsDelta currentRvn deltaRvn (delta:Delta<PostId, PostDto>) (postDic:PostDic) =
    let postDic = PostDic postDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (postId, postDto) -> if postId |> postDic.ContainsKey then (postId, postDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (postId, postDto) -> (postId, postDto |> post) |> postDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (postId, postDto) -> if postId |> postDic.ContainsKey |> not then (postId, postDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (postId, postDto) -> postDic.[postId] <- (postDto |> post)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun postId -> if postId |> postDic.ContainsKey |> not then postId |> Some else None)
        // Note: delta.Removed correspond to "removed" - but marked as such on client, rather than removed.
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fun postId ->
            let post = postDic.[postId]
            postDic.[postId] <- { post with Removed = true }) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> postDic |> Ok)

let private cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText

let private handleCreatePostCmdResult (result:Result<unit, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.AddPostState with
    | Some addPostState ->
        match addPostState.AddPostStatus with
        | Some AddPostPending ->
            match result with
            | Ok _ ->
                let activeState = { activeState with AddPostState = None }
                { state with ProjectionState = Active activeState }, "Post has been added" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddPostCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPostState = { addPostState with AddPostStatus = errorText |> AddPostFailed |> Some }
                let activeState = { activeState with AddPostState = addPostState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to add post" |> errorToastCmd
        | Some (AddPostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPostCmdResult when AddPostStatus is not AddPostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPostCmdResult when AddPostState is None -> %A" result)

let private handleChangePostCmdResult (result:Result<unit, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.EditPostState with
    | Some editPostState ->
        match editPostState.EditPostStatus with
        | Some EditPostPending ->
            match result with
            | Ok _ ->
                let activeState = { activeState with EditPostState = None }
                { state with ProjectionState = Active activeState }, "Post has been edited" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePostCmdResult error -> %A" error) (error |> cmdErrorText)
                let editPostState = { editPostState with EditPostStatus = errorText |> EditPostFailed |> Some }
                let activeState = { activeState with EditPostState = editPostState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to edit post" |> errorToastCmd
        | Some (EditPostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePostCmdResult when EditPostStatus is not EditPostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePostCmdResult when EditPostState is None -> %A" result)

let private handleRemovePostCmdResult (result:Result<unit, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.RemovePostState with
    | Some removePostState ->
        match removePostState.RemovePostStatus with
        | Some RemovePostPending ->
            match result with
            | Ok _ ->
                let activeState = { activeState with RemovePostState = None }
                { state with ProjectionState = Active activeState }, "Post has been removed" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "RemovePostCmdResult error -> %A" error) (error |> cmdErrorText)
                let removePostState = { removePostState with RemovePostStatus = errorText |> RemovePostFailed |> Some }
                let activeState = { activeState with RemovePostState = removePostState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to remove post" |> errorToastCmd
        | Some (RemovePostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostCmdResult when RemovePostStatus is not RemovePostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostCmdResult when RemovePostState is None -> %A" result)

let private handleServerNewsMsg serverNewsMsg state : State * Cmd<Input> =
    match serverNewsMsg, state.ProjectionState with
    | InitializeNewsProjectionQryResult (Ok (newsProjectionDto, hasMorePosts)), Initializing ->
        let newsProjection = newsProjectionDto |> newsProjection
        let activeState = {
            NewsProjection = newsProjection
            HasMorePosts = hasMorePosts
            MorePostsPending = false 
            AddPostState = None
            EditPostState = None
            RemovePostState = None }
        let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else newsProjectionDto.PostDtos.Length
        { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
    | InitializeNewsProjectionQryResult (Error (OtherError errorText)), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | MorePostsQryResult (Ok (rvn, postDtos, hasMorePosts)), Active activeState ->
        if activeState.MorePostsPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else if postDtos.Length = 0 then
            let activeState = { activeState with HasMorePosts = hasMorePosts ; MorePostsPending = false }
            { state with ProjectionState = Active activeState }, "Unable to retrieve more news posts<br><br>They have probably been removed" |> warningToastCmd
        else
            let newsProjection = activeState.NewsProjection
            let addedPostDtos = postDtos |> List.map (fun postDto -> postDto.PostId, postDto)
            let postDtoDelta = { Added = addedPostDtos ; Changed = [] ; Removed = [] }
            match newsProjection.PostDic |> applyPostsDelta newsProjection.Rvn rvn postDtoDelta with
            | Ok postDic ->
                let newsProjection = { newsProjection with Rvn = rvn ; PostDic = postDic }
                let activeState = { activeState with NewsProjection = newsProjection ; HasMorePosts = hasMorePosts ; MorePostsPending = false }
                let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else postDtos.Length // note: probably superfluous since will usually be current page
                { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" postDtoDelta newsProjection.PostDic error)
                let state, cmd = initialize state.IsCurrentPage
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | MorePostsQryResult (Error error), Active activeState ->
        if activeState.MorePostsPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else
            let activeState = { activeState with MorePostsPending = false }
            { state with ProjectionState = Active activeState }, shouldNeverHappenCmd (sprintf "MorePostsQryResult Error %A" error)
    | CreatePostCmdResult result, Active activeState ->
        state |> handleCreatePostCmdResult result activeState
    | ChangePostCmdResult result, Active activeState ->
        state |> handleChangePostCmdResult result activeState
    | RemovePostCmdResult result, Active activeState ->
        state |> handleRemovePostCmdResult result activeState
    | NewsProjectionMsg (PostsDeltaMsg (deltaRvn, postDtoDelta, hasMorePosts)), Active activeState ->
        let newsProjection = activeState.NewsProjection
        match newsProjection.PostDic |> applyPostsDelta newsProjection.Rvn deltaRvn postDtoDelta with
        | Ok postDic ->
            let newsProjection = { newsProjection with Rvn = deltaRvn ; PostDic = postDic }
            let activeState = { activeState with NewsProjection = newsProjection ; HasMorePosts = hasMorePosts }
            let unseenCount = state.UnseenCount + if state.IsCurrentPage then 0 else postDtoDelta.Added.Length
            { state with ProjectionState = Active activeState ; UnseenCount = unseenCount }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" postDtoDelta newsProjection.PostDic error)
            let state, cmd = initialize state.IsCurrentPage
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | NewsProjectionMsg _, _ -> // note: silently ignore NewsProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerNewsMsg when %A -> %A" state.ProjectionState serverNewsMsg)

let handleAddPostInput addPostInput activeState state : State * Cmd<Input> * bool =
    match addPostInput, activeState.AddPostState with
    | NewMessageTextChanged newMessageText, Some addPostState ->
        let newMessageErrorText = validatePostMessageText (Markdown newMessageText)
        let addPostState = { addPostState with NewMessageText = newMessageText ; NewMessageErrorText = newMessageErrorText }
        let activeState = { activeState with AddPostState = addPostState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | AddPost, Some addPostState -> // note: assume no need to validate NewMessageText (i.e. because News.Render.renderAddPostModal will ensure that AddPost can only be dispatched when valid)
        let addPostState = { addPostState with AddPostStatus = AddPostPending |> Some }   
        let activeState = { activeState with AddPostState = addPostState |> Some }
        let cmd = (addPostState.NewPostId, Standard, Markdown (addPostState.NewMessageText.Trim ())) |> CreatePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelAddPost, Some addPostState ->
        match addPostState.AddPostStatus with
        | Some AddPostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPost when AddPostPending", false
        | Some (AddPostFailed _) | None ->
            let activeState = { activeState with AddPostState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPostInput when AddPostState is None -> %A" addPostInput), false

let handleEditPostInput editPostInput activeState state : State * Cmd<Input> * bool =
    match editPostInput, activeState.EditPostState with
    | MessageTextChanged messageText, Some editPostState ->
        let messageErrorText = validatePostMessageText (Markdown messageText)
        let editPostState = { editPostState with MessageText = messageText ; MessageErrorText = messageErrorText }
        let activeState = { activeState with EditPostState = editPostState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | EditPost, Some editPostState -> // note: assume no need to validate MessageText (i.e. because News.Render.renderEditPostModal will ensure that EditPost can only be dispatched when valid)
        let editPostState = { editPostState with EditPostStatus = EditPostPending |> Some }
        let activeState = { activeState with EditPostState = editPostState |> Some }
        let postId, postDic = editPostState.PostId, activeState.NewsProjection.PostDic
        let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
        let currentRvn = match post with | Some post -> post.Rvn | None -> initialRvn
        let cmd = (postId, currentRvn, Markdown (editPostState.MessageText.Trim ())) |> ChangePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelEditPost, Some editPostState ->
        match editPostState.EditPostStatus with
        | Some EditPostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelEditPost when EditPostPending", false
        | Some (EditPostFailed _) | None ->
            let activeState = { activeState with EditPostState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EditPostInput when EditPostState is None -> %A" editPostInput), false

let handleRemovePostInput removePostInput activeState state : State * Cmd<Input> * bool =
    match removePostInput, activeState.RemovePostState with
    | ConfirmRemovePost, Some removePostState ->
        let removePostState = { removePostState with RemovePostStatus = RemovePostPending |> Some }   
        let activeState = { activeState with RemovePostState = removePostState |> Some }
        let postId, postDic = removePostState.PostId, activeState.NewsProjection.PostDic
        let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
        let currentRvn = match post with | Some post -> post.Rvn | None -> initialRvn
        let cmd = (postId, currentRvn) |> RemovePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelRemovePost, Some removePostState ->
        match removePostState.RemovePostStatus with
        | Some RemovePostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelRemovePost when RemovePostPending", false
        | Some (RemovePostFailed _) | None ->
            let activeState = { activeState with RemovePostState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostInput when RemovePostState is None -> %A" removePostInput), false

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ShowMarkdownSyntaxModal, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiUnauthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerNewsMsg serverNewsMsg, _ ->
            let state, cmd = state |> handleServerNewsMsg serverNewsMsg
            state, cmd, false
        | ToggleNewsIsCurrentPage isCurrentPage, _ ->
            { state with IsCurrentPage = isCurrentPage ; UnseenCount = if isCurrentPage then 0 else state.UnseenCount }, Cmd.none, false
        | DismissPost postId, Active activeState -> // note: silently ignore unknown postId (should never happen)
            if postId |> activeState.NewsProjection.PostDic.ContainsKey then postId |> activeState.NewsProjection.PostDic.Remove |> ignore
            state, Cmd.none, true
        | MorePosts, Active activeState -> // note: assume no need to validate HasMorePosts (i.e. because News.Render.render will ensure that MorePosts can only be dispatched when true)
            let activeState = { activeState with MorePostsPending = true }
            let cmd = MorePostsQry |> UiUnauthNewsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
            { state with ProjectionState = Active activeState }, cmd, false
        | ShowAddPostModal, Active activeState ->
            let addPostState = { NewPostId = PostId.Create () ; NewMessageText = String.Empty ; NewMessageErrorText = None ; AddPostStatus = None }
            let activeState = { activeState with AddPostState = addPostState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | AddPostInput addPostInput, Active activeState ->
            state |> handleAddPostInput addPostInput activeState
        | ShowEditPostModal postId, Active activeState ->
            let postDic = activeState.NewsProjection.PostDic
            let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
            let messageText =
                match post with
                | Some post -> match post.PostTypeDto with | StandardDto (Markdown messageText) -> messageText
                | None -> String.Empty
            let editPostState = { PostId = postId ; MessageText = messageText ; MessageErrorText = None ; EditPostStatus = None }
            let activeState = { activeState with EditPostState = editPostState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | EditPostInput editPostInput, Active activeState ->
            state |> handleEditPostInput editPostInput activeState
        | ShowRemovePostModal postId, Active activeState -> // note: no need to check for unknown postId (should never happen)
            let removePostState = { PostId = postId ; RemovePostStatus = None }
            let activeState = { activeState with RemovePostState = removePostState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | RemovePostInput removePostInput, Active activeState ->
            state |> handleRemovePostInput removePostInput activeState
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
