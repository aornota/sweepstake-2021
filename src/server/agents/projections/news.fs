module Aornota.Sweepstake2018.Server.Agents.Projections.News

(* Broadcasts: SendMsg
   Subscribes: UsersRead
               NewsRead
               UserEventWritten (UserCreated only)
               NewsEventWritten (PostCreated | PostChanged | PostRemoved)
               Disconnected *)

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Markdown
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.News
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.NewsEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private NewsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnUsersRead of usersRead : UserRead list
    | OnNewsRead of newsRead : NewsRead list
    | OnUserCreated of userId : UserId * userName : UserName
    | OnPostCreated of postId : PostId * userId : UserId * postType : PostType * messageText : Markdown * timestamp : DateTimeOffset
    | OnPostChanged of postId : PostId * rvn : Rvn * messageText : Markdown
    | OnPostRemoved of postId : PostId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeNewsProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<NewsProjectionDto * bool, OtherError<string>>>
    | HandleMorePostsQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<Rvn * PostDto list * bool, OtherError<string>>>
    
type private UserDic = Dictionary<UserId, UserName>

type private Post = { Ordinal : int ; Rvn : Rvn ; UserId : UserId ; UserName : UserName ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset }
type private PostDic = Dictionary<PostId, Post>

type private Projectee = { LastRvn : Rvn ; MinPostOrdinal : int option ; LastHasMorePosts : bool }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { PostDic : PostDic }

type private StateChangeType =
    | Initialization of postDic : PostDic
    | PostChange of postDic : PostDic * state : State

let [<Literal>] private POST_BATCH_SIZE = 5

let private log category = (Projection News, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private postDto (postId, post:Post) = { PostId = postId ; Rvn = post.Rvn ; UserId = post.UserId ; UserName = post.UserName ; PostTypeDto = post.PostTypeDto ; Timestamp = post.Timestamp }

let private newsProjectionDto state = { PostDtos = state.PostDic |> List.ofSeq |> List.map (fun (KeyValue (postId, post)) -> (postId, post) |> postDto) }

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendPostDelta removedOrdinals minPostOrdinal (projecteeDic:ProjecteeDic) (postDelta:Delta<PostId, Post>) =
    let isRelevant projecteeMinPostOrdinal ordinal =
        match projecteeMinPostOrdinal with
        | Some projecteeMinPostOrdinal when projecteeMinPostOrdinal > ordinal -> false
        | Some _ | None -> true
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let hasMorePosts =
            match projectee.MinPostOrdinal, minPostOrdinal with
            | Some projecteeMinPostOrdinal, Some minPostOrdinal when projecteeMinPostOrdinal > minPostOrdinal -> true
            | Some _, Some _ | Some _, None | None, Some _ | None, None -> false
        let addedPostDtos =
            postDelta.Added // note: no need to filter based on projectee.MinPostOrdinal
            |> List.map (fun (postId, post) -> postId, (postId, post) |> postDto)
        let changedPostDtos =
            postDelta.Changed
            |> List.filter (fun (_, post) -> post.Ordinal |> isRelevant projectee.MinPostOrdinal)
            |> List.map (fun (postId, post) -> postId, (postId, post) |> postDto)
        let removedPostIds =
            postDelta.Removed
            |> List.choose (fun postId ->
                match removedOrdinals |> List.tryFind (fun (removedPostId, _) -> removedPostId = postId) with
                | Some (_, ordinal) -> (postId, ordinal) |> Some
                | None -> None) // note: should never happen
            |> List.filter (fun (_, ordinal) -> ordinal |> isRelevant projectee.MinPostOrdinal)
            |> List.map fst
        let postDtoDelta = { Added = addedPostDtos ; Changed = changedPostDtos ; Removed = removedPostIds }
        if postDtoDelta |> isEmpty |> not || hasMorePosts <> projectee.LastHasMorePosts then
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; LastHasMorePosts = hasMorePosts }
            sprintf "sendPostDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, postDtoDelta, hasMorePosts) |> PostsDeltaMsg |> NewsProjectionMsg |> ServerNewsMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization postDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { PostDic = PostDic postDic }
        | PostChange (postDic, state) ->
            let postDelta = postDic |> delta state.PostDic
            if postDelta |> isEmpty |> not then
                let removedOrdinals = postDelta.Removed |> List.choose (fun postId ->
                    if postId |> state.PostDic.ContainsKey |> not then None // note: ignore unknown postId (should never happen)
                    else
                        let post = state.PostDic.[postId]
                        (postId, post.Ordinal) |> Some)
                let minPostOrdinal =
                    if postDic.Count > 0 then postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.min |> Some
                    else None
                sprintf "%s -> Post delta %A -> %i (potential) projectee/s" source postDelta projecteeDic.Count |> Info |> log
                postDelta |> sendPostDelta removedOrdinals minPostOrdinal projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with PostDic = PostDic postDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private tryFindUserName (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey |> not then None // note: silently ignore unknown userId (should never happen)
    else userDic.[userId] |> Some

let private ifAllRead source (usersRead:(UserRead list) option, newsRead:(NewsRead list) option) =
    match usersRead, newsRead with
    | Some usersRead, Some newsRead ->
        let userDic = UserDic ()
        usersRead |> List.iter (fun userRead -> (userRead.UserId, userRead.UserName) |> userDic.Add)
        let postDic = PostDic ()
        newsRead
        |> List.filter (fun newsRead -> newsRead.Removed |> not)
        |> List.sortBy (fun newsRead -> newsRead.Timestamp)
        |> List.iteri (fun i newsRead ->
            match newsRead.UserId |> tryFindUserName userDic with
            | Some userName ->
                let postTypeDto = match newsRead.PostType with | Standard -> newsRead.MessageText |> StandardDto
                let post = { Ordinal = i ; Rvn = newsRead.Rvn ; UserId = newsRead.UserId ; UserName = userName ; PostTypeDto = postTypeDto ; Timestamp = newsRead.Timestamp }
                (newsRead.PostId, post) |> postDic.Add
            | None -> ())
        let projecteeDic = ProjecteeDic ()
        let state = postDic |> Initialization |> updateState source projecteeDic
        (state, userDic, postDic, projecteeDic) |> Some
    | _ -> None

type News () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersAndNewsRead (0 users) (0 posts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersAndNewsRead None None
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnNewsRead _ -> "OnNewsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostCreated _ -> "OnPostCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostChanged _ -> "OnPostChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostRemoved _ -> "OnPostRemoved when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeNewsProjectionQry _ -> "HandleInitializeNewsProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleMorePostsQry _ -> "HandleMorePostsQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersAndNewsRead users news = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | OnUsersRead usersRead ->
                let source = "OnUsersRead"
                sprintf "%s (%i user/s) when pendingOnUsersAndNewsRead" source usersRead.Length |> Info |> log
                let users = usersRead |> Some
                match (users, news) |> ifAllRead source with
                | Some (state, userDic, postDic, projecteeDic) ->
                    return! projectingNews (state, userDic, postDic, projecteeDic)
                | None -> return! pendingOnUsersAndNewsRead users news
            | OnNewsRead newsRead ->
                let source = "OnNewsRead"
                sprintf "%s (%i post/s) when pendingOnUsersAndNewsRead" source newsRead.Length |> Info |> log
                let news = newsRead |> Some
                match (users, news) |> ifAllRead source with
                | Some (state, userDic, postDic, projecteeDic) ->
                    return! projectingNews (state, userDic, postDic, projecteeDic)
                | None -> return! pendingOnUsersAndNewsRead users news
            | OnUserCreated _ -> "OnUserCreated when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | OnPostCreated _ -> "OnPostCreated when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | OnPostChanged _ -> "OnPostChanged when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | OnPostRemoved _ -> "OnPostRemoved when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | RemoveConnections _ -> "RemoveConnections when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | HandleInitializeNewsProjectionQry _ -> "HandleInitializeNewsProjectionQry when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news
            | HandleMorePostsQry _ -> "HandleMorePostsQry when pendingOnUsersAndNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersAndNewsRead users news }
        and projectingNews (state, userDic, postDic, projecteeDic) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingNews" |> IgnoredInput |> Agent |> log ; return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnUsersRead _ -> "OnUsersRead when projectingNews" |> IgnoredInput |> Agent |> log ; return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnNewsRead _ -> "OnNewsRead when projectingNews" |> IgnoredInput |> Agent |> log ; return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnUserCreated (userId, userName) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A) when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source userId userName userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, userName) |> userDic.Add
                sprintf "%s when projectingNews -> %i user/s)" source userDic.Count |> Info |> log
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnPostCreated (postId, userId, postType, messageText, timestamp) ->
                let source = "OnPostCreated"
                sprintf "%s (%A %A) when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source postId userId userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey |> not then // note: silently ignore already-known postId (should never happen)
                        match userId |> tryFindUserName userDic with
                        | Some userName ->
                            let nextOrdinal =
                                if postDic.Count = 0 then 1
                                else (postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.max) + 1
                            let postTypeDto = match postType with | Standard -> messageText |> StandardDto
                            let post = { Ordinal = nextOrdinal ; Rvn = initialRvn ; UserId = userId ; UserName = userName ; PostTypeDto = postTypeDto ; Timestamp = timestamp }
                            (postId, post) |> postDic.Add
                            (postDic, state) |> PostChange |> updateState source projecteeDic
                        | None -> state
                    else state
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnPostChanged (postId, rvn, messageText) ->
                let source = "OnPostChanged"
                sprintf "%s (%A %A) when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source postId rvn userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey then // note: silently ignore unknown postId (should never happen)
                        let post = postDic.[postId]
                        let postTypeDto = match post.PostTypeDto with | StandardDto _ -> messageText |> StandardDto
                        postDic.[postId] <- { post with Rvn = rvn ; PostTypeDto = postTypeDto }
                        (postDic, state) |> PostChange |> updateState source projecteeDic
                    else state
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | OnPostRemoved postId ->
                let source = "OnPostRemoved"
                sprintf "%s (%A) when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source postId userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey then // note: silently ignore unknown postId (should never happen)
                        postId |> postDic.Remove |> ignore
                        (postDic, state) |> PostChange |> updateState source projecteeDic
                    else state
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source connectionIds userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingNews -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | HandleInitializeNewsProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeNewsProjectionQry"
                sprintf "%s for %A when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source connectionId userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                let initializedState, minPostOrdinal, hasMorePosts =
                    if postDic.Count <= POST_BATCH_SIZE then
                        let minPostOrdinal =
                            if postDic.Count > 0 then postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.min |> Some
                            else None
                        state, minPostOrdinal, false
                    else
                        let initialPosts =
                            postDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (postId, post)) -> postId, post)
                            |> List.sortBy (fun (_, post) -> post.Ordinal) |> List.rev |> List.take POST_BATCH_SIZE
                        let minPostOrdinal = initialPosts |> List.map (fun (_, post) -> post.Ordinal) |> List.min |> Some
                        let initialPostDic = PostDic ()
                        initialPosts |> List.iter (fun (postId, post) -> (postId, post) |> initialPostDic.Add)
                        { state with PostDic = initialPostDic }, minPostOrdinal, true
                let projectee = { LastRvn = initialRvn ; MinPostOrdinal = minPostOrdinal ; LastHasMorePosts = hasMorePosts }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingNews -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = (initializedState |> newsProjectionDto, hasMorePosts) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingNews (state, userDic, postDic, projecteeDic)
            | HandleMorePostsQry (connectionId, reply) ->
                let source = "HandleMorePostsQry"
                sprintf "%s for %A when projectingNews (%i user/s) (%i post/s) (%i projectee/s)" source connectionId userDic.Count postDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if connectionId |> projecteeDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" connectionId) UNEXPECTED_ERROR |> OtherError |> Error
                    else
                        let projectee = projecteeDic.[connectionId]
                        let morePosts =
                            postDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (postId, post)) -> postId, post)
                            |> List.filter (fun (_, post) ->
                                match projectee.MinPostOrdinal with
                                | Some minPostOrdinal -> minPostOrdinal > post.Ordinal
                                | None -> true) // note: should never happen                                   
                            |> List.sortBy (fun (_, post) -> post.Ordinal) |> List.rev                       
                        let morePosts, hasMorePosts =
                            if morePosts.Length <= POST_BATCH_SIZE then morePosts, false
                            else morePosts |> List.take POST_BATCH_SIZE, true
                        let minPostOrdinal =
                            if morePosts.Length > 0 then morePosts |> List.map (fun (_, post) -> post.Ordinal) |> List.min |> Some
                            else projectee.MinPostOrdinal // note: should never happen
                        let postDtos = morePosts |> List.map postDto
                        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; MinPostOrdinal = minPostOrdinal ; LastHasMorePosts = hasMorePosts }
                        projecteeDic.[connectionId] <- projectee
                        (projectee.LastRvn, postDtos, hasMorePosts) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingNews (state, userDic, postDic, projecteeDic) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.News |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | NewsRead newsRead -> newsRead |> OnNewsRead |> agent.Post
            | UserEventWritten (_, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, _) -> (userId, userName) |> OnUserCreated |> agent.Post
                | _ -> ()
            | NewsEventWritten (rvn, newsEvent) ->
                match newsEvent with
                | PostCreated (postId, userId, postType, messageText, timestamp) -> (postId, userId, postType, messageText, timestamp) |> OnPostCreated |> agent.Post
                | PostChanged (postId, messageText) -> (postId, rvn, messageText) |> OnPostChanged |> agent.Post
                | PostRemoved postId -> postId |> OnPostRemoved |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten (subset) | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeNewsProjectionQry connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeNewsProjectionQry) |> agent.PostAndAsyncReply
    member __.HandleMorePostsQry connectionId =
        (fun reply -> (connectionId, reply) |> HandleMorePostsQry) |> agent.PostAndAsyncReply

let news = News ()
