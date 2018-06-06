module Aornota.Sweepstake2018.UI.Pages.Chat.Render

open Aornota.Common.Markdown
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.Render.Markdown
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.Chat.Common
open Aornota.Sweepstake2018.UI.Shared

open System

let [<Literal>] private RECENTLY_ACTIVE = 5.<minute>

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private (|Self|RecentlyActive|SignedIn|NotSignedIn|) (authUserId:UserId, userId:UserId, userAuthDto:UserAuthDto) =
    if userId = authUserId then Self
    else
        match userAuthDto.LastActivity with
        | Some lastApi ->   
            let recentlyActiveCutoff = cutoff (int (RECENTLY_ACTIVE |> minutesToSeconds) * 1<second>)
            if lastApi > recentlyActiveCutoff then RecentlyActive else SignedIn
        | None -> NotSignedIn

let private semanticAndSortOrder authUserId (userId, userAuthDto) =
    match userAuthDto with
    | Some userAuthDto ->
        match (authUserId, userId, userAuthDto) with
        | Self -> Link, 0
        | RecentlyActive -> Success, 1
        | SignedIn -> Primary, 2
        | NotSignedIn -> Dark, 3
    | None -> Light, 5 // note: should never happen

// #region renderChatMessage
let private renderChatMessage theme authUserId (userDic:UserDic) dispatch (chatMessageId, chatMessage) =
    let renderChildren (UserName userName) messageText (timestamp:DateTimeOffset) = [
        let rightItem =
            let timestampText =
                if chatMessage.Expired then "expired"
                else
#if TICK
                    ago timestamp.LocalDateTime
#else
                    timestamp.LocalDateTime |> dateAndTimeText
#endif
            [ str timestampText ] |> para theme paraDefaultSmallest
        yield level true [
            levelLeft [ levelItem [ [ bold userName ; str " says" ] |> para theme paraDefaultSmallest ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield messageText |> notificationContentFromMarkdown theme ]
    let userId, userAuthDto =
        let userId = chatMessage.UserId
        if userId |> userDic.ContainsKey then
            let (_, userDtoAuth) = userDic.[userId]
            userId, userDtoAuth
        else userId, None
    let semantic, _ = (userId, userAuthDto) |> semanticAndSortOrder authUserId
    let children = renderChildren (userId |> userName userDic) chatMessage.MessageText chatMessage.Timestamp
    let onDismissNotification = if chatMessage.Expired then (fun _ -> chatMessageId |> DismissChatMessage |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        notification theme { notificationDefault with NotificationSemantic = semantic |> Some ; OnDismissNotification = onDismissNotification } children
    ]
// #endregion

let render (useDefaultTheme, state, usersProjection:Projection<_ * UserDic>, hasModal, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "Chat" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ChatProjection with
        | Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, chatMessageDic, readyState) ->
            let userDic = match usersProjection with | Ready (_, userDic) -> userDic | Pending | Failed -> UserDic ()
            let newChatMessageState = readyState.NewChatMessageState
            let (ChatMessageId newChatMessageKey), newMessageText = newChatMessageState.NewChatMessageId, newChatMessageState.NewMessageText
            let helpInfo = [
                str "Chat messages are not persisted and will only be received by signed-in users. You can use "
                [ str "Markdown syntax" ] |> link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
                str " to format your message. A preview of your message will appear below." ; br; br ]
            let isSending, sendChatMessageInteraction =
                match newChatMessageState.SendChatMessageStatus with
                | Some SendChatMessagePending -> true, Loading
                | Some (SendChatMessageFailed _) | None ->
                    match Markdown newMessageText |> validateChatMessageText with
                    | Some _ -> false, NotEnabled None
                    | None -> false, Clickable ((fun _ -> SendChatMessage |> dispatch), None)
            let errorText = match newChatMessageState.SendChatMessageStatus with | Some (SendChatMessageFailed errorText) -> errorText |> Some | Some SendChatMessagePending | None -> None
            let authUserId = state.AuthUser.UserId
            let userTags =
                userDic
                |> List.ofSeq
                |> List.choose (fun (KeyValue (userId, (userName, userAuthDto))) ->
                    match userId |> userType userDic with
                    | Some userType when userType <> PersonaNonGrata ->
                        let semantic, sortOrder = (userId, userAuthDto) |> semanticAndSortOrder authUserId
                        (userName, semantic, sortOrder) |> Some
                    | Some _ | None -> None)
                |> List.sortBy (fun (userName, _, sortOrder) -> sortOrder, userName)
                |> List.map (fun (UserName userName, semantic, _) -> [ str userName ] |> tag theme { tagDefault with TagSemantic = semantic |> Some ; IsRounded = false })
            let moreChatMessages =
                let paraMore = { paraDefaultSmallest with ParaAlignment = RightAligned }
                if readyState.MoreChatMessagesPending then
                    [ br ; [ str "Retrieving more chat messages... " ; icon iconSpinnerPulseSmall ] |> para theme paraMore ]
                else if readyState.HasMoreChatMessages then
                    [ br ; [ [ str "More chat messages" ] |> link theme (ClickableLink (fun _ -> MoreChatMessages |> dispatch)) ] |> para theme paraMore ]
                else []
            match errorText with
            | Some errorText ->
                yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
                yield br
            | None -> ()
            yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
                yield textArea theme newChatMessageKey newMessageText newChatMessageState.NewMessageErrorText helpInfo (hasModal |> not) isSending (NewMessageTextChanged >> dispatch)
                if String.IsNullOrWhiteSpace newMessageText |> not then
                    yield notification theme notificationInfo [ Markdown newMessageText |> notificationContentFromMarkdown theme ] ]
            yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Send chat message" ] |> button theme { buttonLinkSmall with Interaction = sendChatMessageInteraction } ]
            yield hr theme false
            yield div divDefault [ divTags userTags ]
            yield divVerticalSpace 5
            yield! chatMessageDic
                |> List.ofSeq
                |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> chatMessageId, chatMessage)
                |> List.sortBy (fun (_, chatMessage) -> chatMessage.Timestamp)
                |> List.rev
                |> List.map (renderChatMessage theme authUserId userDic dispatch)
                |> List.collect id
            yield! moreChatMessages ]
