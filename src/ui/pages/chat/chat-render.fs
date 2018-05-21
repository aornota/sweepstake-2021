module Aornota.Sweepstake2018.UI.Pages.Chat.Render

open Aornota.Common.Markdown
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.Render.Markdown
#if TICK
open Aornota.UI.Common.TimestampHelper
#endif
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Chat
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open System

let [<Literal>] private RECENTLY_ACTIVE = 5.<minute>

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private (|Self|RecentlyActive|SignedIn|NotSignedIn|) (authUserId:UserId, (userId:UserId, chatUser:ChatUser)) =
    if userId = authUserId then Self
    else
        match chatUser.LastApi with
        | Some lastApi ->   
            let recentlyActiveCutoff = cutoff (int (RECENTLY_ACTIVE |> minutesToSeconds) * 1<second>)
            if lastApi > recentlyActiveCutoff then RecentlyActive else SignedIn
        | None -> NotSignedIn

let private tryFindChatUser userId (chatUserDic:ChatUserDic) = if userId |> chatUserDic.ContainsKey then chatUserDic.[userId] |> Some else None

let private semanticAndSortOrder unconfirmed authUserId (userId, chatUser) =
    match unconfirmed, (authUserId, (userId, chatUser)) with
    | true, _ -> Light, 0
    | false, Self -> Link, 1
    | false, RecentlyActive -> Success, 2
    | false, SignedIn -> Primary, 3
    | false, NotSignedIn -> Dark, 4

let private renderChatMessage theme authUserId dispatch (chatMessageId, chatMessage, unconfirmed, userId, chatUser) =
    let renderChildren (UserName userName) messageText (timestamp:DateTimeOffset) unconfirmed = [
        let rightItem =
            if unconfirmed then icon iconSpinnerPulse
            else
                let timestampText =
                    if chatMessage.Expired then "expired"
                    else
#if TICK
                        ago timestamp.LocalDateTime
#else
                        timestamp.LocalDateTime.ToString ("HH:mm:ss")
#endif
                [ str timestampText ] |> para theme paraDefaultSmallest
        yield level true [
            levelLeft [ levelItem [ [ bold userName ; str " says" ] |> para theme paraDefaultSmallest ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield messageText |> notificationContentFromMarkdown theme ]
    let (semantic, _) = (userId, chatUser) |> semanticAndSortOrder unconfirmed authUserId
    let children = renderChildren chatUser.UserName chatMessage.MessageText chatMessage.Timestamp unconfirmed
    let onDismissNotification = if chatMessage.Expired then (fun _ -> chatMessageId |> DismissChatMessage |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        notification theme { notificationDefault with NotificationSemantic = semantic |> Some ; OnDismissNotification = onDismissNotification } children
    ]

let private tagChatUser = { tagDefault with IsRounded = false }

let private renderChatUser theme semantic chatUser =
    let (UserName userName) = chatUser.UserName
    [ str userName ] |> tag theme { tagChatUser with TagSemantic = semantic |> Some }

let render (useDefaultTheme, state, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ str "Chat" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ProjectionState with
        | Initializing -> yield div divCentred [ icon iconSpinnerPulseLarge ]
        | InitializationFailed _ -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Active activeState ->
            let newChatMessage = activeState.NewChatMessage
            let (ChatMessageId newChatMessageId), newMessageText = newChatMessage.NewChatMessageId, newChatMessage.NewMessageText
            let helpInfo = [
                str "Chat messages are not persisted and will only be received by signed-in users. You can use "
                [ str "Markdown syntax" ] |> link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
                str " to format your message. A preview of your message will appear below." ; br; br ]
            let sendButtonInteraction =
                match Markdown newMessageText |> validateChatMessageText with
                | Some _ -> NotEnabled None
                | None -> Clickable ((fun _ -> SendChatMessage |> dispatch), None)
            let authUserId = state.AuthUser.UserId
            let chatUserDic = activeState.ChatProjection.ChatUserDic
            let chatUserTags =
                chatUserDic
                |> List.ofSeq
                |> List.map (fun (KeyValue (userId, chatUser)) ->
                    let (semantic, sortOrder) = (userId, chatUser) |> semanticAndSortOrder false authUserId
                    chatUser, semantic, sortOrder)
                |> List.sortBy (fun (chatUser, _, sortOrder) -> sortOrder, chatUser.UserName)
                |> List.map (fun (chatUser, semantic, _) -> chatUser |> renderChatUser theme semantic)
            // Note: Silently ignore chat messages for unknown chat user (should never happen).
            let unconfirmedChatMessages = activeState.UnconfirmedChatMessageDic |> List.ofSeq |> List.choose (fun (KeyValue (chatMessageId, chatMessage)) ->
                match chatUserDic |> tryFindChatUser chatMessage.UserId with | Some chatUser -> (chatMessageId, chatMessage, true, chatMessage.UserId, chatUser) |> Some | None -> None)
            let confirmedChatMessages = activeState.ChatProjection.ChatMessageDic |> List.ofSeq |> List.choose (fun (KeyValue (chatMessageId, chatMessage)) ->
                match chatUserDic |> tryFindChatUser chatMessage.UserId with | Some chatUser -> (chatMessageId, chatMessage, false, chatMessage.UserId, chatUser) |> Some | None -> None)
            yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
                yield textArea theme newChatMessageId newMessageText newChatMessage.NewMessageErrorText helpInfo true false (NewMessageTextChanged >> dispatch)
                if String.IsNullOrWhiteSpace newMessageText |> not then
                    yield notification theme notificationInfo [ Markdown newMessageText |> notificationContentFromMarkdown theme ] ]
            yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Send chat message" ] |> button theme { buttonLinkSmall with Interaction = sendButtonInteraction } ]
            yield hr theme false
            yield div divDefault [ divTags chatUserTags ]
            yield divVerticalSpace 5
            yield! unconfirmedChatMessages @ confirmedChatMessages
                |> List.sortBy (fun (_, chatMessage, _, _, _) -> chatMessage.Timestamp)
                |> List.rev
                |> List.map (renderChatMessage theme authUserId dispatch)
                |> List.collect id ]
