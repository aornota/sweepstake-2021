module Aornota.Sweepstake2018.UI.Pages.Chat.Render

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
open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open System

let private renderChatMessageUi theme (UserName authUserName) dispatch chatMessageUi =
    // TODO-NMB-MEDIUM: Finesse text colours depending on whether Sent | SendFailed | Received [self] | Received [other]?...
    let renderChildren userName messageText (timestamp:DateTime) unconfirmed errorText = [
        let rightItem =
            if unconfirmed then icon iconSpinnerPulse
            else
                let timestampText =
#if TICK
                    ago timestamp
#else
                    timestamp.ToString ("HH:mm:ss")
#endif
                [ str timestampText ] |> para theme paraDefaultSmallest
        yield level true [
            levelLeft [ levelItem [ [ bold userName ; str " says" ] |> para theme paraDefaultSmallest ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield messageText |> notificationContentFromMarkdown theme
        match errorText with
        | Some errorText ->
            yield! [
                divVerticalSpace 10
                [ str errorText ] |> para theme { paraDefaultSmallest with Weight = Bold } ]
        | None -> () ]
    let notificationData, unconfirmed, errorText =
        match chatMessageUi.ChatMessageType with
        | Sent -> notificationLight, true, None
        | SendFailed errorText -> notificationDanger, false, errorText |> Some
        // TODO-NMB-LOW: Would it be better to [add and] compare ChatMessage.UserId?...
        | Received when chatMessageUi.ChatMessage.UserName = authUserName -> notificationSuccess, false, None
        // TODO-NMB-LOW: Use different semantics for signed-in-and-active (Primary?) | signed-in-but-inactive (Warning? Link?) | not-signed-in (Dark?)?...
        | Received -> notificationPrimary, false, None
    let children = renderChildren chatMessageUi.ChatMessage.UserName chatMessageUi.ChatMessage.MessageText chatMessageUi.Timestamp unconfirmed errorText
    let onDismissNotification = if unconfirmed |> not then (fun _ -> DismissChatMessage chatMessageUi.ChatMessage.ChatMessageId |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        notification theme { notificationData with OnDismissNotification = onDismissNotification } children
    ]

let render (useDefaultTheme, state, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let (ChatMessageId newChatMessageId, Markdown messageText) = state.NewChatMessage.NewChatMessageId, state.NewChatMessage.MessageText
    let helpInfo = [
        str "Chat messages are not persisted and will only be received by signed-in users. You can use "
        [ str "Markdown syntax" ] |> link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let sendButtonInteraction =
        match validateChatMessageText state.NewChatMessage.MessageText with
        | Some _ -> NotEnabled None
        | None -> Clickable ((fun _ -> SendChatMessage |> dispatch), None)
    columnContent [
        yield [ str "Chat" ] |> para theme paraCentredSmall
        yield hr theme false
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme newChatMessageId messageText state.NewChatMessage.ErrorText helpInfo true false (Markdown >> MessageTextChanged >> dispatch)
            if String.IsNullOrWhiteSpace messageText |> not then
                yield notification theme notificationLight [ state.NewChatMessage.MessageText |> notificationContentFromMarkdown theme ] ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Send chat message" ] |> button theme { buttonSuccessSmall with Interaction = sendButtonInteraction } ]
        yield hr theme false
        yield! state.ChatMessageUis
            |> List.sortBy (fun chatMessageUi -> chatMessageUi.Timestamp)
            // TEMP-NMB: Show more recent messages at the top...
            |> List.rev
            // ...NMB-TEMP
            |> List.map (renderChatMessageUi theme state.AuthUser.UserName dispatch)
            |> List.collect id ]
