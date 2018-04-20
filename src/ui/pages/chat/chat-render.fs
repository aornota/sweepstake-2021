module Aornota.Sweepstake2018.UI.Pages.Chat.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.UI.Common.Render.Markdown
#if TICK
open Aornota.UI.Common.TimestampHelper
#endif
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open System

let private renderChatMessageUi theme authUserName dispatch chatMessageUi =
    // TODO-NMB-MEDIUM: Finesse text colours depending on whether Sent | SendFailed | Received [self] | Received [other]?...
    let renderChildren headerColour userName messageText (timestamp:DateTime) unconfirmed errorText = [
        let paraHeader = { paraDefaultSmallest with ParaColour = headerColour }
        let rightItem =
            if unconfirmed then icon iconSpinnerPulse
            else
                let timestampText =
#if TICK
                    ago timestamp
#else
                    timestamp.ToString ("HH:mm:ss")
#endif
                para theme paraHeader [ str timestampText ]
        yield level true [
            levelLeft [ levelItem [ para theme paraHeader [ bold userName ; str " says" ] ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield content [ htmlFromMarkdown messageText ]
        match errorText with
        | Some errorText ->
            yield! [
                divVerticalSpace 10
                para theme { paraDefaultSmallest with Weight = Bold } [ str errorText ] ]
        | None -> () ]
    let notificationData, headerColour, unconfirmed, errorText =
        match chatMessageUi.ChatMessageType with
        | Sent -> notificationLight, GreyscalePara GreyDarker, true, None
        | SendFailed errorText -> notificationDanger, SemanticPara Semantic.Warning, false, Some errorText
        // TODO-NMB-LOW: Would it be better to [add and] compare ChatMessage.UserId?...
        | Received when chatMessageUi.ChatMessage.UserName = authUserName -> notificationInfo, SemanticPara Semantic.Warning, false, None
        | Received -> notificationPrimary, SemanticPara Semantic.Warning, false, None
    let children = renderChildren headerColour chatMessageUi.ChatMessage.UserName chatMessageUi.ChatMessage.MessageText chatMessageUi.Timestamp unconfirmed errorText
    let onDismissNotification = if not unconfirmed then Some (fun _ -> DismissChatMessage chatMessageUi.ChatMessage.ChatMessageId |> dispatch) else None
    [
        divVerticalSpace 10
        notification theme { notificationData with OnDismissNotification = onDismissNotification } children
    ]

let render (useDefaultTheme, state, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let (ChatMessageId newChatMessageId, Markdown messageText) = state.NewChatMessage.NewChatMessageId, state.NewChatMessage.MessageText
    let helpInfo = [
        str "Chat messages are not persisted and will only be received by signed-in users. You can use "
        link theme (ClickableLink (fun _ -> ShowMarkdownSyntaxModal |> dispatch)) [ str "Markdown syntax" ]
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let sendButtonInteraction =
        match validateChatMessageText state.NewChatMessage.MessageText with
        | Some _ -> NotEnabled None
        | None -> Clickable ((fun _ -> SendChatMessage |> dispatch), None)
    columnContent [
        yield para theme paraCentredSmall [ str "Chat" ]
        yield hr theme false
        yield field theme { fieldDefault with Grouped = Some FullWidth } [
            yield textArea theme newChatMessageId messageText state.NewChatMessage.ErrorText helpInfo true false (Markdown >> MessageTextChanged >> dispatch)
            if not (String.IsNullOrWhiteSpace messageText) then yield notification theme notificationLight [ content [ htmlFromMarkdown state.NewChatMessage.MessageText ] ] ]
        yield field theme { fieldDefault with Grouped = Some RightAligned } [ button theme { buttonSuccessSmall with Interaction = sendButtonInteraction } [ str "Send chat message" ] ]
        yield hr theme false
        yield! state.ChatMessageUis
            |> List.sortBy (fun chatMessageUi -> chatMessageUi.Timestamp)
            // TEMP-NMB: Show more recent messages at the top...
            |> List.rev
            // ...NMB-TEMP
            |> List.map (renderChatMessageUi theme state.AuthUser.UserName dispatch)
            |> List.collect id ]
