module Aornota.Sweepstake2018.UI.Pages.Chat.Render

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

open System

let private renderChatMessageUi theme authenticatedUserName dispatch chatMessageUi =
    let renderChildren userName (messageText:string) (timestamp:DateTime) unconfirmed errorText = [  
        let message =
            messageText.Split ([| "\n" |], StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray
            |> List.fold (fun acc item -> match acc with | _ :: _ -> str item :: br :: acc | [] -> str item :: acc) []
            |> List.rev
        let rightItem = if unconfirmed then icon iconSpinnerPulse else para theme paraDefaultSmallest [ str (timestamp.ToString ("HH:mm:ss")) ]
        yield level true [
            levelLeft [ levelItem [ para theme paraDefaultSmallest [ bold userName ; str " says" ] ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield para theme { paraDefaultSmallest with Weight = SemiBold } message
        match errorText with
        | Some errorText ->
            yield! [
                divVerticalSpace 10
                para theme { paraDefaultSmallest with Weight = Bold ; ParaAlignment = Centred } [ str errorText ] ]
        | None -> () ]
    let notificationData, unconfirmed, errorText =
        match chatMessageUi.ChatMessageType with
        | Sent -> notificationLight, true, None
        | SendFailed errorText -> notificationDanger, false, Some errorText
        | Received when chatMessageUi.ChatMessage.UserName = authenticatedUserName -> notificationPrimary, false, None
        | Received -> notificationInfo, false, None
    let children = renderChildren chatMessageUi.ChatMessage.UserName chatMessageUi.ChatMessage.MessageText chatMessageUi.Timestamp unconfirmed errorText
    let onDismissNotification = if not unconfirmed then Some (fun _ -> dispatch (DismissChatMessage chatMessageUi.ChatMessage.ChatMessageId)) else None
    [
        divVerticalSpace 10
        notification theme { notificationData with OnDismissNotification = onDismissNotification } children
    ]

let render theme state dispatch =
    let (ChatMessageId newChatMessageId) = state.NewChatMessage.NewChatMessageId
    let sendButtonInteraction, onEnter =
        match validateChatMessageText state.NewChatMessage.MessageText with
        | Some _ -> NotEnabled None, ignore
        | None -> Clickable ((fun _ -> dispatch SendChatMessage), None), (fun _ -> dispatch SendChatMessage)
    columnContent [
        yield para theme paraCentredSmall [ str "Chat" ]
        yield hr theme false
        yield field theme { fieldDefault with Grouped = Some FullWidth }
            [
                // TEMP-NMB: textArea...
                //textArea theme newChatMessageId state.NewChatMessageText state.NewChatMessageErrorText true (NewChatMessageTextChanged >> dispatch)
                // ...or textBox...
                textBox theme newChatMessageId state.NewChatMessage.MessageText (Some iconFileSmall) false state.NewChatMessage.ErrorText true false (MessageTextChanged >> dispatch) onEnter
                // ...NMB-TEMP
            ]
        yield field theme { fieldDefault with Grouped = Some RightAligned } [
            button theme { buttonSuccessSmall with Interaction = sendButtonInteraction } [ str "Send chat message" ] ]
        yield hr theme false
        yield! state.ChatMessageUis
            |> List.sortBy (fun chatMessageUi -> chatMessageUi.Timestamp)
            // TEMP-NMB: Show more recent messages at the top...
            |> List.rev
            // ...NMB-TEMP
            |> List.map (renderChatMessageUi theme state.AuthenticatedUser.UserName dispatch)
            |> List.collect id
    ]
