module Aornota.Sweepstake2018.UI.Pages.Chat.Render

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

open System

let private renderChatMessageUi theme authenticatedUserName isSigningOut chatDispatch chatMessageUi =
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
                para theme { paraDefaultSmallest with Weight = Bold } [ str errorText ] ]
        | None -> () ]
    let notificationData, unconfirmed, errorText =
        match chatMessageUi.ChatMessageType with
        | Sent -> notificationLight, true, None
        | SendFailed errorText -> notificationDanger, false, Some errorText
        // TODO-NMB-LOW: Would it be better to [add and] compare ChatMessage.UserId?...
        | Received when chatMessageUi.ChatMessage.UserName = authenticatedUserName -> notificationPrimary, false, None
        | Received -> notificationInfo, false, None
    let children = renderChildren chatMessageUi.ChatMessage.UserName chatMessageUi.ChatMessage.MessageText chatMessageUi.Timestamp unconfirmed errorText
    let dismissable =
        // TEMP-NMB: Prevent DismissChatMessage if signing out [or if unconfirmed]...
        not (unconfirmed || isSigningOut)
        // ...or not...
        //not unconfirmed
        // ...NMB-TEMP
    let onDismissNotification = if dismissable then Some (fun _ -> chatDispatch (DismissChatMessage chatMessageUi.ChatMessage.ChatMessageId)) else None
    [
        divVerticalSpace 10
        notification theme { notificationData with OnDismissNotification = onDismissNotification } children
    ]

let render theme chatState isSigningOut chatDispatch =
    let (ChatMessageId newChatMessageId) = chatState.NewChatMessage.NewChatMessageId
    let sendButtonInteraction, onEnter =
        match validateChatMessageText chatState.NewChatMessage.MessageText with
        | Some _ -> NotEnabled None, ignore
        | None -> Clickable ((fun _ -> chatDispatch SendChatMessage), None), (fun _ -> chatDispatch SendChatMessage)
    columnContent [
        yield para theme paraCentredSmall [ str "Chat" ]
        yield hr theme false
        yield field theme { fieldDefault with Grouped = Some FullWidth }
            [
                // TEMP-NMB: textArea...
                //textArea theme newChatMessageId state.NewChatMessage.MessageText state.NewChatMessage.ErrorText true isSigningOut (MessageTextChanged >> dispatch)
                // ...or textBox...
                textBox theme newChatMessageId chatState.NewChatMessage.MessageText (Some iconFileSmall) false chatState.NewChatMessage.ErrorText true isSigningOut (MessageTextChanged >> chatDispatch) onEnter
                // ...NMB-TEMP
            ]
        yield field theme { fieldDefault with Grouped = Some RightAligned } [
            button theme { buttonSuccessSmall with Interaction = sendButtonInteraction } [ str "Send chat message" ] ]
        yield hr theme false
        yield! chatState.ChatMessageUis
            |> List.sortBy (fun chatMessageUi -> chatMessageUi.Timestamp)
            // TEMP-NMB: Show more recent messages at the top...
            |> List.rev
            // ...NMB-TEMP
            |> List.map (renderChatMessageUi theme chatState.AuthenticatedUser.UserName isSigningOut chatDispatch)
            |> List.collect id
    ]
