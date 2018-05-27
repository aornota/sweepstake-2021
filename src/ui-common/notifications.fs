module Aornota.UI.Common.Notifications

open Aornota.Common.UnitsOfMeasure

#if TICK
open Aornota.UI.Common.TimestampHelper
#endif
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open System

type NotificationId = | NotificationId of guid : Guid with
    static member Create () = Guid.NewGuid () |> NotificationId

type NotificationType = | Debug | Info | Warning | Danger

type NotificationMessage = {
    NotificationId : NotificationId
    Type : NotificationType
    Text : string
    Timestamp : DateTime
    Dismissable : bool }

let private render theme source dispatch notificationMessage =
    let notificationData = match notificationMessage.Type with | Debug -> notificationDark | Info -> notificationInfo | Warning -> notificationWarning | Danger -> notificationDanger
    let notificationData =
        if notificationMessage.Dismissable then { notificationData with OnDismissNotification = (fun _ -> notificationMessage.NotificationId |> dispatch) |> Some }
        else notificationData
    let sourceAndTypeText = sprintf "%s | %s" source (match notificationMessage.Type with | Debug -> "DEBUG" | Info -> "INFORMATION" | Warning -> "WARNING" | Danger -> "ERROR")
    let timestampText =
#if TICK
        ago notificationMessage.Timestamp
#else
        notificationMessage.Timestamp.ToString ("HH:mm:ss")
#endif
    [
        divVerticalSpace 10
        notification theme notificationData [
            level true [
                levelLeft [ levelItem [ para theme { paraDefaultSmallest with Weight = Bold } [ str sourceAndTypeText ] ] ]
                levelRight [ levelItem [ para theme { paraDefaultSmallest with ParaAlignment = RightAligned } [ str timestampText ] ] ] ]
            para theme { paraDefaultSmallest with Weight = SemiBold } [ str notificationMessage.Text ] ]
    ]

let private shouldRender (_notificationMessage:NotificationMessage) =
#if DEBUG
    true
#else
    match _notificationMessage.Type with | Debug -> false | Info | Warning | Danger -> true
#endif

let notificationMessage notificationType text timestamp dismissable = {
    NotificationId = NotificationId.Create ()
    Type = notificationType
    Text = text
    Timestamp = timestamp
    Dismissable = dismissable }

let debugMessage debugText dismissable = notificationMessage Debug debugText DateTime.Now dismissable
let debugDismissableMessage debugText = notificationMessage Debug debugText DateTime.Now true
let infoMessage infoText dismissable = notificationMessage Info infoText DateTime.Now dismissable
let infoDismissableMessage infoText = notificationMessage Info infoText DateTime.Now true
let warningMessage warningText dismissable = notificationMessage Warning warningText DateTime.Now dismissable
let warningDismissableMessage warningText = notificationMessage Warning warningText DateTime.Now true
let dangerMessage dangerText dismissable = notificationMessage Danger dangerText DateTime.Now dismissable
let dangerDismissableMessage dangerText = notificationMessage Danger dangerText DateTime.Now true

let removeNotificationMessage notificationId notificationMessages = notificationMessages |> List.filter (fun notificationMessage -> notificationMessage.NotificationId <> notificationId)

let renderSpecialNotificationMessage (useDefaultTheme, source, notificationMessage, _:int<tick>) =
    if shouldRender notificationMessage then columnContent (render (getTheme useDefaultTheme) source ignore { notificationMessage with Dismissable = false })
    else divEmpty

let renderNotificationMessages (useDefaultTheme, source, notificationMessages, _:int<tick>) dispatch =
    let notificationMessages = notificationMessages |> List.filter shouldRender
    match notificationMessages with
    | _ :: _ ->
        let render = render (getTheme useDefaultTheme) source dispatch
        columnContent [
            yield divVerticalSpace 15
            yield! notificationMessages
                |> List.sortBy (fun notificationMessage -> match notificationMessage.Type with | Debug -> 0 | Info -> 3 | Warning -> 2 | Danger -> 1)
                |> List.map render
                |> List.collect id ]
    | [] -> divEmpty
