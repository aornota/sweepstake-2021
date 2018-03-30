module Aornota.UI.Common.DebugMessages

open System

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

type DebugId = | DebugId of guid : Guid

type DebugMessage = {
    DebugId : DebugId
    DebugMessage : string }

let debugMessage message = { DebugId = DebugId (Guid.NewGuid ()) ; DebugMessage = message }

let removeDebugMessage debugId debugMessages = debugMessages |> List.filter (fun debugMessage -> debugMessage.DebugId <> debugId)

let private renderChildren theme colour source message = [
    level true [
        levelLeft [
            levelItem [ para theme { paraDefaultSmallest with ParaColour = colour ; Weight = SemiBold } [ str source ] ]
            levelItem [ span theme spanDefault [] ] ] ]
    para theme { paraDefaultSmallest with Weight = SemiBold } [ str message ] ]

let renderDebugMessage theme source message =
#if DEBUG
    let children = renderChildren theme (GreyscalePara GreyDarker) (sprintf "%s | Debug" source) message
    [ columnContent [
        divVerticalSpace 10
        notification theme notificationLight children ] ]
#else
    []
#endif

let renderDebugMessages theme source (debugMessages:DebugMessage list) dispatch =
#if DEBUG
    match debugMessages with
    | _ :: _ ->
        [
            columnContent [
                yield! debugMessages
                |> List.map (fun debugMessage ->
                    let children = renderChildren theme (GreyscalePara GreyLighter) (sprintf "%s | Debug" source) debugMessage.DebugMessage
                    [
                        divVerticalSpace 10
                        notification theme { notificationDark with OnDismissNotification = Some (fun _ -> dispatch debugMessage.DebugId) } children
                    ])                   
                |> List.collect id ]
        ]
    | [] -> []
#else
    []
#endif

