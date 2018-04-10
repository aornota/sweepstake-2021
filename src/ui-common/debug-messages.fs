module Aornota.UI.Common.DebugMessages

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

open System

type DebugId = | DebugId of guid : Guid with static member Create () = Guid.NewGuid () |> DebugId

type DebugMessage = {
    DebugId : DebugId
    DebugText : string }

let [<Literal>] private DEBUG = "Debug"

let private renderChildren theme colour source text = [
    level true [
        levelLeft [
            levelItem [ para theme { paraDefaultSmallest with ParaColour = colour ; Weight = SemiBold } [ str (sprintf "%s | %s" source DEBUG) ] ]
            levelItem [ span theme spanDefault [] ] ] ]
    para theme { paraDefaultSmallest with Weight = SemiBold } [ str text ] ]

let debugMessage debugText = { DebugId = DebugId.Create () ; DebugText = debugText }

let removeDebugMessage debugId debugMessages = debugMessages |> List.filter (fun debugMessage -> debugMessage.DebugId <> debugId)

let renderDebugMessage theme source debugText =
#if DEBUG
    let children = renderChildren theme (GreyscalePara GreyDarker) source debugText
    Some (columnContent
        [
            divVerticalSpace 10
            notification theme notificationLight children
        ])
#else
    None
#endif

let renderDebugMessages theme source (debugMessages:DebugMessage list) dispatch =
#if DEBUG
    match debugMessages with
    | _ :: _ ->
        Some (columnContent
            [
                yield! debugMessages
                    |> List.map (fun debugMessage ->
                        let children = renderChildren theme (GreyscalePara GreyLighter) source debugMessage.DebugText
                        [
                            divVerticalSpace 10
                            notification theme { notificationDark with OnDismissNotification = Some (fun _ -> dispatch debugMessage.DebugId) } children
                        ])                   
                    |> List.collect id
            ])
    | [] -> None
#else
    None
#endif
