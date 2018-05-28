module Aornota.Sweepstake2018.UI.Pages.Squads.Render

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.Squads.Common

open System

let private groups = [ GroupA ; GroupB ; GroupC ; GroupD ; GroupE ; GroupF ; GroupG ; GroupH ]

let private groupText group =
    let groupText = match group with | GroupA -> "A" | GroupB -> "B" | GroupC -> "C" | GroupD -> "D" | GroupE -> "E" | GroupF -> "F" | GroupG -> "G" | GroupH -> "H"
    sprintf "Group %s" groupText

let private groupTab currentGroup dispatch group =
    let isActive = match currentGroup with | Some currentGroup when currentGroup = group -> true | Some _ | None -> false
    { IsActive = isActive ; TabText = group |> groupText ; TabLinkType = ClickableLink (fun _ -> group |> ShowGroup |> dispatch ) }

// TODO-NEXT...

let render (useDefaultTheme, state, authUser, hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ str "Squads" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ProjectionState with
        | Initializing ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | InitializationFailed _ -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Active activeState ->
            let groupTabs = groups |> List.map (groupTab activeState.CurrentGroup dispatch)
            yield div divCentred [ tabs theme { tabsDefault with Tabs = groupTabs } ]
            // TODO-NEXT: Tabs for Squads-per-Group | Search box? | Selected Squad | Add Player link | ...
            yield br
            yield [ str "Coming soon" ] |> para theme paraCentredSmaller ]
