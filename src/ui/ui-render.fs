module Aornota.Sweepstake2018.UI.Render

open Aornota.Sweepstake2018.UI.Common

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

let private renderHeader theme state dispatch =
    let toggleTooltipText = match state.UseDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleTooltipData = if state.NavbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> dispatch ToggleTheme), Some { toggleTooltipData with TooltipText = toggleTooltipText })
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = Some iconTheme }
    let navbarData = { navbarDefault with NavbarSemantic = Some Light }
    navbar theme navbarData [
        container (Some Fluid) [
            navbarBrand [
                // TODO-NMB: Use different image?...
                yield navbarItem [ image "public/resources/djnarration-24x24.png" (Some (FixedSize Square24)) ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } [ str SWEEPSTAKE_2018 ] ]
                yield navbarBurger (fun _ -> dispatch ToggleNavbarBurger) state.NavbarBurgerIsActive ]
            navbarMenu theme navbarData state.NavbarBurgerIsActive [ 
                navbarStart []
                navbarEnd [ navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderContent theme state dispatch =
    let decrementTooltip = { tooltipDefaultLeft with TooltipText = "Decrement counter" }
    let incrementTooltip = { tooltipDefaultRight with TooltipText = "Increment counter" }
    div divDefault [
        divVerticalSpace 10
        columnContent [
            para theme paraCentredSmall [ str (sprintf "Counter: %i" state.Counter) ]
            hr theme false
            para theme { paraCentredSmallest with ParaColour = SemanticPara White } [
                button theme { buttonInfo with Interaction = Clickable ((fun _ -> dispatch DecrementCounter), Some decrementTooltip) } [ str "-" ]
                str "***"
                button theme { buttonInfo with Interaction = Clickable ((fun _ -> dispatch IncrementCounter), Some incrementTooltip) } [ str "+" ] ] ]
        divVerticalSpace 5 ]

let private renderFooter theme =
    footer theme true [
        container (Some Fluid) [
            para theme paraCentredSmallest [
                link theme { LinkUrl = "https://github.com/aornota/sweepstake-2018" ; LinkType = NewWindow } [ str "Written" ]
                str " in "
                link theme { LinkUrl = "http://fsharp.org/" ; LinkType = NewWindow } [ str "F#" ]
                str " using "
                link theme { LinkUrl = "http://fable.io/" ; LinkType = NewWindow } [ str "Fable" ]
                str ", "
                link theme { LinkUrl = "https://fable-elmish.github.io/" ; LinkType = NewWindow } [ str "Elmish" ]
                str ", "
                link theme { LinkUrl = "https://mangelmaxime.github.io/Fulma/" ; LinkType = NewWindow } [ str "Fulma" ]
                str " / "
                link theme { LinkUrl = "http://bulma.io/" ; LinkType = NewWindow } [ str "Bulma" ]
                str " and "
                link theme { LinkUrl = "https://github.com/giraffe-fsharp/Giraffe/" ; LinkType = NewWindow } [ str "Giraffe" ]
                str ". Developed in "
                link theme { LinkUrl = "https://code.visualstudio.com/" ; LinkType = NewWindow } [ str "Visual Studio Code" ]
                str ". Best viewed with "
                link theme { LinkUrl = "https://www.google.com/chrome/index.html" ; LinkType = NewWindow } [ str "Chrome" ]
                str ". Vaguely mobile-friendly." ] ] ]

let render state dispatch =
    let theme = getTheme state.UseDefaultTheme
    match state.Status with
    | ReadingPreferences _ ->
        div divDefault [] // note: do *not* use pageLoader until we know the preferred theme
    | InitializingCounter ->
        pageLoader theme pageLoaderDefault
    | Ready ->
        div divDefault [
            yield renderHeader theme state dispatch
            yield! renderDebugMessages theme SWEEPSTAKE_2018 state.DebugMessages (DismissDebugMessage >> dispatch)
            yield renderContent theme state dispatch
            yield renderFooter theme ]
