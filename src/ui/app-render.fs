module Aornota.Sweepstake2018.UI.App.Render

open Aornota.Sweepstake2018.UI.App.Common
open Aornota.Sweepstake2018.UI.Pages

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

module Rct = Fable.Helpers.React

let private renderHeader theme state dispatch =
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        match state.AppState with
        | ReadingPreferences -> [ para theme paraStatus [ str "Reading preferences... " ; icon iconSpinnerPulseSmall ] ]
        | Connecting _ -> [ para theme paraStatus [ str "Connecting... " ; icon iconSpinnerPulseSmall ] ]
        | ServiceUnavailable -> [ para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } [ str "Service unavailable" ] ]
        | Unauthenticated unauthenticatedState ->
            match unauthenticatedState.SignInStatus with
            | Some Pending -> [ para theme paraStatus [ str "Signing-in... " ; icon iconSpinnerPulseSmall ] ]
            | Some (Failed _) | None -> [ para theme paraStatus [ str "Not signed-in" ] ]
        | Authenticated authenticatedState ->
            match authenticatedState.SignOutStatus with
            | Some Pending -> [ para theme paraStatus [ str "Signing-out... " ; icon iconSpinnerPulseSmall ] ]
            | Some (Failed _) | None ->
                [
                    para theme paraStatus [ str "Connected as " ; bold authenticatedState.AuthenticatedUser.UserName ]
                    para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> dispatch (SignOut |> AuthenticatedInput |> AppInput))) [ str "Sign out" ] ]
                ]
    let toggleThemeTooltipText = match state.UseDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleThemeTooltipData = if state.NavbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> dispatch ToggleTheme), Some { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText })
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = Some iconTheme }
    let navbarData = { navbarDefault with NavbarSemantic = Some Light }
    navbar theme navbarData [
        container (Some Fluid) [
            navbarBrand [
                // TODO-NMB: Use different image?...
                yield navbarItem [ image "public/resources/djnarration-24x24.png" (Some (FixedSize Square24)) ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } [ str SWEEPSTAKE_2018 ] ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } [ str "|" ] ]
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> dispatch ToggleNavbarBurger) state.NavbarBurgerIsActive ]
            navbarMenu theme navbarData state.NavbarBurgerIsActive [ 
                navbarStart []
                navbarEnd [ navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderUnauthenticated theme (state:UnauthenticatedState) dispatch =
    let signingIn, signInInteraction =
        match state.SignInStatus with
        | Some Pending -> true, Loading
        | Some (Failed _) | None ->
            match validateUserNameText state.UserNameText, validatePasswordText state.PasswordText with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None
            | None, None -> false, Clickable ((fun _ -> dispatch (SignIn |> UnauthenticatedInput |> AppInput)), None)
    columnContent [
        para theme paraCentredSmall [ str "Sign in" ]
        hr theme false
        // TODO-NMB: Finesse layout / alignment...
        field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme state.UserNameKey state.UserNameText (Some iconUserSmall) false state.UserNameErrorText true signingIn
                (UserNameTextChanged >> UnauthenticatedInput >> AppInput >> dispatch) (fun _ -> dispatch (SignIn |> UnauthenticatedInput |> AppInput)) ]
        (* TEMP-NMB: Password not yet being used...
        field theme { fieldDefault with Grouped = Some Centred } [
            // TODO-NMB: Only disable if signingIn - and change onEnter for UserNameText[Box] to ignore...
            textBox theme state.PasswordKey state.PasswordText (Some iconPasswordSmall) true state.PasswordErrorText false true
                (PasswordTextChanged >> UnauthenticatedInput >> AppInput >> dispatch) (fun _ -> dispatch (SignIn |> UnauthenticatedInput |> AppInput)) ] *)
        field theme { fieldDefault with Grouped = Some Centred } [
            button theme { buttonSuccessSmall with Interaction = signInInteraction } [ span theme spanDefault [ str "Sign in" ] ] ] ]

let private renderContent theme state dispatch =
    div divDefault [
        yield divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | ServiceUnavailable ->
            yield columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
        | Unauthenticated unauthenticatedState ->
            // TODO-NMB: Render something (via renderUnauthenticated?) if unauthenticatedState.SignInStatus is Some (Failed _)?...
            yield renderUnauthenticated theme unauthenticatedState dispatch
        | Authenticated authenticatedState ->
            // TODO-NMB: Render something (analogous to renderDebugMessage?) if authenticatedState.SignOutStatus is Some (Failed _)?...
            match authenticatedState.Page with
            | ChatPage ->
                // TODO-MMB: Pass something to Chat.Render.render if authenticatedState.SignOutStatus is Some Pending, i.e. so can disable interactivity?...
                yield Chat.Render.render theme authenticatedState.ChatState (ChatInput >> AuthenticatedInput >> AppInput >> dispatch)
        yield divVerticalSpace 20 ]

let private renderFooter theme =
    footer theme true [
        container (Some Fluid) [
            para theme paraCentredSmallest [
                link theme (NewWindow "https://github.com/aornota/sweepstake-2018") [ str "Written" ] ; str " in "
                link theme (NewWindow "http://fsharp.org/") [ str "F#" ] ; str " using "
                link theme (NewWindow "http://fable.io/") [ str "Fable" ] ; str ", "
                link theme (NewWindow "https://fable-elmish.github.io/") [ str "Elmish" ] ; str ", "
                link theme (NewWindow "https://mangelmaxime.github.io/Fulma/") [ str "Fulma" ] ; str " / "
                link theme (NewWindow "https://bulma.io/") [ str "Bulma" ] ; str " and "
                link theme (NewWindow "https://github.com/giraffe-fsharp/Giraffe/") [ str "Giraffe" ] ; str ". Developed in "
                link theme (NewWindow "https://code.visualstudio.com/") [ str "Visual Studio Code" ] ; str ". Best viewed with "
                link theme (NewWindow "https://www.google.com/chrome/index.html") [ str "Chrome" ] ; str ". Vaguely mobile-friendly." ] ] ]

let render state dispatch =
    let theme = getTheme state.UseDefaultTheme
    div divDefault [
        yield renderHeader theme state dispatch
        yield Rct.ofOption (renderDebugMessages theme SWEEPSTAKE_2018 state.DebugMessages (DismissDebugMessage >> dispatch))
        yield renderContent theme state dispatch
        yield renderFooter theme ]
