module Aornota.Sweepstake2018.UI.App.Render

open Aornota.Sweepstake2018.UI.App.Common
open Aornota.Sweepstake2018.UI.Pages

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

module Rct = Fable.Helpers.React

let private renderHeader theme uiState uiDispatch =
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        match uiState.AppState with
        | ReadingPreferences -> [ para theme paraStatus [ str "Reading preferences... " ; icon iconSpinnerPulseSmall ] ]
        | Connecting _ -> [ para theme paraStatus [ str "Connecting... " ; icon iconSpinnerPulseSmall ] ]
        | ServiceUnavailable -> [ para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } [ str "Service unavailable" ] ]
        | AutomaticallySigningIn _ -> [ para theme paraStatus [ str "Signing-in... " ; icon iconSpinnerPulseSmall ] ]
        | Unauthenticated unauthenticatedState ->
            match unauthenticatedState.SignInStatus with
            | Some Pending -> [ para theme paraStatus [ str "Signing-in... " ; icon iconSpinnerPulseSmall ] ]
            | Some (Failed _) | None -> [ para theme paraStatus [ str "Not signed-in" ] ]
        | Authenticated authenticatedState ->
            match authenticatedState.SignOutStatus with
            | Some Pending -> [ para theme paraStatus [ str "Signing-out... " ; icon iconSpinnerPulseSmall ] ]
            | Some (Failed _) | None ->
                [
                    para theme paraStatus [ str "Signed-in as " ; bold authenticatedState.AuthenticatedUser.UserName ]
                    para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> uiDispatch (SignOut |> AuthenticatedInput |> AppInput))) [ str "Sign out" ] ]
                ]
    let toggleThemeTooltipText = match uiState.UseDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleThemeTooltipData = if uiState.NavbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> uiDispatch ToggleTheme), Some { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText })
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = Some iconTheme }
    let navbarData = { navbarDefault with NavbarSemantic = Some Light }
    navbar theme navbarData [
        container (Some Fluid) [
            navbarBrand [
                // TODO-NMB-MEDIUM: Use different image?...
                yield navbarItem [ image "public/resources/djnarration-24x24.png" (Some (FixedSize Square24)) ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } [ str SWEEPSTAKE_2018 ] ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } [ str "|" ] ]
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> uiDispatch ToggleNavbarBurger) uiState.NavbarBurgerIsActive ]
            navbarMenu theme navbarData uiState.NavbarBurgerIsActive [ 
                navbarStart []
                navbarEnd [ navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderUnauthenticated theme (unauthenticatedState:UnauthenticatedState) unauthenticatedDispatch =
    let isSigningIn, signInInteraction, onEnter =
        let signIn = (fun _ -> unauthenticatedDispatch SignIn)
        match unauthenticatedState.SignInStatus with
        | Some Pending -> true, Loading, ignore
        | Some (Failed _) | None ->
            match validateUserNameText unauthenticatedState.UserNameText, validatePasswordText unauthenticatedState.PasswordText with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None, ignore
            | None, None -> false, Clickable (signIn, None), signIn
    columnContent [
        para theme paraCentredSmall [ str "Sign in" ]
        hr theme false
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme unauthenticatedState.UserNameKey unauthenticatedState.UserNameText (Some iconUserSmall) false unauthenticatedState.UserNameErrorText
                (not unauthenticatedState.FocusPassword) isSigningIn (UserNameTextChanged >> unauthenticatedDispatch) ignore ]
        field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme unauthenticatedState.PasswordKey unauthenticatedState.PasswordText (Some iconPasswordSmall) true unauthenticatedState.PasswordErrorText
                unauthenticatedState.FocusPassword isSigningIn (PasswordTextChanged >> unauthenticatedDispatch) onEnter ]
        field theme { fieldDefault with Grouped = Some Centred } [
            button theme { buttonSuccessSmall with Interaction = signInInteraction } [ span theme spanDefault [ str "Sign in" ] ] ] ]

let private renderContent theme uiState uiDispatch =
    div divDefault [
        yield divVerticalSpace 20
        match uiState.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | ServiceUnavailable ->
            yield columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
        | Unauthenticated unauthenticatedState ->
            // TODO-NMB-HIGH: Render something (via renderUnauthenticated?) if unauthenticatedState.SignInStatus is Some (Failed _)?...
            yield renderUnauthenticated theme unauthenticatedState (UnauthenticatedInput >> AppInput >> uiDispatch)
        | Authenticated authenticatedState ->
            // TODO-NMB-HIGH: Render something (analogous to renderDebugMessage?) if authenticatedState.SignOutStatus is Some (Failed _)?...
            let isSigningOut = match authenticatedState.SignOutStatus with | Some Pending -> true | Some _ | None -> false
            match authenticatedState.Page with
            | ChatPage ->
                yield Chat.Render.render theme authenticatedState.ChatState isSigningOut (ChatInput >> AuthenticatedInput >> AppInput >> uiDispatch)
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

let render uiState uiDispatch =
    let theme = getTheme uiState.UseDefaultTheme
    div divDefault [
        yield renderHeader theme uiState uiDispatch
        yield Rct.ofOption (renderDebugMessages theme SWEEPSTAKE_2018 uiState.DebugMessages (DismissDebugMessage >> uiDispatch))
        yield renderContent theme uiState uiDispatch
        yield renderFooter theme ]
