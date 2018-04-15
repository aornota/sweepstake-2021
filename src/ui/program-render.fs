module Aornota.Sweepstake2018.UI.Program.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Program.Common

open Aornota.UI.Common.Notifications
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

#if TICK
open System
#endif

open Elmish.React.Common

module Rct = Fable.Helpers.React

type HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authenticatedUser : AuthenticatedUser

let headerStatus (appState:AppState) =
    match appState with
    | ReadingPreferences -> ReadingPreferencesHS | Connecting _ -> ConnectingHS | ServiceUnavailable -> ServiceUnavailableHS | AutomaticallySigningIn _ -> SigningIn
    | Unauthenticated unauthenticatedState -> match unauthenticatedState.SignInStatus with | Some Pending -> SigningIn | Some (Failed _) | None -> NotSignedIn
    | Authenticated authenticatedState -> match authenticatedState.SignOutStatus with | Some Pending -> SigningOut | Some (Failed _) | None -> SignedIn authenticatedState.AuthenticatedUser

let private renderHeader (useDefaultTheme, navbarBurgerIsActive, headerStatus, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        let spinner = icon iconSpinnerPulseSmall
        match headerStatus with
        | ReadingPreferencesHS -> [ para theme paraStatus [ str "Reading preferences... " ; spinner ] ]
        | ConnectingHS -> [ para theme paraStatus [ str "Connecting... " ; spinner ] ]
        | ServiceUnavailableHS -> [ para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } [ str "Service unavailable" ] ]
        | SigningIn -> [ para theme paraStatus [ str "Signing-in... " ; spinner ] ]
        | SigningOut -> [ para theme paraStatus [ str "Signing-out... " ; spinner ] ]
        | NotSignedIn -> [ para theme paraStatus [ str "Not signed-in" ] ]
        | SignedIn authenticatedUser -> [ para theme paraStatus [ str "Signed-in as " ; bold authenticatedUser.UserName ] ]
    let authenticatedUserDropDown =
        match headerStatus with
        | SignedIn _ -> 
            // TODO-NMB-MEDIUM... let changePassword = link theme (ClickableLink (fun _ -> uiDispatch (ChangePassword |> AuthenticatedInput |> AppInput))) [ str "Change password" ]
            let signOut = link theme (ClickableLink (fun _ -> dispatch (SignOut |> AuthenticatedInput |> AppInput))) [ str "Sign out" ]
            Some (navbarDropDown theme (icon iconUserSmall) [
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ str "TODO:Change password" ] ]
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ signOut ] ] ])
        | _ -> None
    let toggleThemeTooltipText = match useDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleThemeTooltipData = if navbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> dispatch ToggleTheme), Some { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText })
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = Some iconTheme }
    let navbarData = { navbarDefault with NavbarSemantic = Some Light }
    navbar theme navbarData [
        container (Some Fluid) [
            navbarBrand [
                yield navbarItem [ image "public/resources/sweepstake-2018-24x24.png" (Some (FixedSize Square24)) ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } [ str SWEEPSTAKE_2018 ] ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } [ str "|" ] ]
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> dispatch ToggleNavbarBurger) navbarBurgerIsActive ]
            navbarMenu theme navbarData navbarBurgerIsActive [ 
                navbarStart [ Rct.ofOption authenticatedUserDropDown ]
                navbarEnd [
#if TICK
                    navbarItem [ para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } [ str (DateTime.Now.ToString ("HH:mm:ss")) ] ]
#endif
                    navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderUnauthenticated (useDefaultTheme, unauthenticatedState:UnauthenticatedState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter =
        let signIn = (fun _ -> dispatch SignIn)
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
                (not unauthenticatedState.FocusPassword) isSigningIn (UserNameTextChanged >> dispatch) ignore ]
        field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme unauthenticatedState.PasswordKey unauthenticatedState.PasswordText (Some iconPasswordSmall) true unauthenticatedState.PasswordErrorText
                unauthenticatedState.FocusPassword isSigningIn (PasswordTextChanged >> dispatch) onEnter ]
        field theme { fieldDefault with Grouped = Some Centred } [ button theme { buttonSuccessSmall with Interaction = signInInteraction } [ str "Sign in" ] ] ]

let private renderContent state dispatch =
    let renderSpinner () = div divCentred [ icon iconSpinnerPulseLarge ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
    // TODO-NMB-HIGH: Rework UnauthenticatedState to avoid needing custom equal function?...
    let usEqual (useDefaultTheme1:bool, us1:UnauthenticatedState) (useDefaultTheme2:bool, us2:UnauthenticatedState) =
        useDefaultTheme1 = useDefaultTheme2 && us1.UserNameKey = us2.UserNameKey && us1.UserNameText = us2.UserNameText && us1.UserNameErrorText = us2.UserNameErrorText
        && us1.PasswordKey = us2.PasswordKey && us1.PasswordText = us2.PasswordText && us1.PasswordErrorText = us2.PasswordErrorText && us1.FocusPassword = us2.FocusPassword
        && us1.SignInStatus = us2.SignInStatus
    div divDefault [
        yield lazyView divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield lazyView renderSpinner ()
        | ServiceUnavailable ->
            yield lazyView renderServiceUnavailable state.UseDefaultTheme
        | Unauthenticated unauthenticatedState ->
            // TODO-NMB-HIGH: Render something (via renderUnauthenticated?) if unauthenticatedState.SignInStatus is Some (Failed _)?...
            yield lazyView2With usEqual renderUnauthenticated (state.UseDefaultTheme, unauthenticatedState) (UnauthenticatedInput >> AppInput >> dispatch)
        | Authenticated authenticatedState ->
            // TODO-NMB-HIGH: Render something (analogous to renderDebugMessage?) if authenticatedState.SignOutStatus is Some (Failed _)?...
            let isSigningOut = match authenticatedState.SignOutStatus with | Some Pending -> true | Some (Failed _) | None -> false
            match authenticatedState.Page with
            | ChatPage ->
                yield lazyView2 Chat.Render.render (state.UseDefaultTheme, authenticatedState.ChatState, isSigningOut, state.Ticks) (ChatInput >> AuthenticatedInput >> AppInput >> dispatch)
        yield lazyView divVerticalSpace 20 ]

let private renderFooter useDefaultTheme =
    let theme = getTheme useDefaultTheme
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
    div divDefault [
        lazyView2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, headerStatus state.AppState, state.Ticks) dispatch
        // TEMP-NMB: To test rendering "special" [i.e. not from state] NotificationMessage...
        //lazyView renderSpecialNotificationMessage (state.UseDefaultTheme, SWEEPSTAKE_2018, (infoMessage "Test non-dismissable render message" false), state.Ticks)
        // ...NMB-TEMP
        lazyView2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2018, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        renderContent state dispatch // note: renderContent has its own lazyView handling
        lazyView renderFooter state.UseDefaultTheme ]
