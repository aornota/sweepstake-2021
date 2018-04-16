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

type private HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authenticatedUser : AuthenticatedUser

let private headerStatus (appState:AppState) =
    match appState with
    | ReadingPreferences -> ReadingPreferencesHS | Connecting _ -> ConnectingHS | ServiceUnavailable -> ServiceUnavailableHS | AutomaticallySigningIn _ -> SigningIn
    | Unauthenticated unauthenticatedState ->
        match unauthenticatedState.SignInState with
        | Some signInState -> match signInState.SignInStatus with | Some Pending -> SigningIn | Some (Failed _) | None -> NotSignedIn
        | None -> NotSignedIn
    | Authenticated authenticatedState -> match authenticatedState.SignOutStatus with | Some _ -> SigningOut | None -> SignedIn authenticatedState.AuthenticatedUser

let headerPages (appState:AppState) =
    match appState with
    | Unauthenticated unauthenticatedState ->
        [
            "ToDoUP", unauthenticatedState.CurrentPage = ToDoUP, ShowUnauthenticatedPage ToDoUP |> UnauthenticatedInput
        ]
    | Authenticated authenticatedState ->
        // TODO-NMB-LOW: Finesse handling of "unseen counts" (i.e. something better than count-in-parentheses)...
        let unseenChatCount = match authenticatedState.ChatState with | Some chatState -> Some chatState.UnseenCount | None -> None
        let chatText = match unseenChatCount with | Some unseenCount when unseenCount > 0 -> sprintf "Chat (%i)" unseenCount | _ -> "Chat"
        [
            "ToDoUP", authenticatedState.CurrentPage = UnauthenticatedPage ToDoUP, ShowPage (UnauthenticatedPage ToDoUP) |> AuthenticatedInput
            "ToDoAP", authenticatedState.CurrentPage = AuthenticatedPage ToDoAP, ShowPage (AuthenticatedPage ToDoAP) |> AuthenticatedInput
            chatText, authenticatedState.CurrentPage = AuthenticatedPage ChatPage, ShowPage (AuthenticatedPage ChatPage) |> AuthenticatedInput
        ]
    | _ -> []

let private renderHeader (useDefaultTheme, navbarBurgerIsActive, headerStatus, headerPages, _:int<tick>) dispatch =
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
        | NotSignedIn ->
            [
                para theme paraStatus [ str "Not signed-in" ]
                para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowSignIn |> UnauthenticatedInput |> AppInput |> dispatch)) [ str "Sign-in" ] ]
            ]
        | SignedIn authenticatedUser -> [ para theme paraStatus [ str "Signed-in as " ; bold authenticatedUser.UserName ] ]
    let authenticatedUserDropDown =
        match headerStatus with
        | SignedIn _ -> 
            let changePassword = str "Change password" // TODO-NMB-MEDIUM... link theme (ClickableLink (fun _ -> ChangePassword |> AuthenticatedInput |> AppInput |> dispatch)) [ str "Change password" ]
            let signOut = link theme (ClickableLink (fun _ -> SignOut |> AuthenticatedInput |> AppInput |> dispatch)) [ str "Sign out" ]
            Some (navbarDropDown theme (icon iconUserSmall) [
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ changePassword ] ]
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ signOut ] ] ])
        | _ -> None
    let pageTabs =
        headerPages |> List.map (fun (text, isActive, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = ClickableLink (fun _ -> appInput |> AppInput |> dispatch) })
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
                navbarStart [
                    Rct.ofOption authenticatedUserDropDown
                    navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ] ]
                navbarEnd [
#if TICK
                    navbarItem [ para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } [ str (DateTime.Now.ToString ("HH:mm:ss")) ] ]
#endif
                    navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter =
        let signIn = (fun _ -> dispatch SignIn)
        match signInState.SignInStatus with
        | Some Pending -> true, Loading, ignore
        | Some (Failed _) | None ->
            match validateUserNameText signInState.UserNameText, validatePasswordText signInState.PasswordText with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None, ignore
            | None, None -> false, Clickable (signIn, None), signIn
    // TODO-NMB-HIGH: Display errorText somehow... let errorText = match signInState.SignInStatus with | Some (Failed errorText) -> Some errorText | Some Pending | None -> None
    // TODO-NMB-HIGH: Theme-ing, i.e. useDefaultTheme and sweepstake-2018.css (&c.)...
    Fulma.Components.Modal.modal [ Fulma.Components.Modal.IsActive true ] [
        Fulma.Components.Modal.background [] []
        Fulma.Components.Modal.Card.card [] [
            Fulma.Components.Modal.Card.head [] [
                Fulma.Components.Modal.Card.title [] [ para theme paraCentredSmall [ str "Sign in" ] ]
                Fulma.Elements.Delete.delete [ Fulma.Elements.Delete.OnClick (fun _ -> CancelSignIn |> dispatch) ] [] ]
            Fulma.Components.Modal.Card.body [] [
                // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
                field theme { fieldDefault with Grouped = Some Centred } [
                    textBox theme signInState.UserNameKey signInState.UserNameText (Some iconUserSmall) false signInState.UserNameErrorText (not signInState.FocusPassword) isSigningIn
                        (UserNameTextChanged >> dispatch) ignore ]
                field theme { fieldDefault with Grouped = Some Centred } [
                    textBox theme signInState.PasswordKey signInState.PasswordText (Some iconPasswordSmall) true signInState.PasswordErrorText signInState.FocusPassword isSigningIn
                        (PasswordTextChanged >> dispatch) onEnter ]
                field theme { fieldDefault with Grouped = Some Centred } [ button theme { buttonSuccessSmall with Interaction = signInInteraction } [ str "Sign in" ] ] ] ] ]

// TEMP-NMB...
let private renderToDoUP useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "TODO-NMB-MEDIUM... UnauthorizedPage" ] ; hr theme false ; para theme paraCentredSmaller [ str "Not yet implemented" ] ]
// ...NMB-TEMP

let private renderUnauthenticated (useDefaultTheme, unauthenticatedState, _ticks) (dispatch:UnauthenticatedInput -> unit) =
    div divDefault [
        match unauthenticatedState.SignInState with
        | Some signInState ->
            yield lazyView2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | None -> ()
        match unauthenticatedState.CurrentPage with
        | ToDoUP ->
            yield lazyView renderToDoUP useDefaultTheme ]

let private renderSignOutModal (useDefaultTheme, signOutStatus:Status) dispatch =
    let theme = getTheme useDefaultTheme
    // TODO-NMB-HIGH: Display errorText somehow (e.g. in place of iconSpinnerPulseLarge)...
    let errorText = match signOutStatus with | Failed errorText -> Some errorText | Pending -> None
    // TODO-NMB-HIGH: Theme-ing, i.e. useDefaultTheme and sweepstake-2018.css (&c.)...
    Fulma.Components.Modal.modal [ Fulma.Components.Modal.IsActive true ] [
        Fulma.Components.Modal.background [] []
        Fulma.Components.Modal.Card.card [] [
            Fulma.Components.Modal.Card.head [] [
                yield Fulma.Components.Modal.Card.title [] [ para theme paraCentredSmall [ str "Signing-out..." ] ]
                match errorText with
                | Some _ ->
                    yield Fulma.Elements.Delete.delete [ Fulma.Elements.Delete.OnClick (fun _ -> CancelSignOut |> dispatch) ] []
                | None -> () ]
            Fulma.Components.Modal.Card.body [] [ div divCentred [ icon iconSpinnerPulseLarge ] ] ] ]

// TEMP-NMB...
let private renderToDoAP useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ para theme paraCentredSmall [ str "TODO-NMB-MEDIUM... AuthorizedPage" ] ; hr theme false ; para theme paraCentredSmaller [ str "Not yet implemented" ] ]
// ...NMB-TEMP

let private renderAuthenticated (useDefaultTheme, authenticatedState, ticks) dispatch =
    div divDefault [
        match authenticatedState.SignOutStatus with
        | Some signOutStatus ->
            yield lazyView2 renderSignOutModal (useDefaultTheme, signOutStatus) dispatch
        | None -> ()
        match authenticatedState.CurrentPage with
        | UnauthenticatedPage ToDoUP ->
            yield renderToDoUP useDefaultTheme // TEMP-NMB: No lazyView since renderToDoUP and renderToDoAP will return the same until implemented properly...
        | AuthenticatedPage ToDoAP ->
            yield renderToDoAP useDefaultTheme // TEMP-NMB: No lazyView since renderToDoUP and renderToDoAP will return the same until implemented properly...
        | AuthenticatedPage ChatPage ->
            match authenticatedState.ChatState with
            | Some chatState ->
                yield lazyView2 Chat.Render.render (useDefaultTheme, chatState, ticks) (ChatInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthenticatedPage ChatPage when ChatState is None" false
                yield lazyView renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks) ]

let private renderContent state dispatch =
    let renderSpinner () = div divCentred [ icon iconSpinnerPulseLarge ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
    div divDefault [
        yield lazyView divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield lazyView renderSpinner ()
        | ServiceUnavailable ->
            yield lazyView renderServiceUnavailable state.UseDefaultTheme
        | Unauthenticated unauthenticatedState ->
            yield renderUnauthenticated (state.UseDefaultTheme, unauthenticatedState, state.Ticks) (UnauthenticatedInput >> AppInput >> dispatch) // note: renderUnauthenticated has its own lazyView handling
        | Authenticated authenticatedState ->
            yield renderAuthenticated (state.UseDefaultTheme, authenticatedState, state.Ticks) (AuthenticatedInput >> AppInput >> dispatch) // note: renderAuthenticated has its own lazyView handling
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
        lazyView2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, headerStatus state.AppState, headerPages state.AppState, state.Ticks) dispatch
        lazyView2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2018, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        renderContent state dispatch // note: renderContent has its own lazyView handling
        lazyView renderFooter state.UseDefaultTheme ]
