module Aornota.Sweepstake2018.UI.Program.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.Notifications
open Aornota.UI.Common.Render.Markdown
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Program.Common
open Aornota.Sweepstake2018.UI.Program.Markdown.Literals

open System

module Rct = Fable.Helpers.React

type private HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authUser : AuthUser

let private headerStatus (appState:AppState) =
    match appState with
    | ReadingPreferences -> ReadingPreferencesHS | Connecting _ -> ConnectingHS | ServiceUnavailable -> ServiceUnavailableHS | AutomaticallySigningIn _ -> SigningIn
    | Unauth unauthState ->
        match unauthState.SignInState with
        | Some signInState -> match signInState.SignInStatus with | Some Pending -> SigningIn | Some (Failed _) | None -> NotSignedIn
        | None -> NotSignedIn
    | Auth authState -> match authState.SigningOut with | true -> SigningOut | false -> SignedIn authState.AuthUser

let private headerPages (appState:AppState) =
    match appState with
    | Unauth unauthState ->
        [
            "News", unauthState.CurrentUnauthPage = News, ShowUnauthPage News |> UnauthInput
            "Squads", unauthState.CurrentUnauthPage = Squads, ShowUnauthPage Squads |> UnauthInput
        ]
    | Auth authState ->
        // TODO-NMB-LOW: Finesse handling of "unseen" count/s (i.e. something better than count-in-parentheses)...
        let unseenChatCount = match authState.AuthPageStates.ChatState with | Some chatState -> Some chatState.UnseenCount | None -> None
        let chatText = match unseenChatCount with | Some unseenCount when unseenCount > 0 -> sprintf "Chat (%i)" unseenCount | Some _ | None -> "Chat"
        [
            "News", authState.CurrentPage = UnauthPage News, ShowPage (UnauthPage News) |> AuthInput
            "Squads", authState.CurrentPage = UnauthPage Squads, ShowPage (UnauthPage Squads) |> AuthInput
            "Drafts", authState.CurrentPage = AuthPage Drafts, ShowPage (AuthPage Drafts) |> AuthInput
            chatText, authState.CurrentPage = AuthPage ChatPage, ShowPage (AuthPage ChatPage) |> AuthInput
        ]
    | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> []

let private renderHeader (useDefaultTheme, navbarBurgerIsActive, headerStatus, headerPages, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        let spinner = icon iconSpinnerPulseSmall
        match headerStatus with
        | ReadingPreferencesHS -> [ para theme paraStatus [ str "Reading preferences... " ; spinner ] ]
        | ConnectingHS -> [ para theme paraStatus [ str "Connecting... " ; spinner ] ]
        | ServiceUnavailableHS -> [ para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } [ str "Service unavailable" ] ]
        | SigningIn -> [ para theme paraStatus [ str "Signing in... " ; spinner ] ]
        | SigningOut -> [ para theme paraStatus [ str "Signing out... " ; spinner ] ]
        | NotSignedIn -> 
            [
                para theme paraStatus [ str "Not signed-in" ]
                para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowSignInModal |> UnauthInput |> AppInput |> dispatch)) [ str "Sign in" ] ]
            ]
        | SignedIn authUser ->
            let (UserName userName) = authUser.UserName
            [ para theme paraStatus [ str "Signed-in as " ; bold userName ] ]
    let authUserDropDown =
        match headerStatus with
        | SignedIn _ -> 
            let changePassword = link theme (ClickableLink (fun _ -> ChangePassword |> AuthInput |> AppInput |> dispatch)) [ str "Change password" ]
            let signOut = link theme (ClickableLink (fun _ -> SignOut |> AuthInput |> AppInput |> dispatch)) [ str "Sign out" ]
            Some (navbarDropDown theme (icon iconUserSmall) [
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ changePassword ] ]
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ signOut ] ] ])
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn -> None
    let pageTabs =
        headerPages |> List.map (fun (text, isActive, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = ClickableLink (fun _ -> appInput |> AppInput |> dispatch) })
    // TEMP-NMB...
    let adminUserDropDown =
        match headerStatus with
        | SignedIn authUser when authUser.UserName = UserName "neph" ->
            let userAdministration = link theme (ClickableLink (fun _ -> UserAdministration |> AuthInput |> AppInput |> dispatch)) [ str "User administration" ]
            Some (navbarDropDown theme (icon iconAdminSmall) [ navbarDropDownItem theme false [ para theme paraDefaultSmallest [ userAdministration ] ] ])
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn _ -> None
    // ...NMB-TEMP
    let otherLinks =
        match headerStatus with
        | NotSignedIn | SignedIn _ ->
            [
                navbarItem [ para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowStaticModal ScoringSystem |> dispatch)) [ str "Scoring system" ] ] ]
                navbarItem [ para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowStaticModal Payouts |> dispatch)) [ str "Payouts" ] ] ]
            ]
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut -> []
    let toggleThemeTooltipText = match useDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleThemeTooltipData = if navbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> ToggleTheme |> dispatch), Some { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText })
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = Some iconTheme }
    let navbarData = { navbarDefault with NavbarSemantic = Some Light }
    navbar theme navbarData [
        container (Some Fluid) [
            navbarBrand [
                yield navbarItem [ image "public/resources/sweepstake-2018-24x24.png" (Some (FixedSize Square24)) ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } [ str SWEEPSTAKE_2018 ] ]
                yield navbarItem [ para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } [ str "|" ] ]
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> ToggleNavbarBurger |> dispatch) navbarBurgerIsActive ]
            navbarMenu theme navbarData navbarBurgerIsActive [ 
                navbarStart [
                    yield Rct.ofOption authUserDropDown
                    yield navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ]
                    // TEMP-NMB...
                    yield Rct.ofOption adminUserDropDown
                    // ...NMB-TEMP
                    yield! otherLinks ]
                navbarEnd [
#if TICK
                    navbarItem [ para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } [ str (DateTime.Now.ToString ("HH:mm:ss")) ] ]
#endif
                    navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderStaticModal (useDefaultTheme, titleText, markdown) dispatch =
    let theme = getTheme useDefaultTheme
    cardModal theme [ para theme paraCentredSmall [ str titleText ] ] (Some (fun _ -> HideStaticModal |> dispatch)) [ contentFromMarkdown theme markdown ]

let private markdownSyntaxKey = Guid.NewGuid ()

let private renderMarkdownSyntaxModal useDefaultTheme dispatch =
    let theme = getTheme useDefaultTheme
    let body = [ 
        para theme paraCentredSmaller [ str "As a very quick introduction to Markdown syntax, the following:" ] ; br
        textArea theme markdownSyntaxKey MARKDOWN_SYNTAX_MARKDOWN None [] false true ignore
        br ; para theme paraCentredSmaller [ str "will appear as:" ] ; br
        contentFromMarkdown theme (Markdown MARKDOWN_SYNTAX_MARKDOWN) ]
    cardModal theme [ para theme paraCentredSmall [ str "Markdown syntax" ] ] (Some (fun _ -> HideStaticModal |> dispatch)) body

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter =
        let signIn = (fun _ -> SignIn |> dispatch)
        match signInState.SignInStatus with
        | Some Pending -> true, Loading, ignore
        | Some (Failed _) | None ->
            match validateUserName [] (UserName signInState.UserNameText), validatePassword (Password signInState.PasswordText) with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None, ignore
            | None, None -> false, Clickable (signIn, None), signIn
    let errorText = match signInState.SignInStatus with | Some (Failed errorText) -> Some errorText | Some Pending | None -> None
    let body = [ 
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ para theme paraDefaultSmallest [ str errorText ] ]
            yield br
        | None -> ()
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Some Centred } [
             textBox theme signInState.UserNameKey signInState.UserNameText (Some iconUserSmall) false signInState.UserNameErrorText [] (not signInState.FocusPassword) isSigningIn
                (UserNameTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme signInState.PasswordKey signInState.PasswordText (Some iconPasswordSmall) true signInState.PasswordErrorText [] signInState.FocusPassword isSigningIn
                (PasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Some Centred } [ button theme { buttonSuccessSmall with Interaction = signInInteraction } [ str "Sign in" ] ] ]
    cardModal theme [ para theme paraCentredSmall [ str "Sign in" ] ] (Some (fun _ -> CancelSignIn |> dispatch)) body

// TEMP-NMB...
let private renderNews useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "News" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
let private renderSquads useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "Squads" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
// ...NMB-TEMP

let private renderUnauth (useDefaultTheme, unauthState, _ticks) (dispatch:UnauthInput -> unit) =
    div divDefault [
        match unauthState.SignInState with
        | Some signInState ->
            yield lazyViewOrHMR2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | None -> ()
        match unauthState.CurrentUnauthPage with
        // TEMP-NMB: No lazyViewOrHMR[n] since renderNews | renderSquads will return "much the same" until implemented properly [so may not re-render as expected]...
        | News ->
            yield renderNews useDefaultTheme
        | Squads ->
            yield renderSquads useDefaultTheme ]

let private renderSigningOutModal useDefaultTheme =
    let theme = getTheme useDefaultTheme
    cardModal theme [ para theme paraCentredSmall [ str "Signing out" ] ] None [ div divCentred [ icon iconSpinnerPulseLarge ] ]

// TEMP-NMB...
let private renderDrafts useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ para theme paraCentredSmall [ str "Drafts" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
// ...NMB-TEMP

let private renderAuth (useDefaultTheme, authState, ticks) dispatch =
    div divDefault [
        match authState.SigningOut with
        | true ->
            yield lazyViewOrHMR renderSigningOutModal useDefaultTheme
        | false -> ()
        match authState.CurrentPage with
        // TEMP-NMB: No lazyViewOrHMR[n] since renderNews | renderSquads | renderDrafts will return "much the same" until implemented properly [so may not re-render as expected]...
        | UnauthPage News ->
            yield renderNews useDefaultTheme
        | UnauthPage Squads ->
            yield renderSquads useDefaultTheme
        | AuthPage Drafts ->
            yield renderDrafts useDefaultTheme
        | AuthPage ChatPage ->
            match authState.AuthPageStates.ChatState with
            | Some chatState ->
                yield lazyViewOrHMR2 Chat.Render.render (useDefaultTheme, chatState, ticks) (ChatInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage ChatPage when ChatState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks) ]

let private renderContent state dispatch =
    let renderSpinner () = div divCentred [ icon iconSpinnerPulseLarge ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
    div divDefault [
        yield lazyViewOrHMR divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield lazyViewOrHMR renderSpinner ()
        | ServiceUnavailable ->
            yield lazyViewOrHMR renderServiceUnavailable state.UseDefaultTheme
        | Unauth unauthState ->
            yield renderUnauth (state.UseDefaultTheme, unauthState, state.Ticks) (UnauthInput >> AppInput >> dispatch) // note: renderUnauth has its own lazyViewOrHMR[n] handling
        | Auth authState ->
            yield renderAuth (state.UseDefaultTheme, authState, state.Ticks) (AuthInput >> AppInput >> dispatch) // note: renderAuth has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR divVerticalSpace 20 ]

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
        yield lazyViewOrHMR2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, headerStatus state.AppState, headerPages state.AppState, state.Ticks) dispatch
        match state.StaticModal with
        | Some ScoringSystem ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Scoring system", (Markdown SCORING_SYSTEM_MARKDOWN)) dispatch
        | Some Payouts ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Payouts", (Markdown PAYOUTS_MARKDOWN)) dispatch
        | Some MarkdownSyntax ->
            yield lazyViewOrHMR2 renderMarkdownSyntaxModal state.UseDefaultTheme dispatch
        | None -> ()
        yield lazyViewOrHMR2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2018, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        yield renderContent state dispatch // note: renderContent has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR renderFooter state.UseDefaultTheme ]
