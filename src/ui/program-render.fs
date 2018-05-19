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
open Aornota.Sweepstake2018.Common.Domain.User
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
        | Some signInState -> match signInState.SignInStatus with | Some SignInPending -> SigningIn | Some (SignInFailed _) | None -> NotSignedIn
        | None -> NotSignedIn
    | Auth authState -> match authState.SigningOut with | true -> SigningOut | false -> SignedIn authState.AuthUser

let private headerPages (appState:AppState) =
    match appState with
    | Unauth unauthState ->
        [
            "News", unauthState.CurrentUnauthPage = NewsPage, UnauthPage NewsPage, NewsPage |> ShowUnauthPage |> UnauthInput
            "Squads", unauthState.CurrentUnauthPage = SquadsPage, UnauthPage SquadsPage, SquadsPage |> ShowUnauthPage |> UnauthInput
        ]
    | Auth authState ->
        // TODO-NMB-LOW: Finesse handling of "unseen" count/s (i.e. something better than count-in-parentheses)...
        let unseenChatCount = match authState.AuthPageStates.ChatState with | Some chatState -> chatState.UnseenCount |> Some | None -> None
        let chatText = match unseenChatCount with | Some unseenCount when unseenCount > 0 -> sprintf "Chat (%i)" unseenCount | Some _ | None -> "Chat"
        [
            "News", authState.CurrentPage = UnauthPage NewsPage, UnauthPage NewsPage, NewsPage |> UnauthPage |> ShowPage |> AuthInput
            "Squads", authState.CurrentPage = UnauthPage SquadsPage, UnauthPage SquadsPage, SquadsPage |> UnauthPage |> ShowPage |> AuthInput
            "Drafts", authState.CurrentPage = AuthPage DraftsPage, AuthPage DraftsPage, DraftsPage |> AuthPage |> ShowPage |> AuthInput
            chatText, authState.CurrentPage = AuthPage ChatPage, AuthPage ChatPage, ChatPage |> AuthPage |> ShowPage |> AuthInput
            "User administration", authState.CurrentPage = AuthPage UserAdministrationPage, AuthPage UserAdministrationPage, UserAdministrationPage |> AuthPage |> ShowPage |> AuthInput
        ]
    | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> []

let private renderHeader (useDefaultTheme, navbarBurgerIsActive, headerStatus, headerPages, _:int<tick>) dispatch =
    let isUserAdministrationPage page = match page with | AuthPage UserAdministrationPage -> true | _ -> false
    let theme = getTheme useDefaultTheme
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        let spinner = icon iconSpinnerPulseSmall
        match headerStatus with
        | ReadingPreferencesHS -> [ [ str "Reading preferences... " ; spinner ] |> para theme paraStatus ]
        | ConnectingHS -> [ [ str "Connecting... " ; spinner ] |> para theme paraStatus ]
        | ServiceUnavailableHS -> [ [ str "Service unavailable" ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } ]
        | SigningIn -> [ [ str "Signing in... " ; spinner ] |> para theme paraStatus ]
        | SigningOut -> [ [ str "Signing out... " ; spinner ] |> para theme paraStatus ]
        | NotSignedIn -> 
            [
                [ str "Not signed-in" ] |> para theme paraStatus
                [ [ str "Sign in" ] |> link theme (ClickableLink (fun _ -> ShowSignInModal |> UnauthInput |> AppInput |> dispatch)) ] |> para theme paraDefaultSmallest
            ]
        | SignedIn authUser ->
            let (UserName userName) = authUser.UserName
            [ [ str "Signed-in as " ; bold userName ] |> para theme paraStatus ]
    let authUserDropDown =
        match headerStatus with
        | SignedIn authUser -> 
            let changePassword = [ str "Change password" ] |> link theme (ClickableLink (fun _ -> ShowChangePasswordModal |> AuthInput |> AppInput |> dispatch))
            let signOut = [ str "Sign out" ] |> link theme (ClickableLink (fun _ -> SignOut |> AuthInput |> AppInput |> dispatch))
            navbarDropDown theme (icon iconUserSmall) [
                match authUser.Permissions.ChangePasswordPermission with
                | Some userId when userId = authUser.UserId -> yield navbarDropDownItem theme false [ [ changePassword ] |> para theme paraDefaultSmallest ]
                | Some _ | None -> ()
                yield navbarDropDownItem theme false [ [ signOut ] |> para theme paraDefaultSmallest ] ] |> Some
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn -> None
    let pageTabs =
        headerPages
        |> List.filter (fun (_, _, page, _) -> isUserAdministrationPage page |> not)
        |> List.map (fun (text, isActive, _, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = ClickableLink (fun _ -> appInput |> AppInput |> dispatch) })
    let additionalDropDown =
        match headerStatus with
        | SignedIn authUser ->
            match authUser.Permissions.UserAdministrationPermissions with
            | Some _ ->
                let text, isActive, appInput =
                    match headerPages |> List.tryFind (fun (_, _, page, _) -> isUserAdministrationPage page) with
                    | Some (text, isActive, _, appInput) -> text, isActive, appInput
                    | None -> "User administration", false, UserAdministrationPage |> AuthPage |> ShowPage |> AuthInput // note: should never happen
                let userAdministration = [ str text ] |> link theme (ClickableLink (fun _ -> appInput |> AppInput |> dispatch))
                navbarDropDown theme (icon iconAdminSmall) [ navbarDropDownItem theme isActive [ [ userAdministration ] |> para theme paraDefaultSmallest ] ] |> Some
            | None -> None
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn _ -> None
    let otherLinks =
        match headerStatus with
        | NotSignedIn | SignedIn _ ->
            [
                navbarItem [ [ [ str "Scoring system" ] |> link theme (ClickableLink (fun _ -> ScoringSystem |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest ]
                navbarItem [ [ [ str "Payouts" ] |> link theme (ClickableLink (fun _ -> Payouts |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest ]
            ]
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut -> []
    let toggleThemeTooltipText = match useDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"           
    let toggleThemeTooltipData = if navbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft    
    let toggleThemeInteraction = Clickable ((fun _ -> ToggleTheme |> dispatch), { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText } |> Some)
    let toggleThemeButton = { buttonDarkSmall with IsOutlined = true ; Interaction = toggleThemeInteraction ; IconLeft = iconTheme |> Some }
    let navbarData = { navbarDefault with NavbarSemantic = Light |> Some }
    navbar theme navbarData [
        container (Fluid |> Some) [
            navbarBrand [
                yield navbarItem [ image "public/resources/sweepstake-2018-24x24.png" (FixedSize Square24 |> Some) ]
                yield navbarItem [ [ str SWEEPSTAKE_2018 ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } ]
                yield navbarItem [ [ str "|" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold } ]
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> ToggleNavbarBurger |> dispatch) navbarBurgerIsActive ]
            navbarMenu theme navbarData navbarBurgerIsActive [ 
                navbarStart [
                    yield Rct.ofOption authUserDropDown
                    yield navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ]
                    yield Rct.ofOption additionalDropDown
                    yield! otherLinks ]
                navbarEnd [
#if TICK
                    navbarItem [ [ str (DateTime.Now.ToString ("HH:mm:ss")) ] |> para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } ]
#endif
                    navbarItem [ [] |> button theme toggleThemeButton ] ] ] ] ]

let private renderStaticModal (useDefaultTheme, titleText, markdown) dispatch =
    let theme = getTheme useDefaultTheme
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] ((fun _ -> HideStaticModal |> dispatch) |> Some) [ markdown |> contentFromMarkdown theme ]

let private markdownSyntaxKey = Guid.NewGuid ()

let private renderMarkdownSyntaxModal useDefaultTheme dispatch =
    let theme = getTheme useDefaultTheme
    let body = [ 
        [ str "As a very quick introduction to Markdown syntax, the following:" ] |> para theme paraCentredSmaller ; br
        textArea theme markdownSyntaxKey MARKDOWN_SYNTAX_MARKDOWN None [] false true ignore
        br ; [ str "will appear as:" ] |> para theme paraCentredSmaller ; br
        Markdown MARKDOWN_SYNTAX_MARKDOWN |> contentFromMarkdown theme ]
    cardModal theme [ [ str "Markdown syntax" ] |> para theme paraCentredSmall ] ((fun _ -> HideStaticModal |> dispatch) |> Some) body

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter, onDismiss =
        let signIn, onDismiss = (fun _ -> SignIn |> dispatch), (fun _ -> CancelSignIn |> dispatch)
        match signInState.SignInStatus with
        | Some SignInPending -> true, Loading, ignore, None
        | Some (SignInFailed _) | None ->
            match validateUserName [] (UserName signInState.UserNameText), validatePassword (Password signInState.PasswordText) with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None, ignore, onDismiss |> Some
            | None, None -> false, Clickable (signIn, None), signIn, onDismiss |> Some
    let errorText = match signInState.SignInStatus with | Some (SignInFailed errorText) -> errorText |> Some | Some SignInPending | None -> None
    let body = [ 
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter your user name and password" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme signInState.UserNameKey signInState.UserNameText (iconUserSmall |> Some) false signInState.UserNameErrorText [] (signInState.FocusPassword |> not) isSigningIn
                (UserNameTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme signInState.PasswordKey signInState.PasswordText (iconPasswordSmall |> Some) true signInState.PasswordErrorText [] signInState.FocusPassword isSigningIn
                (PasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Sign in" ] |> button theme { buttonSuccessSmall with Interaction = signInInteraction } ] ]
    cardModal theme [ [ str "Sign in" ] |> para theme paraCentredSmall ] onDismiss body

// TEMP-NMB...
let private renderNews useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ [ str "News" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
let private renderSquads useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ [ str "Squads" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
// ...NMB-TEMP

let private renderUnauth (useDefaultTheme, unauthState, _ticks) (dispatch:UnauthInput -> unit) =
    div divDefault [
        match unauthState.SignInState with
        | Some signInState ->
            yield lazyViewOrHMR2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | None -> ()
        match unauthState.CurrentUnauthPage with
        // TEMP-NMB: No lazyViewOrHMR[n] since renderNews | renderSquads will return "much the same" until implemented properly [so may not re-render as expected]...
        | NewsPage ->
            yield renderNews useDefaultTheme
        | SquadsPage ->
            yield renderSquads useDefaultTheme ]

let private renderChangePasswordModal (useDefaultTheme, changePasswordState) dispatch =
    let theme = getTheme useDefaultTheme
    let onDismiss =
        match changePasswordState.MustChangePasswordReason, changePasswordState.ChangePasswordStatus with
        | Some _, _ | None, Some ChangePasswordPending -> None
        | None, Some _ | None, None -> (fun _ -> CancelChangePassword |> dispatch) |> Some
    let isChangingPassword, changePasswordInteraction, onEnter =
        let changePassword = (fun _ -> ChangePassword |> dispatch)
        match changePasswordState.ChangePasswordStatus with
        | Some ChangePasswordPending -> true, Loading, ignore
        | Some (ChangePasswordFailed _) | None ->
            match validatePassword (Password changePasswordState.NewPasswordText), validateConfirmPassword (Password changePasswordState.NewPasswordText) (Password changePasswordState.ConfirmPasswordText) with
            | Some _, Some _ | Some _, None | None, Some _ -> false, NotEnabled None, ignore
            | None, None -> false, Clickable (changePassword, None), changePassword
    let errorText = match changePasswordState.ChangePasswordStatus with | Some (ChangePasswordFailed errorText) -> errorText |> Some | Some ChangePasswordPending | None -> None
    let body = [
        match changePasswordState.MustChangePasswordReason with
        | Some mustChangePasswordReason ->
            let because reasonText = sprintf "You must change your password because %s" reasonText
            let reasonText =
                match mustChangePasswordReason with
                | FirstSignIn -> because "this is the first time you have signed in"
                | PasswordReset -> because "it has been reset by a system administrator"
            yield notification theme notificationInfo [ [ str reasonText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter and confirm your new password" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme changePasswordState.NewPasswordKey changePasswordState.NewPasswordText (iconPasswordSmall |> Some) true changePasswordState.NewPasswordErrorText []
                true isChangingPassword (NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme changePasswordState.ConfirmPasswordKey changePasswordState.ConfirmPasswordText (iconPasswordSmall |> Some) true changePasswordState.ConfirmPasswordErrorText []
                false isChangingPassword (ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change password" ] |> button theme { buttonSuccessSmall with Interaction = changePasswordInteraction } ]        ]
    cardModal theme [ [ str "Change password" ] |> para theme paraCentredSmall ] onDismiss body

let private renderSigningOutModal useDefaultTheme =
    let theme = getTheme useDefaultTheme
    cardModal theme [ [ str "Signing out" ] |> para theme paraCentredSmall ] None [ div divCentred [ icon iconSpinnerPulseLarge ] ]

// TEMP-NMB...
let private renderDrafts useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ [ str "Drafts" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
let private renderUserAdministration useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ [ str "User administration" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
// ...NMB-TEMP

let private renderAuth (useDefaultTheme, authState, ticks) dispatch =
    div divDefault [
        match authState.ChangePasswordState with
        | Some changePasswordState ->
            yield lazyViewOrHMR2 renderChangePasswordModal (useDefaultTheme, changePasswordState) (ChangePasswordInput >> dispatch)
        | None -> ()
        match authState.SigningOut with
        | true ->
            yield lazyViewOrHMR renderSigningOutModal useDefaultTheme
        | false -> ()
        match authState.CurrentPage with
        // TEMP-NMB: No lazyViewOrHMR[n] since renderNews | renderSquads | &c. will return "much the same" until implemented properly [so may not re-render as expected]...
        | UnauthPage NewsPage ->
            yield renderNews useDefaultTheme
        | UnauthPage SquadsPage ->
            yield renderSquads useDefaultTheme
        | AuthPage DraftsPage ->
            yield renderDrafts useDefaultTheme
        | AuthPage ChatPage ->
            match authState.AuthPageStates.ChatState with
            | Some chatState ->
                yield lazyViewOrHMR2 Chat.Render.render (useDefaultTheme, chatState, ticks) (ChatInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage ChatPage when ChatState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks)
        | AuthPage UserAdministrationPage ->
            yield renderUserAdministration useDefaultTheme ]

let private renderContent state dispatch =
    let renderSpinner () = div divCentred [ icon iconSpinnerPulseLarge ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ [ str "Service unavailable" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Please try again later" ] |> para theme paraCentredSmaller ]
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
        container (Fluid |> Some) [
            [
                [ str "Written" ] |> link theme (SameWindow "https://github.com/aornota/sweepstake-2018") ; str " in "
                [ str "F#" ] |> link theme (SameWindow "http://fsharp.org/") ; str " using "
                [ str "Fable" ] |> link theme (SameWindow "http://fable.io/") ; str ", "
                [ str "Elmish" ] |> link theme (SameWindow "https://fable-elmish.github.io/") ; str ", "
                [ str "Fulma" ] |> link theme (SameWindow "https://mangelmaxime.github.io/Fulma/") ; str " / "
                [ str "Bulma" ] |> link theme (SameWindow "https://bulma.io/") ; str " and "
                [ str "Giraffe" ] |> link theme (SameWindow "https://github.com/giraffe-fsharp/Giraffe/") ; str ". Developed in "
                [ str "Visual Studio Code" ] |> link theme (SameWindow "https://code.visualstudio.com/") ; str ". Best viewed with "
                [ str "Chrome" ] |> link theme (SameWindow "https://www.google.com/chrome/index.html") ; str ". Vaguely mobile-friendly." ] |> para theme paraCentredSmallest ] ]

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
