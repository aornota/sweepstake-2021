module Aornota.Sweepstake2018.UI.Program.Render

open Aornota.Common.Markdown
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.Notifications
open Aornota.UI.Common.Render.Markdown
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Program.Common
open Aornota.Sweepstake2018.UI.Program.Markdown.Literals
open Aornota.Sweepstake2018.UI.Shared

open System

module Rct = Fable.Helpers.React

type private HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authUser : AuthUser

let private headerStatus (appState:AppState) =
    let headerStatus =
        match appState with
        | ReadingPreferences -> ReadingPreferencesHS
        | Connecting _ -> ConnectingHS
        | ServiceUnavailable -> ServiceUnavailableHS
        | AutomaticallySigningIn _ -> SigningIn
        | Unauth unauthState ->
            match unauthState.SignInState with
            | Some signInState -> match signInState.SignInStatus with | Some SignInPending -> SigningIn | Some (SignInFailed _) | None -> NotSignedIn
            | None -> NotSignedIn
        | Auth authState -> match authState.SigningOut with | true -> SigningOut | false -> SignedIn authState.AuthUser
    headerStatus

let private headerPages (appState:AppState) =
    let newsText unseenNewsCount = if unseenNewsCount > 0 then sprintf "News (%i)" unseenNewsCount else "News"
    match appState with
    | Unauth unauthState ->
        let unseenNewsCount = unauthState.UnauthPageStates.NewsState.UnseenCount
        [
            unseenNewsCount |> newsText, unauthState.CurrentUnauthPage = NewsPage, UnauthPage NewsPage, NewsPage |> ShowUnauthPage |> UnauthInput
            "Scores", unauthState.CurrentUnauthPage = ScoresPage, UnauthPage ScoresPage, ScoresPage |> ShowUnauthPage |> UnauthInput
            "Squads", unauthState.CurrentUnauthPage = SquadsPage, UnauthPage SquadsPage, SquadsPage |> ShowUnauthPage |> UnauthInput
            "Fixtures", unauthState.CurrentUnauthPage = FixturesPage, UnauthPage FixturesPage, FixturesPage |> ShowUnauthPage |> UnauthInput
        ]
    | Auth authState ->
        let unseenNewsCount = authState.UnauthPageStates.NewsState.UnseenCount
        let unseenChatCount = authState.AuthPageStates.ChatState.UnseenCount
        let chatText = if unseenChatCount > 0 then sprintf "Chat (%i)" unseenChatCount else "Chat"
        [
            unseenNewsCount |> newsText, authState.CurrentPage = UnauthPage NewsPage, UnauthPage NewsPage, NewsPage |> UnauthPage |> ShowPage |> AuthInput
            "Scores", authState.CurrentPage = UnauthPage ScoresPage, UnauthPage ScoresPage, ScoresPage |> UnauthPage |> ShowPage |> AuthInput
            "Squads", authState.CurrentPage = UnauthPage SquadsPage, UnauthPage SquadsPage, SquadsPage |> UnauthPage |> ShowPage |> AuthInput
            "Fixtures", authState.CurrentPage = UnauthPage FixturesPage, UnauthPage FixturesPage, FixturesPage |> UnauthPage |> ShowPage |> AuthInput
            "User administration", authState.CurrentPage = AuthPage UserAdminPage, AuthPage UserAdminPage, UserAdminPage |> AuthPage |> ShowPage |> AuthInput
            "Draft administration", authState.CurrentPage = AuthPage DraftAdminPage, AuthPage DraftAdminPage, DraftAdminPage |> AuthPage |> ShowPage |> AuthInput
            "Drafts", authState.CurrentPage = AuthPage DraftsPage, AuthPage DraftsPage, DraftsPage |> AuthPage |> ShowPage |> AuthInput
            chatText, authState.CurrentPage = AuthPage ChatPage, AuthPage ChatPage, ChatPage |> AuthPage |> ShowPage |> AuthInput
        ]
    | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> []

// #region renderHeader
let private renderHeader (useDefaultTheme, navbarBurgerIsActive, serverStarted:DateTimeOffset option, headerStatus, headerPages, _:int<tick>) dispatch =
    let isAdminPage page = match page with | AuthPage UserAdminPage | AuthPage DraftAdminPage -> true | _ -> false
    let theme = getTheme useDefaultTheme
    let serverStarted =
        match headerStatus, serverStarted with
        | SignedIn authUser, Some serverStarted when authUser.UserType = SuperUser ->
            let timestampText =
#if TICK
                ago serverStarted.LocalDateTime
#else
                sprintf "on %s" (serverStarted.LocalDateTime |> dateAndTimeText)
#endif
            navbarItem [ [ str (sprintf "Server started %s" timestampText ) ] |> para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDark } ] |> Some
        | _, _ -> None
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        let spinner = icon iconSpinnerPulseSmall
        let separator = [ str "|" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold }
        match headerStatus with
        | ReadingPreferencesHS -> [ [ str "Reading preferences... " ; spinner ] |> para theme paraStatus ]
        | ConnectingHS -> [ [ str "Connecting... " ; spinner ] |> para theme paraStatus ]
        | ServiceUnavailableHS -> [ [ str "Service unavailable" ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } ]
        | SigningIn -> [ separator ; [ str "Signing in... " ; spinner ] |> para theme paraStatus ]
        | SigningOut -> [ separator ; [ str "Signing out... " ; spinner ] |> para theme paraStatus ]
        | NotSignedIn -> 
            [               
                separator ; [ str "Not signed-in" ] |> para theme paraStatus
                [ [ str "Sign in" ] |> link theme (ClickableLink (fun _ -> ShowSignInModal |> UnauthInput |> AppInput |> dispatch)) ] |> para theme paraDefaultSmallest
            ]
        | SignedIn authUser ->
            let (UserName userName) = authUser.UserName
            [ separator ; [ str "Signed-in as " ; bold userName ] |> para theme paraStatus ]
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
        |> List.filter (fun (_, _, page, _) -> isAdminPage page |> not)
        |> List.map (fun (text, isActive, _, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = ClickableLink (fun _ -> appInput |> AppInput |> dispatch) })
    let adminDropDown =
        match headerStatus with
        | SignedIn authUser ->
            let userAdmin =
                match authUser.Permissions.UserAdminPermissions with
                | Some _ ->
                    let text, isActive, appInput =
                        match headerPages |> List.tryFind (fun (_, _, page, _) -> page = AuthPage UserAdminPage) with
                        | Some (text, isActive, _, appInput) -> text, isActive, appInput
                        | None -> "User administration", false, UserAdminPage |> AuthPage |> ShowPage |> AuthInput // note: should never happen
                    let userAdmin = [ str text ] |> link theme (ClickableLink (fun _ -> appInput |> AppInput |> dispatch))
                    navbarDropDownItem theme isActive [ [ userAdmin ] |> para theme paraDefaultSmallest ] |> Some
                | None -> None
            let draftAdmin =
                if authUser.Permissions.DraftAdminPermission then
                    let text, isActive, appInput =
                        match headerPages |> List.tryFind (fun (_, _, page, _) -> page = AuthPage DraftAdminPage) with
                        | Some (text, isActive, _, appInput) -> text, isActive, appInput
                        | None -> "Draft administration", false, DraftAdminPage |> AuthPage |> ShowPage |> AuthInput // note: should never happen
                    let draftAdmin = [ str text ] |> link theme (ClickableLink (fun _ -> appInput |> AppInput |> dispatch))
                    navbarDropDownItem theme isActive [ [ draftAdmin ] |> para theme paraDefaultSmallest ] |> Some
                else None
            let hasDropDown = match userAdmin, draftAdmin with | None, None -> false | _ -> true
            if hasDropDown then navbarDropDown theme (icon iconAdminSmall) [ Rct.ofOption userAdmin ; Rct.ofOption draftAdmin ] |> Some
            else None
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn _ -> None
    let infoDropDown =
        match headerStatus with
        | NotSignedIn | SignedIn _ ->
            let scoringSystem = [ [ str "Scoring system" ] |> link theme (ClickableLink (fun _ -> ScoringSystem |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            let draftAlgorithm = [ [ str "Draft algorithm" ] |> link theme (ClickableLink (fun _ -> DraftAlgorithm |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            let payouts = [ [ str "Payouts" ] |> link theme (ClickableLink (fun _ -> Payouts |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            navbarDropDown theme (icon iconInfoSmall) [
                navbarDropDownItem theme false [ scoringSystem ]
                navbarDropDownItem theme false [ draftAlgorithm ]
                navbarDropDownItem theme false [ payouts ] ] |> Some
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut -> None
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
                yield Rct.ofOption serverStarted
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> ToggleNavbarBurger |> dispatch) navbarBurgerIsActive ]
            navbarMenu theme navbarData navbarBurgerIsActive [ 
                navbarStart [
                    yield Rct.ofOption authUserDropDown
                    yield navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ]
                    yield Rct.ofOption adminDropDown
                    yield Rct.ofOption infoDropDown ]
                navbarEnd [
#if TICK
                    navbarItem [ [ str (DateTimeOffset.UtcNow.LocalDateTime.ToString ("HH:mm:ss")) ] |> para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } ]
#endif
                    navbarItem [ [] |> button theme toggleThemeButton ] ] ] ] ]
// #endregion

let private renderStaticModal (useDefaultTheme, titleText, markdown) dispatch =
    let theme = getTheme useDefaultTheme
    cardModal theme [ [ bold titleText ] |> para theme paraCentredSmall ] ((fun _ -> HideStaticModal |> dispatch) |> Some) [ markdown |> contentFromMarkdown theme ]

let private markdownSyntaxKey = Guid.NewGuid ()

let private renderMarkdownSyntaxModal useDefaultTheme dispatch =
    let theme = getTheme useDefaultTheme
    let body = [ 
        [ str "As a very quick introduction to Markdown syntax, the following:" ] |> para theme paraCentredSmaller ; br
        textArea theme markdownSyntaxKey MARKDOWN_SYNTAX_MARKDOWN None [] false true ignore
        br ; [ str "will appear as:" ] |> para theme paraCentredSmaller ; br
        Markdown MARKDOWN_SYNTAX_MARKDOWN |> contentFromMarkdown theme ]
    cardModal theme [ [ bold "Markdown syntax" ] |> para theme paraCentredSmall ] ((fun _ -> HideStaticModal |> dispatch) |> Some) body

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter, onDismiss =
        let signIn, onDismiss = (fun _ -> SignIn |> dispatch), (fun _ -> CancelSignIn |> dispatch)
        match signInState.SignInStatus with
        | Some SignInPending -> true, Loading, ignore, None
        | Some (SignInFailed _) | None ->
            match validateUserName [] (UserName signInState.UserNameText), validatePassword (Password signInState.PasswordText) with
            | None, None -> false, Clickable (signIn, None), signIn, onDismiss |> Some
            | _ -> false, NotEnabled None, ignore, onDismiss |> Some
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
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Sign in" ] |> button theme { buttonLinkSmall with Interaction = signInInteraction } ] ]
    cardModal theme [ [ bold "Sign in" ] |> para theme paraCentredSmall ] onDismiss body

// #region TEMP-NMB...renderScores
let private renderScores useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ [ bold "Scores" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
// #endregion

let private renderUnauth (useDefaultTheme, unauthState, hasStaticModal, ticks) (dispatch:UnauthInput -> unit) =
    let hasModal = if hasStaticModal then true else match unauthState.SignInState with | Some _ -> true | None -> false
    let usersProjection = unauthState.UnauthProjections.UsersProjection
    let squadsProjection = unauthState.UnauthProjections.SquadsProjection
    div divDefault [
        match hasStaticModal, unauthState.SignInState with
        | false, Some signInState ->
            yield lazyViewOrHMR2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | _ -> ()
        match unauthState.CurrentUnauthPage with
        | NewsPage ->
            let newsState = unauthState.UnauthPageStates.NewsState
            yield lazyViewOrHMR2 News.Render.render (useDefaultTheme, newsState, None, usersProjection, hasModal, ticks) (NewsInput >> UnauthPageInput >> dispatch)

        | ScoresPage ->
            let _scoresState = unauthState.UnauthPageStates.ScoresState
            yield renderScores useDefaultTheme
            // TODO-SOON... yield lazyViewOrHMR2 Scores.Render.render (useDefaultTheme, scoresState, hasModal) (ScoresInput >> UnauthPageInput >> dispatch)

        | SquadsPage ->
            let squadsState = unauthState.UnauthPageStates.SquadsState
            yield lazyViewOrHMR2 Squads.Render.render (useDefaultTheme, squadsState, None, squadsProjection, None, hasModal) (SquadsInput >> UnauthPageInput >> dispatch)
        | FixturesPage ->
            match unauthState.UnauthPageStates.FixturesState with
            | Some fixturesState ->
                yield lazyViewOrHMR2 Fixtures.Render.render (useDefaultTheme, fixturesState, None, hasModal, ticks) (FixturesInput >> UnauthPageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is UnauthPage FixturesPage when UnauthPageStates.FixturesState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks) ]

let private renderChangePasswordModal (useDefaultTheme, changePasswordState) dispatch =
    let theme = getTheme useDefaultTheme
    let onDismiss = match changePasswordState.ChangePasswordStatus with | Some ChangePasswordPending -> None | Some _ | None -> (fun _ -> CancelChangePassword |> dispatch) |> Some
    let isChangingPassword, changePasswordInteraction, onEnter =
        let changePassword = (fun _ -> ChangePassword |> dispatch)
        match changePasswordState.ChangePasswordStatus with
        | Some ChangePasswordPending -> true, Loading, ignore
        | Some (ChangePasswordFailed _) | None ->
            let validPassword = validatePassword (Password changePasswordState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password changePasswordState.NewPasswordText) (Password changePasswordState.ConfirmPasswordText)
            match validPassword, validConfirmPassword with
            | None, None -> false, Clickable (changePassword, None), changePassword
            | _ -> false, NotEnabled None, ignore
    let errorText = match changePasswordState.ChangePasswordStatus with | Some (ChangePasswordFailed errorText) -> errorText |> Some | Some ChangePasswordPending | None -> None
    let body = [
        match changePasswordState.MustChangePasswordReason with
        | Some mustChangePasswordReason ->
            let because reasonText = sprintf "You must change your password because %s" reasonText
            let reasonText =
                match mustChangePasswordReason with
                | FirstSignIn -> because "this is the first time you have signed in"
                | PasswordReset -> because "it has been reset by a system administrator"
            yield notification theme notificationInfo [ [ str reasonText ] |> para theme paraCentredSmallest ]
            yield br
        | None -> ()
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter your new password (twice)" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme changePasswordState.NewPasswordKey changePasswordState.NewPasswordText (iconPasswordSmall |> Some) true changePasswordState.NewPasswordErrorText []
                true isChangingPassword (NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme changePasswordState.ConfirmPasswordKey changePasswordState.ConfirmPasswordText (iconPasswordSmall |> Some) true changePasswordState.ConfirmPasswordErrorText []
                false isChangingPassword (ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change password" ] |> button theme { buttonLinkSmall with Interaction = changePasswordInteraction } ]        ]
    cardModal theme [ [ bold "Change password" ] |> para theme paraCentredSmall ] onDismiss body

let private renderSigningOutModal useDefaultTheme =
    let theme = getTheme useDefaultTheme
    cardModal theme [ [ bold "Signing out" ] |> para theme paraCentredSmall ] None [ div divCentred [ icon iconSpinnerPulseLarge ] ]

// #region TEMP-NMB...renderScores | renderDrafts
let private renderDraftAdmin useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ [ bold "Draft administration" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
let private renderDrafts useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ [ bold "Drafts" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Coming soon" ] |> para theme paraCentredSmaller ]
// #endregion

let private renderAuth (useDefaultTheme, authState, hasStaticModal, ticks) dispatch =
    let hasModal = if hasStaticModal then true else match authState.ChangePasswordState, authState.SigningOut with | Some _, _ -> true | None, true -> true | None, false -> false
    let usersProjection = authState.UnauthProjections.UsersProjection
    let squadsProjection = authState.UnauthProjections.SquadsProjection
    div divDefault [
        match hasStaticModal, authState.ChangePasswordState with
        | false, Some changePasswordState ->
            yield lazyViewOrHMR2 renderChangePasswordModal (useDefaultTheme, changePasswordState) (ChangePasswordInput >> dispatch)
        | _ -> ()
        match hasStaticModal, authState.SigningOut with
        | false, true ->
            yield lazyViewOrHMR renderSigningOutModal useDefaultTheme
        | _ -> ()
        match authState.CurrentPage with
        | UnauthPage NewsPage ->
            let newsState = authState.UnauthPageStates.NewsState
            yield lazyViewOrHMR2 News.Render.render (useDefaultTheme, newsState, authState.AuthUser |> Some, usersProjection, hasModal, ticks) (NewsInput >> UPageInput >> PageInput >> dispatch)

        | UnauthPage ScoresPage ->
            let _scoresState = authState.UnauthPageStates.ScoresState
            yield renderScores useDefaultTheme
            // TODO-SOON... yield lazyViewOrHMR2 Scores.Render.render (useDefaultTheme, scoresState, hasModal) (ScoresInput >> UPageInput >> PageInput >> dispatch)

        | UnauthPage SquadsPage ->
            let squadsState = authState.UnauthPageStates.SquadsState
            let authUser = authState.AuthUser |> Some
            let currentDraftDto = match authState.AuthProjections.DraftsProjection with | Ready currentDraftDto -> currentDraftDto | Pending | Failed -> None
            yield lazyViewOrHMR2 Squads.Render.render (useDefaultTheme, squadsState, authUser, squadsProjection, currentDraftDto, hasModal) (SquadsInput >> UPageInput >> PageInput >> dispatch)
        | UnauthPage FixturesPage ->
            match authState.UnauthPageStates.FixturesState with
            | Some fixturesState ->
                yield lazyViewOrHMR2 Fixtures.Render.render (useDefaultTheme, fixturesState, authState.AuthUser |> Some, hasModal, ticks) (FixturesInput >> UPageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is UnauthPage FixturesPage when UnauthPageStates.FixturesState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks)

        | AuthPage DraftAdminPage ->
            match authState.AuthPageStates.DraftAdminState with
            | Some _draftAdminState ->
                yield renderDraftAdmin useDefaultTheme
               // TODO-SOON...  yield lazyViewOrHMR2 DraftAdmin.Render.render (useDefaultTheme, draftAdminState, hasModal) (DraftAdminInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage DraftAdminPage when AuthPageStates.DraftAdminState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks)

        | AuthPage UserAdminPage ->
            match authState.AuthPageStates.UserAdminState with
            | Some userAdminState ->
                yield lazyViewOrHMR2 UserAdmin.Render.render (useDefaultTheme, userAdminState, authState.AuthUser, usersProjection, hasModal) (UserAdminInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage UserAdminPage when AuthPageStates.UserAdminState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2018, message, ticks)

        | AuthPage DraftsPage ->
            let _draftsState = authState.AuthPageStates.DraftsState
            yield renderDrafts useDefaultTheme
            // TODO-SOON...  yield lazyViewOrHMR2 Drafts.Render.render (useDefaultTheme, draftsState, hasModal) (DraftsInput >> APageInput >> PageInput >> dispatch)

        | AuthPage ChatPage ->
            let chatState = authState.AuthPageStates.ChatState
            yield lazyViewOrHMR2 Chat.Render.render (useDefaultTheme, chatState, usersProjection, hasModal, ticks) (ChatInput >> APageInput >> PageInput >> dispatch) ]

let private renderContent state dispatch =
    let renderSpinner () = div divCentred [ icon iconSpinnerPulseLarge ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ [ str "Service unavailable" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Please try again later" ] |> para theme paraCentredSmaller ]
    let hasStaticModal = match state.StaticModal with | Some _ -> true | None -> false
    div divDefault [
        yield lazyViewOrHMR divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield lazyViewOrHMR renderSpinner ()
        | ServiceUnavailable ->
            yield lazyViewOrHMR renderServiceUnavailable state.UseDefaultTheme
        | Unauth unauthState ->
            yield renderUnauth (state.UseDefaultTheme, unauthState, hasStaticModal, state.Ticks) (UnauthInput >> AppInput >> dispatch) // note: renderUnauth has its own lazyViewOrHMR[n] handling
        | Auth authState ->
            yield renderAuth (state.UseDefaultTheme, authState, hasStaticModal, state.Ticks) (AuthInput >> AppInput >> dispatch) // note: renderAuth has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR divVerticalSpace 20 ]

let private renderFooter useDefaultTheme =
    let theme = getTheme useDefaultTheme
    footer theme true [
        container (Fluid |> Some) [
            [
                [ str "Written" ] |> link theme (SameWindow "https://github.com/aornota/sweepstake-2018") ; str " in "
                [ str "F#" ] |> link theme (SameWindow "http://fsharp.org/") ; str " using "
                [ str "Fable" ] |> link theme (SameWindow "http://fable.io/") ; str ", "
                [ str "Elmish" ] |> link theme (SameWindow "https://elmish.github.io/") ; str ", "
                [ str "Fulma" ] |> link theme (SameWindow "https://mangelmaxime.github.io/Fulma/") ; str " / "
                [ str "Bulma" ] |> link theme (SameWindow "https://bulma.io/") ; str " and "
                [ str "Giraffe" ] |> link theme (SameWindow "https://github.com/giraffe-fsharp/Giraffe/") ; str ". Developed in "
                [ str "Visual Studio Code" ] |> link theme (SameWindow "https://code.visualstudio.com/") ; str ". Best viewed with "
                [ str "Chrome" ] |> link theme (SameWindow "https://www.google.com/chrome/index.html") ; str ". Not especially mobile-friendly." ] |> para theme paraCentredSmallest ] ]

let render state dispatch =
    div divDefault [
        let serverStarted = match state.ConnectionState with | Connected connectionState -> connectionState.ServerStarted |> Some | NotConnected | InitializingConnection _ -> None
        yield lazyViewOrHMR2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, serverStarted, headerStatus state.AppState, headerPages state.AppState, state.Ticks) dispatch
        match state.StaticModal with
        | Some ScoringSystem ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Scoring system", (Markdown SCORING_SYSTEM_MARKDOWN)) dispatch
        | Some DraftAlgorithm ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Draft algorithm", (Markdown DRAFT_ALGORITHM_MARKDOWN)) dispatch
        | Some Payouts ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Payouts", (Markdown PAYOUTS_MARKDOWN)) dispatch
        | Some MarkdownSyntax ->
            yield lazyViewOrHMR2 renderMarkdownSyntaxModal state.UseDefaultTheme dispatch
        | None -> ()
        yield lazyViewOrHMR2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2018, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        yield renderContent state dispatch // note: renderContent has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR renderFooter state.UseDefaultTheme ]

(*let private draftNotificationMessage currentDraftDto =
    match currentDraftDto with
    | Some currentDraftDto ->
        let draftTextLower = currentDraftDto.DraftOrdinal |> draftTextLower
        let text =
            match currentDraftDto.DraftStatusDto with
            | PendingOpenDto (starts, ends) ->
                let starts, ends = starts.LocalDateTime, ends.LocalDateTime
                sprintf "The %s will open on %s and will close on %s" draftTextLower (starts |> dateAndTimeText) (ends |> dateAndTimeText)
            | OpenedDto ends ->
                let ends = ends.LocalDateTime
                sprintf "The %s is now open and will close on %s" draftTextLower (ends |> dateAndTimeText)
            | PendingProcessingDto -> sprintf "The %s will be processed soon" draftTextLower
            | FreeSelectionDto -> "There are no further drafts; please pick team/coach | goalkeeper | outfield players (as required)" // TODO-SOON: Finesse this...
        infoMessage text false |> Some
    | None -> None*)
