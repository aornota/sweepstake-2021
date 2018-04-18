module Aornota.Sweepstake2018.UI.Program.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Program.Common

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.Render.Markdown
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open System

open Elmish.React.Common

module Rct = Fable.Helpers.React

type private HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authenticatedUser : AuthenticatedUser

let [<Literal>] private SCORING_SYSTEM_MARKDOWN = """Each sweepstake team will consist of a **team/coach**, **1 goalkeeper** and **10 outfield players**.

(Although outfield players have been categorized as defenders, midfielders or forwards, you can choose any combination you like, e.g. if you want to go for eight defenders and two
midfielders - the no-longer-fashionable [except in Northern Ireland] 8-2-0 formation - please do.)

The **team/coach** will score (or lose) points for:
 - **winning** a match: **20** or **16** or **12** (see below)
 - **drawing** a match: **8** or **6** or **4** (see below)
 - a team player receiving a **yellow card**: **-1**
 - a team player receiving a **red card**: **-3**

(If a player receives a second yellow card in a match, the two yellow cards will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight"
red card, both cards will be scored.)

Where multiple possible scores are given above, the score will depend on whether the team and their opponents are in the top 16 seeds:
 - if the team **is** a top 16 seed but their opponents are **not**, the **lowest** score will apply
 - if the team is **not** a top 16 seed but their opponents **are**, the **highest** score will apply
 - if **both** teams are top 16 seeds - or if **neither** team is - the **middle** score will apply

The top 16 seeds are (in order): Russia; Germany; Brazil; Portugal; Argentina; Belgium; Poland; France; Spain; Peru; Switzerland; England; Colombia; Mexico; Uruguay; and Croatia.

The remaining teams are: Denmark; Iceland; Costa Rica; Sweden; Tunisia; Egypt; Senegal; Iran; Serbia; Nigeria; Australia; Japan; Morocco; Panama; South Korea; and Saudi Arabia.

(Note that Russia are only in the top 16 seeds because they are hosting the tournament; based on the October 2017 world rankings, they are the worst team participating.)

**All players** will score (or lose) points for:
 - being named **man-of-the-match**: **15**
 - _**scoring**_ a **goal** or a **penalty**: **12**
 - _**assisting**_ a **goal**: **3** (note that a goal cannot be assisted by the same player who scored the goal - unless they win a free kick and then score directly from it)
 - _**winning**_ a **penalty**: **3** (note that a penalty can be won by the same player who took the penalty)
 - _**missing**_ a **penalty**: **-6**
 - _**scoring**_ an **own goal**: **-6**
 - receiving a **yellow card**: **-2**
 - receiving a **red card**: **-6**

(A penalty will be considered as "missed" irrespective of whether the goalkeeper touched the ball. And again, if a player receives a second yellow card in a match, the two yellow cards
will be scored as a red card instead; however, if a player receives a yellow card followed by a "straight" red card, both cards will be scored.)

In addition, **goalkeepers** will score points for:
 - keeping a **clean sheet**: **12**
 - _**saving**_ a **penalty**: **12**

Note that outfield players can also score "goalkeeper" points if they end up playing in goal. (It probably won't happen - but you never know...)

(If more than one goalkeeper features for a team in a match, the "clean sheet" points will be awarded to whichever goalkeeper played more "regulation" minutes; if they played the same
amount of minutes, the points will be shared. A penalty will only be considered as "saved" if the goalkeeper touched the ball.)

Information about assists and such will be nicked from <https://www.whoscored.com/>.

As always, points can only be scored for goals / penalties / assists / &c. during normal time and extra time. **Penalty shootouts do not contribute to the scoring** [except to the extent 
that they determine who wins the match] - well, unless a player manages to get booked or sent-off during the shootout. Stranger things have happened..."""

let [<Literal>] private PAYOUTS_MARKDOWN = """**To be confirmed** - but based on the [world-famous Euro 2016 sweepstake](https://aornota.github.io/sweepstake.2016/), probably something like:
 - £60 for first place
 - £30 for second place
 - £20 for third place
 - £10 for the [_деревянная ложка_](https://translate.google.co.uk/#auto/en/%D0%B4%D0%B5%D1%80%D0%B5%D0%B2%D1%8F%D0%BD%D0%BD%D0%B0%D1%8F%20%D0%BB%D0%BE%D0%B6%D0%BA%D0%B0)"""

let [<Literal>] private MARKDOWN_SYNTAX_MARKDOWN = """# Markdown syntax
### A very quick introduction
Text can be:
- **emboldened**
- _italicized_
- **_emboldened and italicized_**
- ~~struck-through~~

This is a paragraph.
This is part of the same paragraph.

But this is a new paragraph.

This is a picture by the wonderful Gregory Kondos:

![River Trees](https://tinyurl.com/y8qlfx5o)

And here's a Matt Miles quote [from _Dark Mountain_ issue 11]:
> The immigrants of the Global South, the cultures we've turned our backs on even as we profit from
> their labour, are the indicator species of our own societal collapse. The most sensitive and
> susceptible elements of our own species - the ones from whom everything has already been taken,
> the ones who have no recourse to technological mediation, whose subsistence economies have
> already been wrecked by globalization, whose land succumbs to the rising seas, whose societies
> have been destroyed by imperial land grabs and resource wars - they are here now, knocking on
> our front doors, because they have nowhere else to go. On a planet dominated by the movements of
> human beings, we are our own indicator species.
---
Made possible thanks to [Marked.js](https://marked.js.org/#/README.md)."""

let private headerStatus (appState:AppState) =
    match appState with
    | ReadingPreferences -> ReadingPreferencesHS | Connecting _ -> ConnectingHS | ServiceUnavailable -> ServiceUnavailableHS | AutomaticallySigningIn _ -> SigningIn
    | Unauthenticated unauthenticatedState ->
        match unauthenticatedState.SignInState with
        | Some signInState -> match signInState.SignInStatus with | Some Pending -> SigningIn | Some (Failed _) | None -> NotSignedIn
        | None -> NotSignedIn
    | Authenticated authenticatedState -> match authenticatedState.SignOutStatus with | Some _ -> SigningOut | None -> SignedIn authenticatedState.AuthenticatedUser

let private headerPages (appState:AppState) =
    match appState with
    | Unauthenticated unauthenticatedState ->
        [
            "News", unauthenticatedState.CurrentPage = News, ShowUnauthenticatedPage News |> UnauthenticatedInput
            "Squads", unauthenticatedState.CurrentPage = Squads, ShowUnauthenticatedPage Squads |> UnauthenticatedInput
        ]
    | Authenticated authenticatedState ->
        // TODO-NMB-LOW: Finesse handling of "unseen counts" (i.e. something better than count-in-parentheses)...
        let unseenChatCount = match authenticatedState.ChatState with | Some chatState -> Some chatState.UnseenCount | None -> None
        let chatText = match unseenChatCount with | Some unseenCount when unseenCount > 0 -> sprintf "Chat (%i)" unseenCount | _ -> "Chat"
        [
            "News", authenticatedState.CurrentPage = UnauthenticatedPage News, ShowPage (UnauthenticatedPage News) |> AuthenticatedInput
            "Squads", authenticatedState.CurrentPage = UnauthenticatedPage Squads, ShowPage (UnauthenticatedPage Squads) |> AuthenticatedInput
            "Drafts", authenticatedState.CurrentPage = AuthenticatedPage Drafts, ShowPage (AuthenticatedPage Drafts) |> AuthenticatedInput
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
            let changePassword = link theme (ClickableLink (fun _ -> ChangePassword |> AuthenticatedInput |> AppInput |> dispatch)) [ str "Change password" ]
            let signOut = link theme (ClickableLink (fun _ -> SignOut |> AuthenticatedInput |> AppInput |> dispatch)) [ str "Sign out" ]
            Some (navbarDropDown theme (icon iconUserSmall) [
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ changePassword ] ]
                    navbarDropDownItem theme false [ para theme paraDefaultSmallest [ signOut ] ] ])
        | _ -> None
    let pageTabs =
        headerPages |> List.map (fun (text, isActive, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = ClickableLink (fun _ -> appInput |> AppInput |> dispatch) })
    let otherLinks =
        match headerStatus with
        | NotSignedIn | SignedIn _ ->
            [
                navbarItem [ para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowStaticModal ScoringSystem |> dispatch)) [ str "Scoring system" ] ] ]
                navbarItem [ para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> ShowStaticModal Payouts |> dispatch)) [ str "Payouts" ] ] ]
            ]
        | _ -> []
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
                    yield Rct.ofOption authenticatedUserDropDown
                    yield navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ]
                    yield! otherLinks ]
                navbarEnd [
#if TICK
                    navbarItem [ para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } [ str (DateTime.Now.ToString ("HH:mm:ss")) ] ]
#endif
                    navbarItem [ button theme toggleThemeButton [] ] ] ] ] ]

let private renderStaticModal (useDefaultTheme, titleText, markdown) dispatch =
    let theme = getTheme useDefaultTheme
    // TODO-NMB-HIGH: Theme-ing, i.e. useDefaultTheme and sweepstake-2018.css (&c.)...
    Fulma.Components.Modal.modal [ Fulma.Components.Modal.IsActive true ] [
        Fulma.Components.Modal.background [] []
        Fulma.Components.Modal.Card.card [] [
            Fulma.Components.Modal.Card.head [] [
                Fulma.Components.Modal.Card.title [] [ para theme paraCentredSmall [ str titleText ] ]
                Fulma.Elements.Delete.delete [ Fulma.Elements.Delete.OnClick (fun _ -> HideStaticModal |> dispatch) ] [] ]
            Fulma.Components.Modal.Card.body [] [ content [ htmlFromMarkdown markdown ] ] ] ]

let private markdownCheatSheetKey = Guid.NewGuid ()

let private renderMarkdownSyntaxModal useDefaultTheme dispatch =
    let theme = getTheme useDefaultTheme
    // TODO-NMB-HIGH: Theme-ing, i.e. useDefaultTheme and sweepstake-2018.css (&c.)...
    Fulma.Components.Modal.modal [ Fulma.Components.Modal.IsActive true ] [
        Fulma.Components.Modal.background [] []
        Fulma.Components.Modal.Card.card [] [
            Fulma.Components.Modal.Card.head [] [
                Fulma.Components.Modal.Card.title [] [ para theme paraCentredSmall [ str "Markdown syntax" ] ]
                Fulma.Elements.Delete.delete [ Fulma.Elements.Delete.OnClick (fun _ -> HideStaticModal |> dispatch) ] [] ]
            Fulma.Components.Modal.Card.body [] [ 
                para theme paraCentredSmaller [ str "As a very quick introduction to Markdown syntax, the following:" ] ; br
                textArea theme markdownCheatSheetKey MARKDOWN_SYNTAX_MARKDOWN None [] false true ignore
                br ; para theme paraCentredSmaller [ str "will appear as:" ] ; br
                notification theme notificationLight [ content [ htmlFromMarkdown (Markdown MARKDOWN_SYNTAX_MARKDOWN) ] ] ] ] ]

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let isSigningIn, signInInteraction, onEnter =
        let signIn = (fun _ -> SignIn |> dispatch)
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
                    textBox theme signInState.UserNameKey signInState.UserNameText (Some iconUserSmall) false signInState.UserNameErrorText [] (not signInState.FocusPassword) isSigningIn
                        (UserNameTextChanged >> dispatch) ignore ]
                field theme { fieldDefault with Grouped = Some Centred } [
                    textBox theme signInState.PasswordKey signInState.PasswordText (Some iconPasswordSmall) true signInState.PasswordErrorText [] signInState.FocusPassword isSigningIn
                        (PasswordTextChanged >> dispatch) onEnter ]
                field theme { fieldDefault with Grouped = Some Centred } [ button theme { buttonSuccessSmall with Interaction = signInInteraction } [ str "Sign in" ] ] ] ] ]

// TEMP-NMB...
let private renderNews useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "News" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
let private renderSquads useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ para theme paraCentredSmall [ str "Squads" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
// ...NMB-TEMP

let private renderUnauthenticated (useDefaultTheme, unauthenticatedState, _ticks) (dispatch:UnauthenticatedInput -> unit) =
    div divDefault [
        match unauthenticatedState.SignInState with
        | Some signInState ->
            yield lazyView2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | None -> ()
        match unauthenticatedState.CurrentPage with
        // TEMP-NMB: No lazyView since renderNews | renderSquads will return much the same until implemented properly...
        | News ->
            yield renderNews useDefaultTheme
        | Squads ->
            yield renderSquads useDefaultTheme ]

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
let private renderDrafts useDefaultTheme =
    let theme = getTheme useDefaultTheme
    columnContent [ para theme paraCentredSmall [ str "Drafts" ] ; hr theme false ; para theme paraCentredSmaller [ str "Coming soon" ] ]
// ...NMB-TEMP

let private renderAuthenticated (useDefaultTheme, authenticatedState, ticks) dispatch =
    div divDefault [
        match authenticatedState.SignOutStatus with
        | Some signOutStatus ->
            yield lazyView2 renderSignOutModal (useDefaultTheme, signOutStatus) dispatch
        | None -> ()
        match authenticatedState.CurrentPage with
        // TEMP-NMB: No lazyView since renderNews | renderSquads | renderDrafts will return much the same until implemented properly...
        | UnauthenticatedPage News ->
            yield renderNews useDefaultTheme
        | UnauthenticatedPage Squads ->
            yield renderSquads useDefaultTheme
        | AuthenticatedPage Drafts ->
            yield renderDrafts useDefaultTheme
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
        yield lazyView2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, headerStatus state.AppState, headerPages state.AppState, state.Ticks) dispatch
        match state.StaticModal with
        | Some ScoringSystem ->
            yield lazyView2 renderStaticModal (state.UseDefaultTheme, "Scoring system", (Markdown SCORING_SYSTEM_MARKDOWN)) dispatch
        | Some Payouts ->
            yield lazyView2 renderStaticModal (state.UseDefaultTheme, "Payouts", (Markdown PAYOUTS_MARKDOWN)) dispatch
        | Some MarkdownSyntax ->
            yield lazyView2 renderMarkdownSyntaxModal state.UseDefaultTheme dispatch
        | None -> ()
        yield lazyView2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2018, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        yield renderContent state dispatch // note: renderContent has its own lazyView handling
        yield lazyView renderFooter state.UseDefaultTheme ]
