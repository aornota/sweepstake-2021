module Aornota.Sweepstake2018.UI.Render

open Aornota.Sweepstake2018.Shared
open Aornota.Sweepstake2018.UI.Common

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma

let private renderHeader theme state dispatch =
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        match state.Status with
        | ReadingPreferences -> [ para theme paraStatus [ str "Reading preferences..." ] ]
        | InitializingWS -> [ para theme paraStatus [ str "Initializing..." ] ]
        | ServiceUnavailable -> [ para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } [ str "Service unavailable - please try again later" ] ]
        | NotConnected _ -> [ para theme paraStatus [ str "Not connected" ] ]
        | Connecting _ -> [ para theme paraStatus [ str "Connecting..." ] ]
        | Connected (connection, _, _, _) ->
            [
                para theme paraStatus [ str "Connected as " ; bold connection.Nickname ]
                para theme paraDefaultSmallest [ link theme (ClickableLink (fun _ -> dispatch Disconnect)) [ str "Disconnect" ] ]
            ]
        | Disconnecting _ -> [ para theme paraStatus [ str "Disconnecting..." ] ]
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

let private renderNotConnected theme (ConnectionId connectionId, nicknameText, errorText, connecting) dispatch =
    let connectInteraction =
        match connecting with
        | true -> Loading
        | false -> match validateNicknameText nicknameText with | Some _ -> NotEnabled None | None -> Clickable ((fun _ -> dispatch Connect), None)
    columnContent [
        para theme paraCentredSmaller [ str "Please enter your nickname" ]
        hr theme false
        field theme { fieldDefault with Grouped = Some Centred } [
            textBox theme connectionId nicknameText (Some iconUserSmall) errorText true connecting (NicknameTextChanged >> dispatch) (fun _ -> dispatch Connect)
            button theme { buttonSuccessSmall with Interaction = connectInteraction } [ span theme spanDefault [ str "Connect" ] ] ] ]

let private renderConnected theme (connection, messages, MessageId messageId, messageText) dispatch =
    let sendMessageInteraction = match validateMessageText messageText with | Some _ -> NotEnabled None | None -> Clickable ((fun _ -> dispatch SendMessage), None)
    columnContent [
        para theme paraCentredSmaller [ str "Messages" ]
        hr theme false
        field theme { fieldDefault with Grouped = Some FullWidth } [
            // TEMP-NMB: textArea...
            //textArea theme messageId messageText true (MessageTextChanged >> dispatch) ]
            // ...or textBox...
            textBox theme messageId messageText (Some iconFileSmall) None true false (MessageTextChanged >> dispatch) (fun _ -> dispatch SendMessage) ]
            // ...NMB-TEMP
        field theme { fieldDefault with Grouped = Some RightAligned } [
            button theme { buttonSuccessSmall with Interaction = sendMessageInteraction } [ span theme spanDefault [ str "Send message" ] ] ]
        hr theme false
        // TODO-NMB: Render messages: Sent [Dark?] | SendFailed [Danger?] | Received [Primary for self? Info for other?]...
    ]

let private renderContent theme state dispatch =
    div divDefault [
        yield divVerticalSpace 20
        match state.Status with
        | ReadingPreferences | InitializingWS | Disconnecting _ ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | ServiceUnavailable ->
            yield columnContent [ para theme paraCentredSmall [ str "Service unavailable" ] ; hr theme false ; para theme paraCentredSmaller [ str "Please try again later" ] ]
        | NotConnected (connectionId, nicknameText, validationErrorText, connectResultErrorText) ->
            yield renderNotConnected theme (connectionId, nicknameText, (if Option.isSome validationErrorText then validationErrorText else connectResultErrorText), false) dispatch
        | Connecting connection ->
            yield renderNotConnected theme (connection.ConnectionId, connection.Nickname, None, true) dispatch
        | Connected (connection, messages, messageId, messageText) ->
            yield renderConnected theme (connection, messages, messageId, messageText) dispatch
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
        yield! renderDebugMessages theme SWEEPSTAKE_2018 state.DebugMessages (DismissDebugMessage >> dispatch)
        yield renderContent theme state dispatch
        yield renderFooter theme ]
