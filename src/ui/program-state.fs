module Aornota.Sweepstake2018.UI.Program.State

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Literals
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Pages.Chat.Common
open Aornota.Sweepstake2018.UI.Program.Common
open Aornota.Sweepstake2018.UI.Shared

open Aornota.UI.Common.LocalStorage
open Aornota.UI.Common.Notifications
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Shared

open System

open Elmish

open Fable.Core.JsInterop
open Fable.Import
module Brw = Fable.Import.Browser

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2018-ui-app-preferences"

#if DEBUG
let private random = Random ()
#endif

let private setBodyClass useDefaultTheme = Browser.document.body.className <- getThemeClass (getTheme useDefaultTheme).ThemeClass

let private readPreferencesCmd =
    let readPreferences () = async {
#if DEBUG
        do! Async.Sleep (random.Next (20, 100))
#endif
        return Option.map ofJson<Preferences> (readJson APP_PREFERENCES_KEY) }
    Cmd.ofAsync readPreferences () (Ok >> ReadingPreferencesInput >> AppInput) (Error >> ReadingPreferencesInput >> AppInput)

let private writePreferencesCmd state =
    let writePreferences uiState = async {
        let lastPage =
            match uiState.AppState with
            | Unauthenticated unauthenticatedState -> Some (UnauthenticatedPage unauthenticatedState.CurrentPage)
            | Authenticated authenticatedState -> Some authenticatedState.CurrentPage
            | _ -> None
        let jwt = match uiState.AppState with | Authenticated authenticatedState -> Some (Jwt (authenticatedState.AuthenticatedUser)) | _ -> None
        let preferences = { UseDefaultTheme = uiState.UseDefaultTheme ; SessionId = uiState.SessionId ; LastPage = lastPage ; Jwt = jwt }
        do writeJson APP_PREFERENCES_KEY (toJson preferences) }
    Cmd.ofAsync writePreferences state (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private initializeWsSub dispatch =
    let receiveServerWsApi (wsMessage:Brw.MessageEvent) : unit =
        try // note: expect wsMessage.data to be deserializable to ServerWsApi
            let serverWsApi = ofJson<ServerWsApi> <| unbox wsMessage.data
#if DEBUG
            if random.NextDouble () < 0.02 then failwith (sprintf "Fake error deserializing %A" serverWsApi)
#endif
            HandleServerWsApi serverWsApi |> dispatch
        with exn -> OnUiWsError (DeserializeServerWsApiError exn.Message) |> dispatch
    let wsUrl =
#if DEBUG
        sprintf "ws://localhost:%i" WS_PORT
#else       
        sprintf "ws://localhost:%i" WS_PORT // TODO-NMB-MEDIUM: Confirm "production" wsUrl (e.g. Azure app service? use "wss://..."?)...
#endif
    let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
    try
        let ws = Brw.WebSocket.Create wsApiUrl
        ws.onopen <- (fun _ -> ConnectingInput ws |> AppInput |> dispatch)
        ws.onerror <- (fun _ -> OnUiWsError (WsOnError wsApiUrl) |> dispatch)
        ws.onmessage <- receiveServerWsApi
        ()
    with _ -> OnUiWsError (WsOnError wsApiUrl) |> dispatch

let private sendUiWsApi (ws:Brw.WebSocket) (uiWsApi:UiWsApi) =
    if ws.readyState <> ws.OPEN then OnUiWsError (SendWsNotOpenError uiWsApi) |> Cmd.ofMsg
    else
        try
#if DEBUG
            if random.NextDouble () < 0.02 then failwith "Fake sendUiWsApiCmd error"
#endif
            ws.send (toJson uiWsApi)
            Cmd.none
        with exn -> OnUiWsError (SendWsOtherError (uiWsApi, exn.Message)) |> Cmd.ofMsg

let private shouldNeverHappenText text = sprintf "SHOULD NEVER HAPPEN -> %s" text

let private sendUiUnauthenticatedWsApiCmd (ws:Brw.WebSocket option) uiUnauthenticatedWsApi =
    match ws with
    | Some ws -> sendUiWsApi ws (UiUnauthenticatedWsApi uiUnauthenticatedWsApi)
    | None -> AddNotificationMessage (debugDismissableMessage (shouldNeverHappenText "sendUiUnauthenticatedWsApiCmd called when WebSocket is None")) |> Cmd.ofMsg

let private sendUiWsApiCmd (ws:Brw.WebSocket option) uiWsApi =
    match ws with
    | Some ws -> sendUiWsApi ws uiWsApi
    | None -> AddNotificationMessage (debugDismissableMessage (shouldNeverHappenText "sendUiWsApiCmd called when WebSocket is None")) |> Cmd.ofMsg

let private addNotificationMessage notificationMessage state = { state with NotificationMessages = notificationMessage :: state.NotificationMessages }

let private addDebugMessage debugText state = addNotificationMessage (debugDismissableMessage debugText) state
let private addInfoMessage infoText state = addNotificationMessage (infoDismissableMessage infoText) state
let private addWarningMessage warningText state = addNotificationMessage (warningDismissableMessage warningText) state
let private addDangerMessage dangerText state = addNotificationMessage (dangerDismissableMessage dangerText) state

let private shouldNeverHappen debugText state : State * Cmd<Input> = addDebugMessage (shouldNeverHappenText debugText) state, Cmd.none

let private error debugText toastText state : State * Cmd<Input> =
    addDebugMessage (sprintf "ERROR -> %s" debugText) state, match toastText with | Some toastText -> errorToastCmd toastText | None -> Cmd.none

let private appStateText appState =
    match appState with
    | ReadingPreferences -> "ReadingPreferences" | Connecting _ -> "Connecting" | ServiceUnavailable -> "ServiceUnavailable" | AutomaticallySigningIn _ -> "AutomaticallySigningIn"
    | Authenticated _ -> "Authenticated" | Unauthenticated _ -> "Unauthenticated"

let defaultSignInState userName signInStatus = {
    UserNameKey = Guid.NewGuid ()
    UserNameText = match userName with | Some userName -> userName | None -> String.Empty
    UserNameErrorText = None
    PasswordKey = Guid.NewGuid ()
    PasswordText = String.Empty
    PasswordErrorText = None
    FocusPassword = match userName with | Some _ -> true | None -> false
    SignInStatus = signInStatus }

let private defaultUnauthenticatedState currentPage signInState state =
    let unauthenticatedState = {
        CurrentPage = match currentPage with | Some currentPage -> currentPage | None -> News
        NewsState = ()
        SquadsState = ()
        SignInState = signInState }
    { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none

let private defaultAuthenticatedState authenticatedUser currentPage (unauthenticatedState:UnauthenticatedState option) state =
    let currentPage = match currentPage with | Some currentPage -> currentPage | None -> AuthenticatedPage ChatPage
    // Note: No actual need to call Chat.State.initialize here as will be initialized on demand - i.e. by ShowPage (AuthenticatedPage ChatPage) - but no harm in being pre-emptive.
    let chatState, chatCmd = Chat.State.initialize authenticatedUser (currentPage = AuthenticatedPage ChatPage)
    let authenticatedState = {
        AuthenticatedUser = authenticatedUser
        CurrentPage = currentPage
        NewsState = match unauthenticatedState with | Some unauthenticatedState -> unauthenticatedState.NewsState | None -> ()
        SquadsState = match unauthenticatedState with | Some unauthenticatedState -> unauthenticatedState.SquadsState | None -> ()
        DraftsState = ()
        ChatState = Some chatState
        SignOutStatus = None }
    { state with AppState = Authenticated authenticatedState }, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)

let initialize () =
    let state = {
        Ticks = 0<tick>
        NotificationMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
        StaticModal = None
        Ws = None
        AppState = ReadingPreferences }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let private handleUiWsError uiWsError state : State * Cmd<Input> =
    match uiWsError, state.AppState with
    | WsOnError wsApiUrl, Connecting _ ->
        let uiState = { state with AppState = ServiceUnavailable }
        error (sprintf "WsOnError when Connecting -> %s" wsApiUrl) (Some "Unable to create a web socket connection to the web server") uiState
    | WsOnError wsApiUrl, _ -> error (sprintf "WsOnError not when Connecting -> %s" wsApiUrl) (Some "An unexpected web socket error has occurred") state
    | SendWsNotOpenError uiWsApi, _ -> error (sprintf "SendWsNotOpenError -> %A" uiWsApi) (Some "The web socket connection to the web server has been closed") state
    | SendWsOtherError (uiWsApi, errorText), _ -> error (sprintf "SendWsOtherError -> %s -> %A" errorText uiWsApi) (Some "Unable to send a web socket message") state
    | DeserializeServerWsApiError errorText, _ -> error (sprintf "DeserializeServerWsApiError -> %s" errorText) (Some "Unable to process a received web socket message") state

let private handleServerWsError serverWsError state =
    match serverWsError with
    | ReceiveError errorText -> error (sprintf "Server ReceiveError -> %s" errorText) (Some "The web server was unable to receive a web socket message") state
    | DeserializeUiWsApiError errorText -> error (sprintf "Server DeserializeUiWsApiError -> %s" errorText) (Some"The web server was unable to process a web socket message") state

let private handleConnected (otherConnections, signedIn) jwt lastPage state =
    let toastCmd =
#if DEBUG
        let plural i = if i = 1 then String.Empty else "s"
        infoToastCmd (sprintf "%i other web socket connection%s | %i user%s signed-in" otherConnections (plural otherConnections) signedIn (plural signedIn))
#else
        Cmd.none
#endif
    let state, cmd =
        match jwt with
        | Some jwt -> { state with AppState = AutomaticallySigningIn (jwt, lastPage) }, AutoSignInWs jwt |> sendUiUnauthenticatedWsApiCmd state.Ws
        | None ->
            let lastPage = match lastPage with | Some (UnauthenticatedPage unauthenticatedPage) -> Some unauthenticatedPage | Some (AuthenticatedPage _) | None -> None
            let showPageCmd = match lastPage with | Some lastPage -> ShowUnauthenticatedPage lastPage |> UnauthenticatedInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
            // TEMP-NMB: Force sign-in Modal to be displayed...
            let showSignInCmd =
                ShowSignIn |> UnauthenticatedInput |> AppInput |> Cmd.ofMsg
            // ...or not...
                //Cmd.none
            // ...NMB-TEMP
            let state, cmd = defaultUnauthenticatedState None None state
            // ...NMB-TEMP
            state, Cmd.batch [ cmd ; showPageCmd ; showSignInCmd ]
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult result unauthenticatedState state =
    match unauthenticatedState.SignInState, result with
    | Some _, Ok authenticatedUser ->
        let currentPage = Some (UnauthenticatedPage unauthenticatedState.CurrentPage)
        let state, cmd = defaultAuthenticatedState authenticatedUser currentPage (Some unauthenticatedState) state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd "You have signed in" ]
    | Some signInState, Error errorText ->
        let state, toastCmd = error (sprintf "SignInResultWs -> %s" errorText) (Some (sprintf "Unable to sign in as %s" signInState.UserNameText)) state // TODO-NMB-MEDIUM: Is this [error] necessary, e.g. if errorText rendered elsewhere?...
        let signInState = { signInState with SignInStatus = Some (Failed errorText) }
        { state with AppState = Unauthenticated { unauthenticatedState with SignInState = Some signInState } }, toastCmd
    | None, _ -> shouldNeverHappen (sprintf "Unexpected 'SignInResult' when SignInState is None -> %A" result) state

let private handleAutoSignInResult result (jwt:AuthenticatedUser) lastPage state =
    match result with
    | Ok authenticatedUser -> // TODO-NMB-LOW: Check authenticatedUser vs. _jwt?...
        let showPageCmd = match lastPage with | Some lastPage -> ShowPage lastPage |> AuthenticatedInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultAuthenticatedState authenticatedUser None None state
        state, Cmd.batch [ showPageCmd ; cmd ; successToastCmd "You have been automatically signed in" ]
    | Error errorText ->
        let state, toastCmd = error (sprintf "AutoSignInResultWs -> %s" errorText) (Some (sprintf "Unable to automatically sign in as %s" jwt.UserName)) state // TODO-NMB-MEDIUM: Is this [error] necessary, e.g. if errorText rendered elsewhere?...
        let lastPage = match lastPage with | Some (UnauthenticatedPage unauthenticatedPage) -> Some unauthenticatedPage | Some (AuthenticatedPage _) | None -> None
        let signInState = defaultSignInState (Some jwt.UserName) (Some (Failed errorText))
        let showPageCmd = match lastPage with | Some lastPage -> ShowUnauthenticatedPage lastPage |> UnauthenticatedInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultUnauthenticatedState None (Some signInState) state
        state, Cmd.batch [ showPageCmd ; cmd ; toastCmd ]

let private handleSignOutResult result authenticatedState state =
    match authenticatedState.SignOutStatus, result with
    | Some Pending, Ok _sessionId -> // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
        let currentPage = match authenticatedState.CurrentPage with | UnauthenticatedPage unauthenticatedPage -> Some unauthenticatedPage | _ -> None
        let state, cmd = defaultUnauthenticatedState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd "You have signed out" ]
    | Some Pending, Error errorText ->
        let state, toastCmd = error (sprintf "SignOutResultWs -> %s" errorText) (Some "Unable to sign out") state // TODO-NMB-MEDIUM: Is this [error] necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Authenticated { authenticatedState with SignOutStatus = Some (Failed errorText) } }, toastCmd
    | Some (Failed _), _ | None, _ -> shouldNeverHappen (sprintf "Unexpected 'SignOutResult' when SignOutStatus is %A -> %A" authenticatedState.SignOutStatus result) state

let private handleAutoSignOut _sessionId (authenticatedState:AuthenticatedState) state = // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
    let currentPage = match authenticatedState.CurrentPage with | UnauthenticatedPage unauthenticatedPage -> Some unauthenticatedPage | _ -> None
    let state, cmd = defaultUnauthenticatedState currentPage None state
    state, Cmd.batch [ cmd ; writePreferencesCmd state ; warningToastCmd "You have been automatically signed out" ]

let private handleServerAppWsApi serverAppWsApi state =
    match serverAppWsApi, state.AppState with
    | ServerWsErrorWs serverWsError, _ -> handleServerWsError serverWsError state
    | ConnectedWs (otherConnections, signedIn), Connecting (jwt, lastPage) -> handleConnected (otherConnections, signedIn) jwt lastPage state
    | SignInResultWs result, Unauthenticated unauthenticatedState -> handleSignInResult result unauthenticatedState state
    | AutoSignInResultWs result, AutomaticallySigningIn (Jwt jwt, lastPage) -> handleAutoSignInResult result jwt lastPage state
    | SignOutResultWs result, Authenticated authenticatedState -> handleSignOutResult result authenticatedState state
    | AutoSignOutWs sessionId, Authenticated authenticatedState -> handleAutoSignOut sessionId authenticatedState state
    | OtherUserSignedIn userName, Authenticated _ -> state, infoToastCmd (sprintf "%s has signed in" userName)
    | OtherUserSignedOut userName, Authenticated _ -> state, infoToastCmd (sprintf "%s has signed out" userName)
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerAppWsApi when %s -> %A" (appStateText appState) serverAppWsApi) state

let private handleServerWsApi serverWsApi state =
    match serverWsApi, state.AppState with
    | ServerAppWsApi serverAppWsApi, _ -> handleServerAppWsApi serverAppWsApi state
    | ServerChatWsApi serverChatWsApi, Authenticated _ -> state, ReceiveServerChatWsApi serverChatWsApi |> ChatInput |> AuthenticatedInput |> AppInput |> Cmd.ofMsg
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerWsApi when %s -> %A" (appStateText appState) serverWsApi) state

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = Connecting (preferences.Jwt, preferences.LastPage) }, Cmd.ofSub initializeWsSub
    | Ok None -> { state with AppState = Connecting (None, None) }, Cmd.ofSub initializeWsSub
    | Error exn ->
        let state, _ = error (sprintf "ReadPreferencesResult -> %s" exn.Message) None state
        state, ReadingPreferencesInput (Ok None) |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with Ws = Some ws }, Cmd.none

let private handleUnauthenticatedInput unauthenticatedInput unauthenticatedState state =
    match unauthenticatedInput, unauthenticatedState.SignInState with
    | ShowSignIn, None ->
        let unauthenticatedState = { unauthenticatedState with SignInState = Some (defaultSignInState None None) }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | ShowUnauthenticatedPage unauthenticatedPage, None ->
        if unauthenticatedState.CurrentPage <> unauthenticatedPage then
            // TODO-NMB-MEDIUM: Initialize "optional" pages (if required) and toggle "IsCurrent" for relevant pages...
            let unauthenticatedState = { unauthenticatedState with CurrentPage = unauthenticatedPage }
            let state = { state with AppState = Unauthenticated unauthenticatedState }
            state, writePreferencesCmd state
        else state, Cmd.none
    | NewsInputU, None -> shouldNeverHappen "Unexpected NewsInputU -> NYI" state
    | SquadsInputU, None -> shouldNeverHappen "Unexpected SquadsInputU -> NYI" state
    | SignInInput (UserNameTextChanged userNameText), Some signInState ->
        let signInState = { signInState with UserNameText = userNameText ; UserNameErrorText = validateUserNameText userNameText }
        let unauthenticatedState = { unauthenticatedState with SignInState = Some signInState }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | SignInInput (PasswordTextChanged passwordText), Some signInState ->
        let signInState = { signInState with PasswordText = passwordText ; PasswordErrorText = validatePasswordText passwordText }
        let unauthenticatedState = { unauthenticatedState with SignInState = Some signInState }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | SignInInput SignIn, Some signInState -> // note: assume no need to validate unauthenticatedState.UserNameText or unauthenticatedState.PasswordText (i.e. because App.Render.renderUnauthenticated will ensure that SignIn can only be dispatched when valid)
        let signInState = { signInState with SignInStatus = Some Pending }
        let unauthenticatedState = { unauthenticatedState with SignInState = Some signInState }
        let cmd = SignInWs (state.SessionId, signInState.UserNameText, signInState.PasswordText) |> sendUiUnauthenticatedWsApiCmd state.Ws
        { state with AppState = Unauthenticated unauthenticatedState }, cmd
    | SignInInput CancelSignIn, Some _ ->
        let unauthenticatedState = { unauthenticatedState with SignInState = None }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | _, _ -> shouldNeverHappen (sprintf "Unexpected UnauthenticatedInput when SignIsState is %A -> %A" unauthenticatedState.SignInState unauthenticatedInput) state

let private handleAuthenticatedInput authenticatedInput authenticatedState state =
    match authenticatedInput, authenticatedState.SignOutStatus with
    | ShowPage page, None ->
        if authenticatedState.CurrentPage <> page then
            let chatState, chatCmd =
                match page, authenticatedState.ChatState with
                | AuthenticatedPage ChatPage, None ->
                    let chatState, chatCmd = Chat.State.initialize authenticatedState.AuthenticatedUser true
                    Some chatState, chatCmd
                | _, Some chatState -> Some chatState, ToggleChatIsCurrentPage (page = AuthenticatedPage ChatPage) |> Cmd.ofMsg
                | _, None -> None, Cmd.none
            // TODO-NMB-MEDIUM: Initialize other "optional" pages (if required) and toggle "IsCurrent" for other relevant pages...
            let authenticatedState = { authenticatedState with CurrentPage = page ; ChatState = chatState }
            let chatCmd = chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)
            let state = { state with AppState = Authenticated authenticatedState }
            state, Cmd.batch [ chatCmd ; writePreferencesCmd state ]
        else state, Cmd.none
    | NewsInputA, None -> shouldNeverHappen "Unexpected NewsInputA -> NYI" state
    | SquadsInputA, None -> shouldNeverHappen "Unexpected SquadsInputA -> NYI" state
    | DraftsInput, None -> shouldNeverHappen "Unexpected DraftsInput -> NYI" state
    | ChatInput ShowMarkdownSyntaxModal, None -> { state with StaticModal = Some MarkdownSyntax }, Cmd.none
    | ChatInput (SendAuthenticatedWsApi (_authenticatedUser, uiAuthenticatedWsApi)), None -> // TODO-NMB-LOW: Check _authenticatedUser vs. authenticatedState.AuthenticatedUser?...
        state, UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, uiAuthenticatedWsApi) |> sendUiWsApiCmd state.Ws
    | ChatInput chatInput, None ->
        match authenticatedState.ChatState with
        | Some chatState ->
            let chatState, chatCmd = Chat.State.transition chatInput chatState
            { state with AppState = Authenticated { authenticatedState with ChatState = Some chatState } }, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)
        | None -> shouldNeverHappen "Unexpected ChatInput when ChatState is None" state
    | SignOut, None ->
        let cmd = UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, SignOutWs) |> sendUiWsApiCmd state.Ws
        { state with AppState = Authenticated { authenticatedState with SignOutStatus = Some Pending } }, cmd
    | CancelSignOut, Some (Failed _) ->
        let currentPage = match authenticatedState.CurrentPage with | UnauthenticatedPage unauthenticatedPage -> Some unauthenticatedPage | _ -> None
        let state, cmd = defaultUnauthenticatedState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd "You have signed out" ]
    | ChangePassword, None -> state, warningToastCmd "Change password functionality is coming soon"
    | _, _ -> shouldNeverHappen (sprintf "Unexpected AuthenticatedInput when SignOutStatus is %A -> %A" authenticatedState.SignOutStatus authenticatedInput) state

let private handleAppInput appInput state =
    match appInput, state.AppState with
    | ReadingPreferencesInput result, ReadingPreferences -> handleReadingPreferencesInput result state
    | ConnectingInput ws, Connecting _ -> handleConnectingInput ws state
    | UnauthenticatedInput unauthenticatedInput, Unauthenticated unauthenticatedState -> handleUnauthenticatedInput unauthenticatedInput unauthenticatedState state
    | AuthenticatedInput authenticatedInput, Authenticated authenticatedState -> handleAuthenticatedInput authenticatedInput authenticatedState state
    | _, appState -> shouldNeverHappen (sprintf "Unexpected AppInput when %s -> %A" (appStateText appState) appInput) state

let transition input state =
    match input with
#if TICK
    | Tick -> { state with Ticks = state.Ticks + 1<tick> }, Cmd.none
#endif
    | AddNotificationMessage notificationMessage -> addNotificationMessage notificationMessage state, Cmd.none
    | DismissNotificationMessage notificationId -> { state with NotificationMessages = state.NotificationMessages |> removeNotificationMessage notificationId }, Cmd.none // note: silently ignore unknown notificationId
    | ToggleTheme ->
        let state = { state with UseDefaultTheme = not state.UseDefaultTheme }
        setBodyClass state.UseDefaultTheme
        state, writePreferencesCmd state
    | ToggleNavbarBurger -> { state with NavbarBurgerIsActive = not state.NavbarBurgerIsActive }, Cmd.none
    | ShowStaticModal staticModal -> { state with StaticModal = Some staticModal }, Cmd.none
    | HideStaticModal -> { state with StaticModal = None }, Cmd.none
    | WritePreferencesResult (Ok _) -> state, Cmd.none
    | WritePreferencesResult (Error exn) -> error (sprintf "WritePreferencesResult -> %s" exn.Message) None state // note: no need for toast
    | OnUiWsError uiWsError -> handleUiWsError uiWsError state
    | HandleServerWsApi serverWsApi -> handleServerWsApi serverWsApi state
    | AppInput appInput -> handleAppInput appInput state
