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
            | Unauth unauthState -> Some (UnauthPage unauthState.CurrentUnauthPage)
            | Auth authState -> Some authState.CurrentPage
            | _ -> None
        let jwt = match uiState.AppState with | Auth authState -> Some (Jwt (authState.AuthUser)) | _ -> None
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
        sprintf "wss://sweepstake-2018.azurewebsites.net:%i" WS_PORT // TODO-NMB-MEDIUM: Confirm "production" wsUrl (e.g. Azure app service? use "wss://..."?)...
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

let private sendUiUnauthWsApiCmd (ws:Brw.WebSocket option) uiUnauthWsApi =
    match ws with
    | Some ws -> sendUiWsApi ws (UiUnauthWsApi uiUnauthWsApi)
    | None -> AddNotificationMessage (debugDismissableMessage (shouldNeverHappenText "sendUiUnauthWsApiCmd called when WebSocket is None")) |> Cmd.ofMsg

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

let private addDebugError debugText toastText state : State * Cmd<Input> =
    addDebugMessage (sprintf "ERROR -> %s" debugText) state, match toastText with | Some toastText -> errorToastCmd toastText | None -> Cmd.none

let private addError errorText state = addDangerMessage errorText state

let private appStateText appState =
    match appState with
    | ReadingPreferences -> "ReadingPreferences" | Connecting _ -> "Connecting" | ServiceUnavailable -> "ServiceUnavailable" | AutomaticallySigningIn _ -> "AutomaticallySigningIn"
    | Unauth _ -> "Unauth" | Auth _ -> "Auth"

let defaultSignInState userName signInStatus = {
    UserNameKey = Guid.NewGuid ()
    UserNameText = match userName with | Some userName -> userName | None -> String.Empty
    UserNameErrorText = None
    PasswordKey = Guid.NewGuid ()
    PasswordText = String.Empty
    PasswordErrorText = None
    FocusPassword = match userName with | Some _ -> true | None -> false
    SignInStatus = signInStatus }

let private defaultUnauthState currentPage signInState state =
    let unauthState = {
        CurrentUnauthPage = match currentPage with | Some currentPage -> currentPage | None -> News
        UnauthPageStates = { NewsState = () ; SquadsState = () }
        SignInState = signInState }
    { state with AppState = Unauth unauthState }, Cmd.none

let private defaultAuthState authUser currentPage (unauthState:UnauthState option) state =
    let currentPage = match currentPage with | Some currentPage -> currentPage | None -> AuthPage ChatPage
    // Note: No actual need to call Chat.State.initialize here as will be initialized on demand - i.e. by ShowPage (AuthPage ChatPage) - but no harm in being pre-emptive.
    let chatState, chatCmd = Chat.State.initialize authUser (currentPage = AuthPage ChatPage)
    let authState = {
        AuthUser = authUser
        CurrentPage = currentPage
        UnauthPageStates = match unauthState with | Some unauthState -> unauthState.UnauthPageStates | None -> { NewsState = () ; SquadsState = () }
        AuthPageStates = { DraftsState = () ; ChatState = Some chatState }
        SigningOut = false }
    { state with AppState = Auth authState }, chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)

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
        addDebugError (sprintf "WsOnError when Connecting -> %s" wsApiUrl) (Some "Unable to create a connection to the web server<br><br>Please try again later") uiState
    | WsOnError wsApiUrl, _ -> addDebugError (sprintf "WsOnError not when Connecting -> %s" wsApiUrl) (Some "An unexpected error has occurred") state
    | SendWsNotOpenError uiWsApi, _ -> addDebugError (sprintf "SendWsNotOpenError -> %A" uiWsApi) (Some "The connection to the web server has been closed<br><br>Please try refreshing the page") state
    | SendWsOtherError (uiWsApi, errorText), _ -> addDebugError (sprintf "SendWsOtherError -> %s -> %A" errorText uiWsApi) (Some "Unable to send a message") state
    | DeserializeServerWsApiError errorText, _ -> addDebugError (sprintf "DeserializeServerWsApiError -> %s" errorText) (Some "Unable to process a received message") state

let private handleServerWsError serverWsError state =
    match serverWsError with
    | ReceiveError errorText -> addDebugError (sprintf "Server ReceiveError -> %s" errorText) (Some "The web server was unable to receive a message") state
    | DeserializeUiWsApiError errorText -> addDebugError (sprintf "Server DeserializeUiWsApiError -> %s" errorText) (Some"The web server was unable to process a message") state

let private handleConnected (otherConnections, signedIn) jwt lastPage state =
    let toastCmd =
#if DEBUG
        // TEMP-NMB: Show [ other-web-socket-connection | signed-in-user ] counts (as toast)...
        let otherConnections = if otherConnections > 0 then sprintf "<strong>%i</strong>" otherConnections else sprintf "%i" otherConnections
        let signedIn = if signedIn > 0 then sprintf "<strong>%i</strong>" signedIn else sprintf "%i" signedIn
        infoToastCmd (sprintf "Other web socket connections: %s<br>Signed-in users: %s" otherConnections signedIn)
        // ...or not...
        //Cmd.none
        // ...NMB-TEMP
#else
        Cmd.none
#endif
    let state, cmd =
        match jwt with
        | Some jwt -> { state with AppState = AutomaticallySigningIn (jwt, lastPage) }, AutoSignInWs jwt |> sendUiUnauthWsApiCmd state.Ws
        | None ->
            let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> Some unauthPage | Some (AuthPage _) | None -> None
            let showPageCmd = match lastPage with | Some lastPage -> ShowUnauthPage lastPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
            // TEMP-NMB: ShowSignInModal once connected...
            let showSignInCmd =
                ShowSignInModal |> UnauthInput |> AppInput |> Cmd.ofMsg
            // ...or not...
                //Cmd.none
            // ...NMB-TEMP
            let state, cmd = defaultUnauthState None None state
            state, Cmd.batch [ cmd ; showPageCmd ; showSignInCmd ]
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult result unauthState state =
    match unauthState.SignInState, result with
    | Some _, Ok authUser ->
        let currentPage = Some (UnauthPage unauthState.CurrentUnauthPage)
        let state, cmd = defaultAuthState authUser currentPage (Some unauthState) state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd (sprintf "You have signed in as <strong>%s</strong>" authUser.UserName) ]
    | Some signInState, Error errorText ->
        let toastCmd = errorToastCmd (sprintf "Unable to sign in as <strong>%s</strong>" signInState.UserNameText)
        let errorText =
#if DEBUG
            sprintf "SignInResultWs error -> %s" errorText
#else
            "An unexpected error occurred when signing in"
#endif        
        let signInState = { signInState with SignInStatus = Some (Failed errorText) }
        { state with AppState = Unauth { unauthState with SignInState = Some signInState } }, toastCmd
    | None, _ -> shouldNeverHappen (sprintf "Unexpected SignInResultWs when SignInState is None -> %A" result) state

let private handleAutoSignInResult result (jwt:AuthUser) lastPage state =
    match result with
    | Ok authUser -> // TODO-NMB-LOW: Check authUser vs. _jwt?...
        let showPageCmd = match lastPage with | Some lastPage -> ShowPage lastPage |> AuthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultAuthState authUser None None state
        state, Cmd.batch [ showPageCmd ; cmd ; successToastCmd (sprintf "You have been automatically signed in as <strong>%s</strong>" authUser.UserName) ]
    | Error errorText ->
        let toastCmd = errorToastCmd (sprintf "Unable to automatically sign in as <strong>%s</strong>" jwt.UserName)
        let errorText =
#if DEBUG
            sprintf "AutoSignInResultWs error -> %s" errorText
#else
            "An unexpected error occurred when automatically signing in"
#endif        
        let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> Some unauthPage | Some (AuthPage _) | None -> None
        let signInState = defaultSignInState (Some jwt.UserName) (Some (Failed errorText))
        let showPageCmd = match lastPage with | Some lastPage -> ShowUnauthPage lastPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultUnauthState None (Some signInState) state
        state, Cmd.batch [ cmd ; showPageCmd ; toastCmd ]

let private handleSignOutResult result authState state =
    let toastCmd = successToastCmd "You have signed out"
    match authState.SigningOut, result with
    | true, Ok _sessionId -> // TODO-NMB-LOW: Check _sessionId vs. authState.AuthUser.SessionId?...
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> Some unauthPage | _ -> None
        let state, cmd = defaultUnauthState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; toastCmd ]
    | true, Error errorText ->
        let state, _ =
#if DEBUG
            addDebugError (sprintf "SignOutResultWs error -> %s" errorText) None state
#else
            addError "An unexpected error occurred when signing out" state, Cmd.none
#endif        
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> Some unauthPage | _ -> None
        let state, cmd = defaultUnauthState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; toastCmd ]
    | false, _ -> shouldNeverHappen (sprintf "Unexpected SignOutResultWs when not SigningOut -> %A" result) state

let private handleAutoSignOut _sessionId (authState:AuthState) state = // TODO-NMB-LOW: Check _sessionId vs. authState.AuthUser.SessionId?...
    let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> Some unauthPage | _ -> None
    let state, cmd = defaultUnauthState currentPage None state
    state, Cmd.batch [ cmd ; writePreferencesCmd state ; warningToastCmd "You have been automatically signed out" ]

let private handleServerAppWsApi serverAppWsApi state =
    match serverAppWsApi, state.AppState with
    | ServerWsErrorWs serverWsError, _ -> handleServerWsError serverWsError state
    | ConnectedWs (otherConnections, signedIn), Connecting (jwt, lastPage) -> handleConnected (otherConnections, signedIn) jwt lastPage state
    | SignInResultWs result, Unauth unauthState -> handleSignInResult result unauthState state
    | AutoSignInResultWs result, AutomaticallySigningIn (Jwt jwt, lastPage) -> handleAutoSignInResult result jwt lastPage state
    | SignOutResultWs result, Auth authState -> handleSignOutResult result authState state
    | AutoSignOutWs sessionId, Auth authState -> handleAutoSignOut sessionId authState state
    | OtherUserSignedIn userName, Auth _ -> state, infoToastCmd (sprintf "<strong>%s</strong> has signed in" userName)
    | OtherUserSignedOut userName, Auth _ -> state, infoToastCmd (sprintf "<strong>%s</strong> has signed out" userName)
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerAppWsApi when %s -> %A" (appStateText appState) serverAppWsApi) state

let private handleServerWsApi serverWsApi state =
    match serverWsApi, state.AppState with
    | ServerAppWsApi serverAppWsApi, _ -> handleServerAppWsApi serverAppWsApi state
    | ServerChatWsApi serverChatWsApi, Auth _ -> state, ReceiveServerChatWsApi serverChatWsApi |> ChatInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerWsApi when %s -> %A" (appStateText appState) serverWsApi) state

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = Connecting (preferences.Jwt, preferences.LastPage) }, Cmd.ofSub initializeWsSub
    | Ok None -> { state with AppState = Connecting (None, None) }, Cmd.ofSub initializeWsSub
    | Error exn ->
        let state, _ = addDebugError (sprintf "ReadPreferencesResult -> %s" exn.Message) None state // note: no need for toast
        state, ReadingPreferencesInput (Ok None) |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with Ws = Some ws }, Cmd.none

let private handleUnauthInput unauthInput unauthState state =
    match unauthInput, unauthState.SignInState with
    | ShowUnauthPage unauthPage, _ ->
        if unauthState.CurrentUnauthPage <> unauthPage then
            // TODO-NMB-MEDIUM: Initialize "optional" pages (if required) and toggle "IsCurrent" for relevant pages...
            let unauthState = { unauthState with CurrentUnauthPage = unauthPage }
            let state = { state with AppState = Unauth unauthState }
            state, writePreferencesCmd state
        else state, Cmd.none
    | UnauthPageInput NewsInput, None -> shouldNeverHappen "Unexpected NewsInput -> NYI" state
    | UnauthPageInput SquadsInput, None -> shouldNeverHappen "Unexpected SquadsInput -> NYI" state
    | ShowSignInModal, None ->
        let unauthState = { unauthState with SignInState = Some (defaultSignInState None None) }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (UserNameTextChanged userNameText), Some signInState ->
        let signInState = { signInState with UserNameText = userNameText ; UserNameErrorText = validateUserNameText userNameText }
        let unauthState = { unauthState with SignInState = Some signInState }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (PasswordTextChanged passwordText), Some signInState ->
        let signInState = { signInState with PasswordText = passwordText ; PasswordErrorText = validatePasswordText passwordText }
        let unauthState = { unauthState with SignInState = Some signInState }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput SignIn, Some signInState -> // note: assume no need to validate unauthState.UserNameText or unauthState.PasswordText (i.e. because App.Render.renderUnauth will ensure that SignIn can only be dispatched when valid)
        let signInState = { signInState with SignInStatus = Some Pending }
        let unauthState = { unauthState with SignInState = Some signInState }
        let cmd = SignInWs (state.SessionId, signInState.UserNameText, signInState.PasswordText) |> sendUiUnauthWsApiCmd state.Ws
        { state with AppState = Unauth unauthState }, cmd
    | SignInInput CancelSignIn, Some _ ->
        let unauthState = { unauthState with SignInState = None }
        { state with AppState = Unauth unauthState }, Cmd.none
    | _, _ -> shouldNeverHappen (sprintf "Unexpected UnauthInput when SignIsState is %A -> %A" unauthState.SignInState unauthInput) state

let private handleAuthInput authInput authState state =
    match authInput, authState.SigningOut with
    | ShowPage page, false ->
        if authState.CurrentPage <> page then
            let chatState, chatCmd =
                match page, authState.AuthPageStates.ChatState with
                | AuthPage ChatPage, None ->
                    let chatState, chatCmd = Chat.State.initialize authState.AuthUser true
                    Some chatState, chatCmd
                | _, Some chatState -> Some chatState, ToggleChatIsCurrentPage (page = AuthPage ChatPage) |> Cmd.ofMsg
                | _, None -> None, Cmd.none
            // TODO-NMB-MEDIUM: Initialize other "optional" pages (if required) and toggle "IsCurrent" for other relevant pages...
            let authPageStates = { authState.AuthPageStates with ChatState = chatState }
            let authState = { authState with CurrentPage = page ; AuthPageStates = authPageStates }
            let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
            let state = { state with AppState = Auth authState }
            state, Cmd.batch [ chatCmd ; writePreferencesCmd state ]
        else state, Cmd.none
    | PageInput (UPageInput NewsInput), false -> shouldNeverHappen "Unexpected NewsInput -> NYI" state
    | PageInput (UPageInput SquadsInput), false -> shouldNeverHappen "Unexpected SquadsInput -> NYI" state
    | PageInput (APageInput DraftsInput), false -> shouldNeverHappen "Unexpected DraftsInput -> NYI" state
    | PageInput (APageInput (ChatInput ShowMarkdownSyntaxModal)), false -> { state with StaticModal = Some MarkdownSyntax }, Cmd.none
    | PageInput (APageInput (ChatInput (SendAuthWsApi (_authUser, uiAuthWsApi)))), false -> // TODO-NMB-LOW: Check _authUser vs. authState.AuthUser?...
        state, UiAuthWsApi (Jwt authState.AuthUser, uiAuthWsApi) |> sendUiWsApiCmd state.Ws
    | PageInput (APageInput (ChatInput chatInput)), false ->
        match authState.AuthPageStates.ChatState with
        | Some chatState ->
            let chatState, chatCmd = Chat.State.transition chatInput chatState
            let authPageStates = { authState.AuthPageStates with ChatState = Some chatState }
            { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
        | None -> shouldNeverHappen "Unexpected ChatInput when ChatState is None" state
    | ChangePassword, false -> state, warningToastCmd "Change password functionality is coming soon"
    | SignOut, false ->
        let cmd = UiAuthWsApi (Jwt authState.AuthUser, SignOutWs) |> sendUiWsApiCmd state.Ws
        { state with AppState = Auth { authState with SigningOut = true } }, cmd
    | UserAdministration, false -> // TODO-NMB-LOW: Check that authState.AuthUser has appropriate permissions?...
        state, warningToastCmd "User administration functionality is coming soon"
    | _, true -> shouldNeverHappen (sprintf "Unexpected AuthInput when SigningOut -> %A" authInput) state

let private handleAppInput appInput state =
    match appInput, state.AppState with
    | ReadingPreferencesInput result, ReadingPreferences -> handleReadingPreferencesInput result state
    | ConnectingInput ws, Connecting _ -> handleConnectingInput ws state
    | UnauthInput unauthInput, Unauth unauthState -> handleUnauthInput unauthInput unauthState state
    | AuthInput authInput, Auth authState -> handleAuthInput authInput authState state
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
    | WritePreferencesResult (Error exn) -> addDebugError (sprintf "WritePreferencesResult -> %s" exn.Message) None state // note: no need for toast
    | OnUiWsError uiWsError -> handleUiWsError uiWsError state
    | HandleServerWsApi serverWsApi -> handleServerWsApi serverWsApi state
    | AppInput appInput -> handleAppInput appInput state
