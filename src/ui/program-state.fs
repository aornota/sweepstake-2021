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
        let jwt = match uiState.AppState with | Authenticated authenticatedState -> Some (Jwt (authenticatedState.AuthenticatedUser)) | _ -> None
        let preferences = { UseDefaultTheme = uiState.UseDefaultTheme ; SessionId = uiState.SessionId ; Jwt = jwt }
        do writeJson APP_PREFERENCES_KEY (toJson preferences) }
    Cmd.ofAsync writePreferences state (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private initializeWsSub dispatch =
    let receiveServerWsApi (wsMessage:Brw.MessageEvent) : unit =
        try // note: Expect wsMessage.data to be deserializable to ServerWsApi
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

let private sendUiWsApiCmd (ws:Brw.WebSocket) (uiWsApi:UiWsApi) =
    if ws.readyState <> ws.OPEN then OnUiWsError (SendWsNotOpenError uiWsApi) |> Cmd.ofMsg
    else
        try
#if DEBUG
            if random.NextDouble () < 0.02 then failwith "Fake sendUiWsApiCmd error"
#endif
            ws.send (toJson uiWsApi)
            Cmd.none
        with exn -> OnUiWsError (SendWsOtherError (uiWsApi, exn.Message)) |> Cmd.ofMsg

let private addNotificationMessage notificationMessage state = { state with NotificationMessages = notificationMessage :: state.NotificationMessages }

let private addDebugMessage debugText state = addNotificationMessage (debugDismissableMessage debugText) state
let private addInfoMessage infoText state = addNotificationMessage (infoDismissableMessage infoText) state
let private addWarningMessage warningText state = addNotificationMessage (warningDismissableMessage warningText) state
let private addDangerMessage dangerText state = addNotificationMessage (dangerDismissableMessage dangerText) state

let private shouldNeverHappenText text = sprintf "SHOULD NEVER HAPPEN -> %s" text
let private shouldNeverHappen debugText state : State * Cmd<Input> = addDebugMessage (shouldNeverHappenText debugText) state, Cmd.none

let private error debugText toastText state : State * Cmd<Input> =
    addDebugMessage (sprintf "ERROR -> %s" debugText) state, match toastText with | Some toastText -> errorToastCmd toastText | None -> Cmd.none

let private appStateText appState =
    match appState with
    | ReadingPreferences -> "ReadingPreferences" | Connecting _ -> "Connecting" | ServiceUnavailable -> "ServiceUnavailable" | AutomaticallySigningIn _ -> "AutomaticallySigningIn"
    | Authenticated _ -> "Authenticated" | Unauthenticated _ -> "Unauthenticated"

let private defaultUnauthenticatedState (userName:string option) state =
    let sendUiUnauthenticatedWsApi, debugText =
        match state.Ws with
        | Some ws -> UiUnauthenticatedWsApi >> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenText "defaultUnauthenticatedState called when state.Ws is None")
    let unauthenticatedState = {
        SendUiUnauthenticatedWsApi = sendUiUnauthenticatedWsApi
        UserNameKey = Guid.NewGuid ()
        UserNameText = match userName with | Some userName -> userName | None -> String.Empty
        UserNameErrorText = None
        PasswordKey = Guid.NewGuid ()
        PasswordText = String.Empty
        PasswordErrorText = None
        FocusPassword = match userName with | Some _ -> true | None -> false
        SignInStatus = None }
    unauthenticatedState, match debugText with | Some debugText -> AddNotificationMessage (debugDismissableMessage debugText) |> Cmd.ofMsg | None -> Cmd.none

let private defaultUnauthenticatedUIState (userName:string option) state =
    let unauthenticatedState, cmd = defaultUnauthenticatedState userName state
    { state with AppState = Unauthenticated unauthenticatedState }, cmd

let private defaultAuthenticatedUiState authenticatedUser state =
    let sendUiWsApi, debugText =
        match state.Ws with
        | Some ws -> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenText "defaultAuthenticatedUiState called when state.Ws is None")
    let chatState, chatCmd = Chat.State.initialize authenticatedUser
    let authenticatedState = {
        SendUiWsApi = sendUiWsApi
        AuthenticatedUser = authenticatedUser
        Page = ChatPage
        ChatState = chatState
        SignOutStatus = None }
    let state = { state with AppState = Authenticated authenticatedState }
    let state = match debugText with | Some debugText -> addDebugMessage debugText state | None -> state
    state, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)

let initialize () =
    let state = {
        Ticks = 0<tick>
        NotificationMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
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

let private handleConnected (otherConnections, signedIn) jwt state =
    let toastCmd =
#if DEBUG
        let plural i = if i = 1 then String.Empty else "s"
        infoToastCmd (sprintf "%i other web socket connection%s | %i user%s signed-in" otherConnections (plural otherConnections) signedIn (plural signedIn))
#else
        Cmd.none
#endif
    let state, cmd =
        match jwt with
        | Some jwt ->
            let unauthenticatedState, cmd = defaultUnauthenticatedState None state
            let autoCmd = AutoSignInWs jwt |> unauthenticatedState.SendUiUnauthenticatedWsApi
            { state with AppState = AutomaticallySigningIn jwt }, Cmd.batch [ cmd ; autoCmd ]
        | None -> defaultUnauthenticatedUIState None state       
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult result unauthenticatedState state =
    match result with
    | Ok authenticatedUser ->
        let state, cmd = defaultAuthenticatedUiState authenticatedUser state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd "You have signed in" ]
    | Error errorText ->
        let state, toastCmd = error (sprintf "SignInResultWs -> %s" errorText) (Some (sprintf "Unable to sign in as %s" unauthenticatedState.UserNameText)) state // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Unauthenticated { unauthenticatedState with SignInStatus = Some (Failed errorText) } }, toastCmd

let private handleAutoSignInResult result (jwt:AuthenticatedUser) state =
    match result with
    | Ok authenticatedUser -> // TODO-NMB-LOW: Check authenticatedUser vs. _jwt?...
        let state, cmd = defaultAuthenticatedUiState authenticatedUser state
        state, Cmd.batch [ cmd ; successToastCmd "You have been automatically signed in" ]
    | Error errorText ->
        let unauthenticatedState, cmd = defaultUnauthenticatedState (Some jwt.UserName) state
        let state, toastCmd = error (sprintf "AutoSignInResultWs -> %s" errorText) (Some (sprintf "Unable to automatically sign in as %s" jwt.UserName)) state // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Unauthenticated { unauthenticatedState with SignInStatus = Some (Failed errorText) } }, Cmd.batch [ cmd ; toastCmd ]

let private handleSignOutResult result (authenticatedState:AuthenticatedState) state =
    match result with
    | Ok _sessionId -> // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
        let state, cmd = defaultUnauthenticatedUIState (Some authenticatedState.AuthenticatedUser.UserName) state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; successToastCmd "You have signed out" ]
    | Error errorText ->
        let state, toastCmd = error (sprintf "SignOutResultWs -> %s" errorText) (Some "Unable to sign out") state // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Authenticated { authenticatedState with SignOutStatus = Some (Failed errorText) } }, toastCmd

let private handleAutoSignOut _sessionId (authenticatedState:AuthenticatedState) state = // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
    let state, cmd = defaultUnauthenticatedUIState (Some authenticatedState.AuthenticatedUser.UserName) state
    state, Cmd.batch [ cmd ; warningToastCmd "You have been automatically signed out" ]

let private handleServerAppWsApi serverAppWsApi state =
    match serverAppWsApi, state.AppState with
    | ServerWsErrorWs serverWsError, _ -> handleServerWsError serverWsError state
    | ConnectedWs (otherConnections, signedIn), Connecting jwt -> handleConnected (otherConnections, signedIn) jwt state
    | SignInResultWs result, Unauthenticated unauthenticatedState -> handleSignInResult result unauthenticatedState state
    | AutoSignInResultWs result, AutomaticallySigningIn (Jwt jwt) -> handleAutoSignInResult result jwt state
    | SignOutResultWs result, Authenticated authenticatedState -> handleSignOutResult result authenticatedState state
    | AutoSignOutWs sessionId, Authenticated authenticatedState -> handleAutoSignOut sessionId authenticatedState state
    | OtherUserSignedIn userName, Authenticated _ -> state, infoToastCmd (sprintf "%s has signed in" userName)
    | OtherUserSignedOut userName, Authenticated _ -> state, infoToastCmd (sprintf "%s has signed out" userName)
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerAppWsApi when %s -> %A" (appStateText appState) serverAppWsApi) state

let private handleServerWsApi serverWsApi state =
    match serverWsApi, state.AppState with
    | ServerAppWsApi serverAppWsApi, _ -> handleServerAppWsApi serverAppWsApi state
    | ServerChatWsApi serverChatWsApi, Authenticated _ ->
        state, ReceiveServerWsApi (ServerChatWsApi serverChatWsApi) |> SharedInput |> ChatInput |> AuthenticatedInput |> AppInput |> Cmd.ofMsg
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerWsApi when %s -> %A" (appStateText appState) serverWsApi) state

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = Connecting preferences.Jwt }, Cmd.ofSub initializeWsSub
    | Ok None -> { state with AppState = Connecting None }, Cmd.ofSub initializeWsSub
    | Error exn ->
        let state, _ = error (sprintf "ReadPreferencesResult -> %s" exn.Message) None state
        state, ReadingPreferencesInput (Ok None) |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with Ws = Some ws }, Cmd.none

let private handleUnauthenticatedInput unauthenticatedInput unauthenticatedState state =
    match unauthenticatedInput with
    | UserNameTextChanged userNameText ->
        let unauthenticatedState = { unauthenticatedState with UserNameText = userNameText ; UserNameErrorText = validateUserNameText userNameText }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | PasswordTextChanged passwordText ->
        let unauthenticatedState = { unauthenticatedState with PasswordText = passwordText ; PasswordErrorText = validatePasswordText passwordText }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | SignIn -> // note: assume no need to validate unauthenticatedState.UserNameText or unauthenticatedState.PasswordText (i.e. because App.Render.renderUnauthenticated will ensure that SignIn can only be dispatched when valid)
        let unauthenticatedState = { unauthenticatedState with SignInStatus = Some Pending }
        let cmd = SignInWs (state.SessionId, unauthenticatedState.UserNameText, unauthenticatedState.PasswordText) |> unauthenticatedState.SendUiUnauthenticatedWsApi
        { state with AppState = Unauthenticated unauthenticatedState }, cmd

let private handleAuthenticatedInput authenticatedInput authenticatedState state =
    match authenticatedInput with
    | ChatInput (SharedInput (SendNotificationMessage notificationMessage)) -> addNotificationMessage notificationMessage state, Cmd.none
    | ChatInput (SharedInput (SendUnauthenticatedWsApi uiUnauthenticatedWsApi)) -> state, UiUnauthenticatedWsApi uiUnauthenticatedWsApi |> authenticatedState.SendUiWsApi
    | ChatInput (SharedInput (SendAuthenticatedWsApi (_authenticatedUser, uiAuthenticatedWsApi))) -> // TODO-NMB-LOW: Check _authenticatedUser vs. authenticatedState.AuthenticatedUser?...
        state, UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, uiAuthenticatedWsApi) |> authenticatedState.SendUiWsApi
    | ChatInput chatInput ->
        let chatState, chatCmd = Chat.State.transition chatInput authenticatedState.ChatState
        { state with AppState = Authenticated { authenticatedState with ChatState = chatState } }, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)
    | SignOut ->
        let cmd = UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, SignOutWs) |> authenticatedState.SendUiWsApi
        { state with AppState = Authenticated { authenticatedState with SignOutStatus = Some Pending } }, cmd

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
    | WritePreferencesResult (Ok _) -> state, Cmd.none
    | WritePreferencesResult (Error exn) -> error (sprintf "WritePreferencesResult -> %s" exn.Message) None state // note: no need for toast
    | OnUiWsError uiWsError -> handleUiWsError uiWsError state
    | HandleServerWsApi serverWsApi -> handleServerWsApi serverWsApi state
    | AppInput appInput -> handleAppInput appInput state
