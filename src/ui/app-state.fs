module Aornota.Sweepstake2018.UI.App.State

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Literals
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.App.Common
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Pages.Chat.Common
open Aornota.Sweepstake2018.UI.Shared

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Common.LocalStorage
open Aornota.UI.Theme.Common

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

let private writePreferencesCmd uiState =
    let writePreferences uiState = async {
        let jwt = match uiState.AppState with | Authenticated authenticatedState -> Some (Jwt (authenticatedState.AuthenticatedUser)) | _ -> None
        let preferences = { UseDefaultTheme = uiState.UseDefaultTheme ; SessionId = uiState.SessionId ; Jwt = jwt }
        do writeJson APP_PREFERENCES_KEY (toJson preferences) }
    Cmd.ofAsync writePreferences uiState (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private initializeWsSub uiDispatch =
    let receiveServerWsApi (wsMessage:Brw.MessageEvent) : unit =
        try // note: Expect wsMessage.data to be deserializable to ServerWsApi
            let serverWsApi = ofJson<ServerWsApi> <| unbox wsMessage.data
#if DEBUG
            if random.NextDouble () < 0.02 then failwith (sprintf "Fake error deserializing %A" serverWsApi)
#endif
            HandleServerWsApi serverWsApi |> uiDispatch
        with exn -> OnUiWsError (DeserializeServerWsApiError exn.Message) |> uiDispatch
    let wsUrl =
#if DEBUG
        sprintf "ws://localhost:%i" WS_PORT
#else       
        sprintf "ws://localhost:%i" WS_PORT // TODO-NMB-MEDIUM: Confirm "production" wsUrl (e.g. Azure app service? use "wss://..."?)...
#endif
    let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
    try
        let ws = Brw.WebSocket.Create wsApiUrl
        ws.onopen <- (fun _ -> ConnectingInput ws |> AppInput |> uiDispatch)
        ws.onerror <- (fun _ -> OnUiWsError (WsOnError wsApiUrl) |> uiDispatch)
        ws.onmessage <- receiveServerWsApi
        ()
    with _ -> OnUiWsError (WsOnError wsApiUrl) |> uiDispatch

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

let private addDebugMessage debugText uiState = { uiState with DebugMessages = debugMessage debugText :: uiState.DebugMessages }

let private shouldNeverHappenText text = sprintf "SHOULD NEVER HAPPEN -> %s" text

let private error debugText toastText uiState : UiState * Cmd<UiInput> =
    addDebugMessage (sprintf "ERROR -> %s" debugText) uiState, match toastText with | Some toastText -> errorToastCmd toastText | None -> Cmd.none

let private shouldNeverHappen debugText uiState : UiState * Cmd<UiInput> = addDebugMessage (shouldNeverHappenText debugText) uiState, Cmd.none

let private appStateText appState =
    match appState with
    | ReadingPreferences -> "ReadingPreferences" | Connecting _ -> "Connecting" | ServiceUnavailable -> "ServiceUnavailable" | AutomaticallySigningIn _ -> "AutomaticallySigningIn"
    | Authenticated _ -> "Authenticated" | Unauthenticated _ -> "Unauthenticated"

let private defaultUnauthenticatedState (userName:string option) uiState =
    let sendUiUnauthenticatedWsApi, debugMessage =
        match uiState.Ws with
        | Some ws -> UiUnauthenticatedWsApi >> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenText "defaultUnauthenticatedState called when uiState.Ws is None")
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
    unauthenticatedState, match debugMessage with | Some debugMessage -> AddDebugMessageUi debugMessage |> Cmd.ofMsg | None -> Cmd.none

let private defaultUnauthenticatedUIState (userName:string option) uiState =
    let unauthenticatedState, cmd = defaultUnauthenticatedState userName uiState
    { uiState with AppState = Unauthenticated unauthenticatedState }, cmd

let private defaultAuthenticatedUiState authenticatedUser uiState =
    let sendUiWsApi, debugMessage =
        match uiState.Ws with
        | Some ws -> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenText "defaultAuthenticatedUiState called when uiState.Ws is None")
    let chatState, chatCmd = Chat.State.initialize authenticatedUser
    let authenticatedState = {
        SendUiWsApi = sendUiWsApi
        AuthenticatedUser = authenticatedUser
        Page = ChatPage
        ChatState = chatState
        SignOutStatus = None }
    let uiState = { uiState with AppState = Authenticated authenticatedState }
    let uiState = match debugMessage with | Some debugMessage -> addDebugMessage debugMessage uiState | None -> uiState
    uiState, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)

let initialize () =
    let uiState = {
        DebugMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
        Ws = None
        AppState = ReadingPreferences }
    setBodyClass uiState.UseDefaultTheme
    uiState, readPreferencesCmd

let private handleUiWsError uiWsError uiState : UiState * Cmd<UiInput> =
    match uiWsError, uiState.AppState with
    | WsOnError wsApiUrl, Connecting _ ->
        let uiState = { uiState with AppState = ServiceUnavailable }
        error (sprintf "WsOnError when Connecting -> %s" wsApiUrl) (Some "Unable to create a web socket connection to the web server") uiState
    | WsOnError wsApiUrl, _ -> error (sprintf "WsOnError not when Connecting -> %s" wsApiUrl) (Some "An unexpected web socket error has occurred") uiState
    | SendWsNotOpenError uiWsApi, _ -> error (sprintf "SendWsNotOpenError -> %A" uiWsApi) (Some "The web socket connection to the web server has been closed") uiState
    | SendWsOtherError (uiWsApi, errorText), _ -> error (sprintf "SendWsOtherError -> %s -> %A" errorText uiWsApi) (Some "Unable to send a web socket message") uiState
    | DeserializeServerWsApiError errorText, _ -> error (sprintf "DeserializeServerWsApiError -> %s" errorText) (Some "Unable to process a received web socket message") uiState

let private handleServerWsError serverWsError uiState =
    match serverWsError with
    | ReceiveError errorText -> error (sprintf "Server ReceiveError -> %s" errorText) (Some "The web server was unable to receive a web socket message") uiState
    | DeserializeUiWsApiError errorText -> error (sprintf "Server DeserializeUiWsApiError -> %s" errorText) (Some"The web server was unable to process a web socket message") uiState

let private handleConnected (otherConnections, signedIn) jwt uiState =
    let toastCmd =
#if DEBUG
        let plural i = if i = 1 then String.Empty else "s"
        infoToastCmd (sprintf "%i other web socket connection%s | %i user%s signed-in" otherConnections (plural otherConnections) signedIn (plural signedIn))
#else
        Cmd.none
#endif
    let uiState, cmd =
        match jwt with
        | Some jwt ->
            let unauthenticatedState, cmd = defaultUnauthenticatedState None uiState
            let autoCmd = AutoSignInWs jwt |> unauthenticatedState.SendUiUnauthenticatedWsApi
            { uiState with AppState = AutomaticallySigningIn jwt }, Cmd.batch [ cmd ; autoCmd ]
        | None -> defaultUnauthenticatedUIState None uiState       
    uiState, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult result unauthenticatedState uiState =
    match result with
    | Ok authenticatedUser ->
        let uiState, cmd = defaultAuthenticatedUiState authenticatedUser uiState
        uiState, Cmd.batch [ cmd ; writePreferencesCmd uiState ; successToastCmd "You have signed in" ]
    | Error errorText ->
        let uiState, toastCmd = error (sprintf "SignInResultWs -> %s" errorText) (Some (sprintf "Unable to sign in as %s" unauthenticatedState.UserNameText)) uiState // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { uiState with AppState = Unauthenticated { unauthenticatedState with SignInStatus = Some (Failed errorText) } }, toastCmd

let private handleAutoSignInResult result (jwt:AuthenticatedUser) uiState =
    match result with
    | Ok authenticatedUser -> // TODO-NMB-LOW: Check authenticatedUser vs. _jwt?...
        let uiState, cmd = defaultAuthenticatedUiState authenticatedUser uiState
        uiState, Cmd.batch [ cmd ; successToastCmd "You have been automatically signed in" ]
    | Error errorText ->
        let unauthenticatedState, cmd = defaultUnauthenticatedState (Some jwt.UserName) uiState
        let uiState, toastCmd = error (sprintf "AutoSignInResultWs -> %s" errorText) (Some (sprintf "Unable to automatically sign in as %s" jwt.UserName)) uiState // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { uiState with AppState = Unauthenticated { unauthenticatedState with SignInStatus = Some (Failed errorText) } }, Cmd.batch [ cmd ; toastCmd ]

let private handleSignOutResult result (authenticatedState:AuthenticatedState) uiState =
    match result with
    | Ok _sessionId -> // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
        let uiState, cmd = defaultUnauthenticatedUIState (Some authenticatedState.AuthenticatedUser.UserName) uiState
        uiState, Cmd.batch [ cmd ; writePreferencesCmd uiState ; successToastCmd "You have signed out" ]
    | Error errorText ->
        let uiState, toastCmd = error (sprintf "SignOutResultWs -> %s" errorText) (Some "Unable to sign out") uiState // TODO-NMB-MEDIUM: Is this necessary, e.g. if errorText rendered elsewhere?...
        { uiState with AppState = Authenticated { authenticatedState with SignOutStatus = Some (Failed errorText) } }, toastCmd

let private handleAutoSignOut _sessionId (authenticatedState:AuthenticatedState) uiState = // TODO-NMB-LOW: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
    let uiState, cmd = defaultUnauthenticatedUIState (Some authenticatedState.AuthenticatedUser.UserName) uiState
    uiState, Cmd.batch [ cmd ; warningToastCmd "You have been automatically signed out" ]

let private handleServerAppWsApi serverAppWsApi uiState =
    match serverAppWsApi, uiState.AppState with
    | ServerWsErrorWs serverWsError, _ -> handleServerWsError serverWsError uiState
    | ConnectedWs (otherConnections, signedIn), Connecting jwt -> handleConnected (otherConnections, signedIn) jwt uiState
    | SignInResultWs result, Unauthenticated unauthenticatedState -> handleSignInResult result unauthenticatedState uiState
    | AutoSignInResultWs result, AutomaticallySigningIn (Jwt jwt) -> handleAutoSignInResult result jwt uiState
    | SignOutResultWs result, Authenticated authenticatedState -> handleSignOutResult result authenticatedState uiState
    | AutoSignOutWs sessionId, Authenticated authenticatedState -> handleAutoSignOut sessionId authenticatedState uiState
    | OtherUserSignedIn userName, Authenticated _ -> uiState, infoToastCmd (sprintf "%s has signed in" userName)
    | OtherUserSignedOut userName, Authenticated _ -> uiState, infoToastCmd (sprintf "%s has signed out" userName)
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerAppWsApi when %s -> %A" (appStateText appState) serverAppWsApi) uiState

let private handleServerWsApi serverWsApi uiState =
    match serverWsApi, uiState.AppState with
    | ServerAppWsApi serverAppWsApi, _ -> handleServerAppWsApi serverAppWsApi uiState
    | ServerChatWsApi serverChatWsApi, Authenticated _ ->
        uiState, ReceiveServerWsApi (ServerChatWsApi serverChatWsApi) |> SharedInput |> ChatInput |> AuthenticatedInput |> AppInput |> Cmd.ofMsg
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerWsApi when %s -> %A" (appStateText appState) serverWsApi) uiState

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (uiState:UiState) =
    match result with
    | Ok (Some preferences) ->
        let uiState = { uiState with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass uiState.UseDefaultTheme
        { uiState with AppState = Connecting preferences.Jwt }, Cmd.ofSub initializeWsSub
    | Ok None -> { uiState with AppState = Connecting None }, Cmd.ofSub initializeWsSub
    | Error exn ->
        let uiState, _ = error (sprintf "ReadPreferencesResult -> %s" exn.Message) None uiState
        uiState, ReadingPreferencesInput (Ok None) |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws uiState : UiState * Cmd<UiInput> = { uiState with Ws = Some ws }, Cmd.none

let private handleUnauthenticatedInput unauthenticatedInput unauthenticatedState uiState =
    match unauthenticatedInput with
    | UserNameTextChanged userNameText ->
        let unauthenticatedState = { unauthenticatedState with UserNameText = userNameText ; UserNameErrorText = validateUserNameText userNameText }
        { uiState with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | PasswordTextChanged passwordText ->
        let unauthenticatedState = { unauthenticatedState with PasswordText = passwordText ; PasswordErrorText = validatePasswordText passwordText }
        { uiState with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | SignIn -> // note: assume no need to validate unauthenticatedState.UserNameText or unauthenticatedState.PasswordText (i.e. because App.Render.renderUnauthenticated will ensure that SignIn can only be dispatched when valid)
        let unauthenticatedState = { unauthenticatedState with SignInStatus = Some Pending }
        let cmd = SignInWs (uiState.SessionId, unauthenticatedState.UserNameText, unauthenticatedState.PasswordText) |> unauthenticatedState.SendUiUnauthenticatedWsApi
        { uiState with AppState = Unauthenticated unauthenticatedState }, cmd

let private handleAuthenticatedInput authenticatedInput authenticatedState uiState =
    match authenticatedInput with
    | ChatInput (SharedInput (AddDebugMessage debugMessage)) -> addDebugMessage debugMessage uiState, Cmd.none
    | ChatInput (SharedInput (SendUnauthenticatedWsApi uiUnauthenticatedWsApi)) -> uiState, UiUnauthenticatedWsApi uiUnauthenticatedWsApi |> authenticatedState.SendUiWsApi
    | ChatInput (SharedInput (SendAuthenticatedWsApi (_authenticatedUser, uiAuthenticatedWsApi))) -> // TODO-NMB-LOW: Check _authenticatedUser vs. authenticatedState.AuthenticatedUser?...
        uiState, UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, uiAuthenticatedWsApi) |> authenticatedState.SendUiWsApi
    | ChatInput chatInput ->
        let chatState, chatCmd = Chat.State.transition chatInput authenticatedState.ChatState
        { uiState with AppState = Authenticated { authenticatedState with ChatState = chatState } }, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)
    | SignOut ->
        let cmd = UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, SignOutWs) |> authenticatedState.SendUiWsApi
        { uiState with AppState = Authenticated { authenticatedState with SignOutStatus = Some Pending } }, cmd

let private handleAppInput appInput uiState =
    match appInput, uiState.AppState with
    | ReadingPreferencesInput result, ReadingPreferences -> handleReadingPreferencesInput result uiState
    | ConnectingInput ws, Connecting _ -> handleConnectingInput ws uiState
    | UnauthenticatedInput unauthenticatedInput, Unauthenticated unauthenticatedState -> handleUnauthenticatedInput unauthenticatedInput unauthenticatedState uiState
    | AuthenticatedInput authenticatedInput, Authenticated authenticatedState -> handleAuthenticatedInput authenticatedInput authenticatedState uiState
    | _, appState -> shouldNeverHappen (sprintf "Unexpected AppInput when %s -> %A" (appStateText appState) appInput) uiState

let transition uiInput uiState =
    match uiInput with
    | AddDebugMessageUi message -> addDebugMessage message uiState, Cmd.none
    | DismissDebugMessage debugId -> { uiState with DebugMessages = uiState.DebugMessages |> removeDebugMessage debugId }, Cmd.none // note: silently ignore unknown debugId
    | ToggleTheme ->
        let uiState = { uiState with UseDefaultTheme = not uiState.UseDefaultTheme }
        setBodyClass uiState.UseDefaultTheme
        uiState, writePreferencesCmd uiState
    | ToggleNavbarBurger -> { uiState with NavbarBurgerIsActive = not uiState.NavbarBurgerIsActive }, Cmd.none
    | WritePreferencesResult (Ok _) -> uiState, Cmd.none
    | WritePreferencesResult (Error exn) -> error (sprintf "WritePreferencesResult -> %s" exn.Message) None uiState // note: no need for toast
    | OnUiWsError uiWsError -> handleUiWsError uiWsError uiState
    | HandleServerWsApi serverWsApi -> handleServerWsApi serverWsApi uiState
    | AppInput appInput -> handleAppInput appInput uiState
