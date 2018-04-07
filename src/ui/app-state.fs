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
    Cmd.ofAsync readPreferences () (Ok >> ReadPreferencesResult >> ReadingPreferencesInput >> AppInput) (Error >> ReadPreferencesResult >> ReadingPreferencesInput >> AppInput)

let private writePreferencesCmd (useDefaultTheme, sessionId, jwt) =
    let writePreferences (useDefaultTheme, sessionId, jwt) = async {
        let preferences = { UseDefaultTheme = useDefaultTheme ; SessionId = sessionId ; Jwt = jwt }
        do writeJson APP_PREFERENCES_KEY (toJson preferences) }
    Cmd.ofAsync writePreferences (useDefaultTheme, sessionId, jwt) (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

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
        // TODO-NMB: Confirm "production" wsUrl (e.g. Azure app service?)...
        sprintf "wss://localhost:%i" WS_PORT
#endif
    let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
    try
        let ws = Brw.WebSocket.Create wsApiUrl
        ws.onopen <- (fun _ -> WsOnOpen ws |> ConnectingInput |> AppInput |> dispatch)
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

let private addDebugMessage message state = { state with DebugMessages = debugMessage message :: state.DebugMessages }

let private errorMessage message = sprintf "ERROR -> %s" message
let private shouldNeverHappenMessage message = sprintf "SHOULD NEVER HAPPEN -> %s" message

let private defaultUnauthenticatedStateOnly (userName:string option) state =
    let sendUiUnauthenticatedWsApi, debugMessage =
        match state.Ws with
        | Some ws -> UiUnauthenticatedWsApi >> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenMessage "defaultUnauthenticatedStateOnly called when state.Ws is None")
    let unauthenticatedState = {
        SendUiUnauthenticatedWsApi = sendUiUnauthenticatedWsApi
        UserNameKey = Guid.NewGuid ()
        UserNameText = match userName with | Some userName -> userName | None -> String.Empty
        UserNameErrorText = None
        PasswordKey = Guid.NewGuid ()
        PasswordText = String.Empty
        PasswordErrorText = None
        SignInStatus = None }
    let cmd = match debugMessage with | Some debugMessage -> AddDebugMessageApp debugMessage |> Cmd.ofMsg | None -> Cmd.none
    unauthenticatedState, cmd

let private defaultUnauthenticatedState (userName:string option) state =
    let unauthenticatedState, cmd = defaultUnauthenticatedStateOnly userName state
    let state = { state with AppState = Unauthenticated unauthenticatedState }
    state, cmd

let private defaultAuthenticatedState authenticatedUser state =
    let sendUiWsApi, debugMessage =
        match state.Ws with
        | Some ws -> sendUiWsApiCmd ws, None
        | None -> (fun _ -> Cmd.none), Some (shouldNeverHappenMessage "defaultAuthenticatedState called when state.SendUiWsApiCmd is None")
    let chatState, chatCmd = Chat.State.initialize authenticatedUser
    let authenticatedState = {
        SendUiWsApi = sendUiWsApi
        AuthenticatedUser = authenticatedUser
        Page = ChatPage
        ChatState = chatState
        SignOutStatus = None }
    let state = { state with AppState = Authenticated authenticatedState }
    let state = match debugMessage with | Some debugMessage -> addDebugMessage debugMessage state | None -> state
    state, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)

let initialize () =
    let state = {
        DebugMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
        Ws = None
        AppState = ReadingPreferences }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let transition input state =
    let shouldNeverHappen message state = addDebugMessage (shouldNeverHappenMessage message) state
    let writePreferencesCmd state =
        let jwt = match state.AppState with | Authenticated authenticatedState -> Some (Jwt (authenticatedState.AuthenticatedUser)) | _ -> None
        writePreferencesCmd (state.UseDefaultTheme, state.SessionId, jwt)
    let unchanged = state, Cmd.none
    match input, state.AppState with
    | AddDebugMessageApp message, _ ->
        addDebugMessage message state, Cmd.none
    | DismissDebugMessage debugId, _ -> // note: silently ignore unknown debugId
        { state with DebugMessages = state.DebugMessages |> removeDebugMessage debugId }, Cmd.none
    | ToggleTheme, _ ->
        let state = { state with UseDefaultTheme = not state.UseDefaultTheme }
        setBodyClass state.UseDefaultTheme
        state, writePreferencesCmd state
    | ToggleNavbarBurger, _ ->
        { state with NavbarBurgerIsActive = not state.NavbarBurgerIsActive }, Cmd.none
    | WritePreferencesResult (Ok _), _ ->
        unchanged
    | WritePreferencesResult (Error exn), _ -> // note: no need for toast
        addDebugMessage (errorMessage (sprintf "WritePreferencesResult -> %s" exn.Message)) state, Cmd.none
    | OnUiWsError (WsOnError wsApiUrl), Connecting _ ->
        let toastCmd = errorToastCmd "Unable to create a web socket connection to the web server"
        let state = { state with AppState = ServiceUnavailable }
        addDebugMessage (errorMessage (sprintf "WsOnError when Connecting -> %s" wsApiUrl)) state, toastCmd
    | OnUiWsError (WsOnError wsApiUrl), _ ->
        let toastCmd = errorToastCmd "An unexpected web socket error has occurred"
        addDebugMessage (errorMessage (sprintf "WsOnError not when Connecting -> %s" wsApiUrl)) state, toastCmd
    | OnUiWsError (SendWsNotOpenError uiWsApi), _ ->
        let toastCmd = errorToastCmd "The web socket connection to the web server has been closed"
        addDebugMessage (errorMessage (sprintf "SendWsNotOpenError -> %A" uiWsApi)) state, toastCmd
    | OnUiWsError (SendWsOtherError (uiWsApi, errorText)), _ ->
        let toastCmd = errorToastCmd "Unable to send a web socket message"
        addDebugMessage (errorMessage (sprintf "SendWsOtherError -> %s -> %A" errorText uiWsApi)) state, toastCmd
    | OnUiWsError (DeserializeServerWsApiError errorText), _ ->
        let toastCmd = errorToastCmd "Unable to process a received web socket message"
        addDebugMessage (errorMessage (sprintf "DeserializeServerWsApiError -> %s" errorText)) state, toastCmd
    | HandleServerWsApi (ServerAppWsApi (ServerWsErrorWs (ReceiveError errorText))), _ ->
        let toastCmd = errorToastCmd "The web server was unable to receive a web socket message"
        addDebugMessage (errorMessage (sprintf "Server ReceiveError -> %s" errorText)) state, toastCmd
    | HandleServerWsApi (ServerAppWsApi (ServerWsErrorWs (DeserializeUiWsApiError errorText))), _ ->
        let toastCmd = errorToastCmd "The web server was unable to process a web socket message"
        addDebugMessage (errorMessage (sprintf "Server DeserializeUiWsApiError -> %s" errorText)) state, toastCmd 
    | HandleServerWsApi (ServerAppWsApi (ConnectedWs (otherConnections, signedIn))), Connecting jwt ->
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
                let unauthenticatedState, cmd = defaultUnauthenticatedStateOnly None state
                let autoCmd = unauthenticatedState.SendUiUnauthenticatedWsApi (AutoSignInWs jwt)
                { state with AppState = AutomaticallySigningIn jwt }, Cmd.batch [ cmd ; autoCmd ]
            | None -> defaultUnauthenticatedState None state       
        state, Cmd.batch [ cmd ; toastCmd ]
    | HandleServerWsApi serverWsApi, Connecting _ ->
        addDebugMessage (sprintf "Unexpected serverWsApi when Connecting -> %A" serverWsApi) state, Cmd.none
    | HandleServerWsApi (ServerAppWsApi (SignInResultWs (Ok authenticatedUser))), Unauthenticated _ ->
        let toastCmd = successToastCmd "You have signed in"
        let state, cmd = defaultAuthenticatedState authenticatedUser state
        state, Cmd.batch [ cmd ; toastCmd ; writePreferencesCmd state ]
    | HandleServerWsApi (ServerAppWsApi (SignInResultWs (Error errorText))), Unauthenticated unauthenticatedState ->
        let toastCmd = errorToastCmd (sprintf "Unable to sign in as %s" unauthenticatedState.UserNameText)
        let unauthenticatedState = { unauthenticatedState with SignInStatus = Some (Failed errorText) }
        let state = addDebugMessage (errorMessage (sprintf "SignInResultWs -> %s" errorText)) state // TODO-NMB: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Unauthenticated unauthenticatedState }, toastCmd
    | HandleServerWsApi (ServerAppWsApi (AutoSignInResultWs (Ok authenticatedUser))), AutomaticallySigningIn (Jwt _jwt) ->
        // TODO-NMB: Check authenticatedUser vs. _jwt?...
        let toastCmd = successToastCmd "You have been automatically signed in"
        let state, cmd = defaultAuthenticatedState authenticatedUser state
        state, Cmd.batch [ cmd ; toastCmd ]
    | HandleServerWsApi (ServerAppWsApi (AutoSignInResultWs (Error errorText))), AutomaticallySigningIn (Jwt jwt) ->
        let toastCmd = errorToastCmd (sprintf "Unable to automatically sign in as %s" jwt.UserName)
        let unauthenticatedState, cmd = defaultUnauthenticatedStateOnly (Some jwt.UserName) state
        let unauthenticatedState = { unauthenticatedState with SignInStatus = Some (Failed errorText) }
        let state = addDebugMessage (errorMessage (sprintf "AutoSignInResultWs -> %s" errorText)) state // TODO-NMB: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.batch [ cmd ; toastCmd ]
    | HandleServerWsApi serverWsApi, Unauthenticated _ ->
        addDebugMessage (sprintf "Unexpected serverWsApi when Unauthenticated -> %A" serverWsApi) state, Cmd.none
    | HandleServerWsApi (ServerAppWsApi (SignOutResultWs (Ok _sessionId))), Authenticated authenticatedState ->
        // TODO-NMB: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
        let toastCmd = successToastCmd "You have signed out"
        let state, cmd = defaultUnauthenticatedState (Some authenticatedState.AuthenticatedUser.UserName) state
        state, Cmd.batch [ cmd ; toastCmd ; writePreferencesCmd state ]
    | HandleServerWsApi (ServerAppWsApi (SignOutResultWs (Error errorText))), Authenticated authenticatedState ->
        let toastCmd = errorToastCmd "Unable to sign out"
        let authenticatedState = { authenticatedState with SignOutStatus = Some (Failed errorText) }
        let state = addDebugMessage (errorMessage (sprintf "SignOutResultWs -> %s" errorText)) state // TODO-NMB: Is this necessary, e.g. if errorText rendered elsewhere?...
        { state with AppState = Authenticated authenticatedState }, toastCmd
    | HandleServerWsApi (ServerAppWsApi (AutoSignOutWs _sessionId)), Authenticated authenticatedState ->
        // TODO-NMB: Check _sessionId vs. authenticatedState.AuthenticatedUser.SessionId?...
        let toastCmd = warningToastCmd "You have been automatically signed out"
        let state, cmd = defaultUnauthenticatedState (Some authenticatedState.AuthenticatedUser.UserName) state
        state, Cmd.batch [ cmd ; toastCmd ]
    | HandleServerWsApi (ServerAppWsApi (OtherUserSignedIn userName)), Authenticated _ ->
        let toastCmd = infoToastCmd (sprintf "%s has signed in" userName)
        state, toastCmd
    | HandleServerWsApi (ServerAppWsApi (OtherUserSignedOut userName)), Authenticated _ ->
        let toastCmd = infoToastCmd (sprintf "%s has signed out" userName)
        state, toastCmd
    | HandleServerWsApi (ServerChatWsApi serverChatWsApi), Authenticated _ ->
        let cmd = ReceiveServerWsApi (ServerChatWsApi serverChatWsApi) |> SharedInput |> ChatInput |> AuthenticatedInput |> AppInput |> Cmd.ofMsg
        state, cmd
    | HandleServerWsApi serverWsApi, Authenticated _ ->
        addDebugMessage (sprintf "Unexpected serverWsApi when Authenticated -> %A" serverWsApi) state, Cmd.none
    | HandleServerWsApi serverWsApi, _ ->
        addDebugMessage (sprintf "Unexpected serverWsApi when not Connecting, Unauthenticated or Authenticated -> %A" serverWsApi) state, Cmd.none
    | AppInput (ReadingPreferencesInput (ReadPreferencesResult (Ok (Some preferences)))), ReadingPreferences ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = Connecting preferences.Jwt }, Cmd.ofSub initializeWsSub
    | AppInput (ReadingPreferencesInput (ReadPreferencesResult (Ok None))), ReadingPreferences ->
        { state with AppState = Connecting None }, Cmd.ofSub initializeWsSub
    | AppInput (ReadingPreferencesInput (ReadPreferencesResult (Error exn))), ReadingPreferences ->
        let state = addDebugMessage (errorMessage (sprintf "ReadPreferencesResult -> %s" exn.Message)) state
        state, ReadPreferencesResult (Ok None) |> ReadingPreferencesInput |> AppInput |> Cmd.ofMsg
    | _, ReadingPreferences ->
        addDebugMessage (sprintf "Unexpected input when ReadingPreferences -> %A" input) state, Cmd.none
    | AppInput (ConnectingInput (WsOnOpen ws)), Connecting _ ->
        { state with Ws = Some ws }, Cmd.none
    | _, Connecting _ ->
        addDebugMessage (sprintf "Unexpected input when Connecting -> %A" input) state, Cmd.none
    | _, ServiceUnavailable ->
        addDebugMessage (sprintf "Unexpected input when ServiceUnavailable -> %A" input) state, Cmd.none
    | _, AutomaticallySigningIn _ ->
        addDebugMessage (sprintf "Unexpected input when AutomaticallySigningIn -> %A" input) state, Cmd.none
    | AppInput (UnauthenticatedInput (UserNameTextChanged userNameText)), Unauthenticated unauthenticatedState ->
        let unauthenticatedState = { unauthenticatedState with UserNameText = userNameText ; UserNameErrorText = validateUserNameText userNameText }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | AppInput (UnauthenticatedInput (PasswordTextChanged passwordText)), Unauthenticated unauthenticatedState ->
        let unauthenticatedState = { unauthenticatedState with PasswordText = passwordText ; PasswordErrorText = validatePasswordText passwordText }
        { state with AppState = Unauthenticated unauthenticatedState }, Cmd.none
    | AppInput (UnauthenticatedInput SignIn), Unauthenticated unauthenticatedState -> // note: assume no need to validate unauthenticatedState.UserNameText or unauthenticatedState.PasswordText (i.e. because App.Render.renderUnauthenticated will ensure that SendChatMessage can only be called when valid)
        let unauthenticatedState = { unauthenticatedState with SignInStatus = Some Pending }
        let cmd = unauthenticatedState.SendUiUnauthenticatedWsApi (SignInWs (state.SessionId, unauthenticatedState.UserNameText, unauthenticatedState.PasswordText))
        { state with AppState = Unauthenticated unauthenticatedState }, cmd
    | _, Unauthenticated _ ->
        addDebugMessage (sprintf "Unexpected input when Unauthenticated -> %A" input) state, Cmd.none
    | AppInput (AuthenticatedInput (ChatInput (SharedInput (AddDebugMessage debugMessage)))), Authenticated _ ->
        addDebugMessage debugMessage state, Cmd.none
    | AppInput (AuthenticatedInput (ChatInput (SharedInput (SendUnauthenticatedWsApi uiUnauthenticatedWsApi)))), Authenticated authenticatedState ->
        let cmd = authenticatedState.SendUiWsApi (UiUnauthenticatedWsApi uiUnauthenticatedWsApi)
        state, cmd
    | AppInput (AuthenticatedInput (ChatInput (SharedInput (SendAuthenticatedWsApi (_authenticatedUser, uiAuthenticatedWsApi))))), Authenticated authenticatedState ->
        // TODO-NMB: Check _authenticatedUser vs. authenticatedState.AuthenticatedUser?...
        let cmd = authenticatedState.SendUiWsApi (UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, uiAuthenticatedWsApi))
        state, cmd
    | AppInput (AuthenticatedInput (ChatInput chatInput)), Authenticated authenticatedState ->
        let chatState, chatCmd = Chat.State.transition chatInput authenticatedState.ChatState
        let authenticatedState = { authenticatedState with ChatState = chatState }
        { state with AppState = Authenticated authenticatedState }, chatCmd |> Cmd.map (ChatInput >> AuthenticatedInput >> AppInput)
    | AppInput (AuthenticatedInput SignOut), Authenticated authenticatedState ->
        let authenticatedState = { authenticatedState with SignOutStatus = Some Pending }
        let cmd = authenticatedState.SendUiWsApi (UiAuthenticatedWsApi (Jwt authenticatedState.AuthenticatedUser, SignOutWs))
        { state with AppState = Authenticated authenticatedState }, cmd
    | _, Authenticated _ ->
        addDebugMessage (sprintf "Unexpected input when Authenticated -> %A" input) state, Cmd.none
    (* TEMP-NMB: Prevent warning for unmatched UiInput * AppState cases...
    | _ -> unchanged
    ...NMB-TEMP *)
