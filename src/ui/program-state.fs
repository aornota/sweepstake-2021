module Aornota.Sweepstake2018.UI.Program.State

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.UnexpectedError
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LocalStorage
open Aornota.UI.Common.Notifications
open Aornota.UI.Common.Toasts
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages
open Aornota.Sweepstake2018.UI.Pages.Chat.Common
open Aornota.Sweepstake2018.UI.Program.Common

open System

open Elmish

open Fable.Core.JsInterop
open Fable.Import
module Brw = Fable.Import.Browser

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2018-ui-app-preferences"

let private setBodyClass useDefaultTheme = Browser.document.body.className <- getThemeClass (getTheme useDefaultTheme).ThemeClass

let private readPreferencesCmd =
    let readPreferences () = async {
        do! ifDebugSleepAsync 20 100
        return Key APP_PREFERENCES_KEY |> readJson |> Option.map (fun (Json json) -> json |> ofJson<Preferences>) }
    Cmd.ofAsync readPreferences () (Ok >> ReadingPreferencesInput >> AppInput) (Error >> ReadingPreferencesInput >> AppInput)

let private writePreferencesCmd state =
    let writePreferences uiState = async {
        let lastPage =
            match uiState.AppState with
            | Unauth unauthState -> UnauthPage unauthState.CurrentUnauthPage |> Some
            | Auth authState -> authState.CurrentPage |> Some
            | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> None
        let user =
            match uiState.AppState with
            | Auth authState -> (authState.AuthUser.UserName, authState.AuthUser.Jwt) |> Some
            | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ | Unauth _ -> None
        let preferences = { UseDefaultTheme = uiState.UseDefaultTheme ; SessionId = uiState.SessionId ; LastPage = lastPage ; User = user }
        do preferences |> toJson |> Json |> writeJson (Key APP_PREFERENCES_KEY) }
    Cmd.ofAsync writePreferences state (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private initializeWsSub dispatch =
    let receiveServerMsg (wsMessage:Brw.MessageEvent) : unit =
        try // note: expect wsMessage.data to be deserializable to ServerMsg
            let serverMsg = unbox wsMessage.data |> ofJson<ServerMsg>
            ifDebugFakeErrorFailWith (sprintf "Fake error deserializing %A" serverMsg)
            HandleServerMsg serverMsg |> dispatch
        with exn -> WsError (DeserializeServerMsgError exn.Message) |> dispatch
    let wsUrl =
#if AZURE
        "wss://sweepstake-2018.azurewebsites.net:443" // note: WS_PORT irrelevant for Azure (since effectively "internal")
#else
        sprintf "ws://localhost:%i" WS_PORT
#endif
    let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
    try
        let ws = Brw.WebSocket.Create wsApiUrl
        ws.onopen <- (fun _ -> ConnectingInput ws |> AppInput |> dispatch)
        ws.onerror <- (fun _ -> WsError (WsOnError wsApiUrl) |> dispatch)
        ws.onmessage <- receiveServerMsg
        ()
    with _ -> WsError (WsOnError wsApiUrl) |> dispatch

let private sendMsg (ws:Brw.WebSocket) (uiMsg:UiMsg) =
    if ws.readyState <> ws.OPEN then WsError (SendMsgWsNotOpenError uiMsg) |> Cmd.ofMsg
    else
        try
            ifDebugFakeErrorFailWith "Fake sendMsg error"
            ws.send (uiMsg |> toJson)
            Cmd.none
        with exn -> WsError (SendMsgOtherError (uiMsg, exn.Message)) |> Cmd.ofMsg

let private shouldNeverHappenText text = sprintf "SHOULD NEVER HAPPEN -> %s" text

let private sendUnauthMsgCmd (ws:Brw.WebSocket option) uiUnauthMsg =
    match ws with
    | Some ws -> uiUnauthMsg |> UiUnauthMsg |> sendMsg ws
    | None -> AddNotificationMessage (debugDismissableMessage (shouldNeverHappenText "sendUnauthMsgCmd called when WebSocket is None")) |> Cmd.ofMsg

let private sendAuthMsgCmd (ws:Brw.WebSocket option) jwt uiAuthMsg =
    match ws with
    | Some ws -> (jwt, uiAuthMsg) |> UiAuthMsg |> sendMsg ws
    | None -> AddNotificationMessage (debugDismissableMessage (shouldNeverHappenText "sendAuthMsgCmd called when WebSocket is None")) |> Cmd.ofMsg

let private addNotificationMessage notificationMessage state = { state with NotificationMessages = notificationMessage :: state.NotificationMessages }

let private addDebugMessage debugText state = addNotificationMessage (debugDismissableMessage debugText) state
let private addInfoMessage infoText state = addNotificationMessage (infoDismissableMessage infoText) state
let private addWarningMessage warningText state = addNotificationMessage (warningDismissableMessage warningText) state
let private addDangerMessage dangerText state = addNotificationMessage (dangerDismissableMessage dangerText) state

let private shouldNeverHappen debugText state : State * Cmd<Input> = addDebugMessage (shouldNeverHappenText debugText) state, Cmd.none

let private addDebugError debugText toastText state : State * Cmd<Input> =
    addDebugMessage (sprintf "ERROR -> %s" debugText) state, match toastText with | Some toastText -> toastText |> errorToastCmd | None -> Cmd.none

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

let private handleWsError wsError state : State * Cmd<Input> =
    match wsError, state.AppState with
    | WsOnError wsApiUrl, Connecting _ ->
        let uiState = { state with AppState = ServiceUnavailable }
        addDebugError (sprintf "WsOnError when Connecting -> %s" wsApiUrl) ("Unable to create a connection to the web server<br><br>Please try again later" |> Some) uiState
    | WsOnError wsApiUrl, _ -> addDebugError (sprintf "WsOnError not when Connecting -> %s" wsApiUrl) (UNEXPECTED_ERROR |> Some) state
    | SendMsgWsNotOpenError uiMsg, _ ->
        addDebugError (sprintf "SendMsgWsNotOpenError -> %A" uiMsg) ("The connection to the web server has been closed<br><br>Please try refreshing the page" |> Some) state
    | SendMsgOtherError (uiMsg, errorText), _ -> addDebugError (sprintf "SendMsgOtherError -> %s -> %A" errorText uiMsg) (unexpectedErrorWhen "sending a message" |> Some) state
    | DeserializeServerMsgError errorText, _ -> addDebugError (sprintf "DeserializeServerMsgError -> %s" errorText) (unexpectedErrorWhen "processing a received message" |> Some) state

let private handleServerUiMsgError serverUiMsgError state =
    match serverUiMsgError with
    | ReceiveUiMsgError errorText ->
        addDebugError (sprintf "Server ReceiveUiMsgError -> %s" errorText) ("The web server was unable to receive a message<br><br>Please try refreshing the page" |> Some) state
    | DeserializeUiMsgError errorText ->
        addDebugError (sprintf "Server DeserializeUiMsgError -> %s" errorText) ("The web server was unable to process a message<br><br>Please try refreshing the page" |> Some) state

let private handleConnected (otherConnections, signedIn) user lastPage state =
    let toastCmd =
#if DEBUG
        // TEMP-NMB: Show [ other-web-socket-connection | signed-in-user ] counts (as toast)...
        let otherConnections = if otherConnections > 0 then sprintf "<strong>%i</strong>" otherConnections else sprintf "%i" otherConnections
        let signedIn = if signedIn > 0 then sprintf "<strong>%i</strong>" signedIn else sprintf "%i" signedIn
        sprintf "Other web socket connections: %s<br>Signed-in users: %s" otherConnections signedIn |> infoToastCmd
        // ...or not...
        //Cmd.none
        // ...NMB-TEMP
#else
        Cmd.none
#endif
    let state, cmd =
        match user with
        | Some (userName, jwt) ->
            { state with AppState = ((userName, jwt), lastPage) |> AutomaticallySigningIn }, (state.SessionId, jwt) |> AutoSignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.Ws
        | None ->
            let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
            let showPageCmd = match lastPage with | Some lastPage -> lastPage |> ShowUnauthPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
            // TEMP-NMB: ShowSignInModal once connected...
            let showSignInCmd =
                ShowSignInModal |> UnauthInput |> AppInput |> Cmd.ofMsg
            // ...or not...
                //Cmd.none
            // ...NMB-TEMP
            let state, cmd = defaultUnauthState None None state
            state, Cmd.batch [ cmd ; showPageCmd ; showSignInCmd ]
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult (result:Result<AuthUser,SignInCmdError<string>>) unauthState state =
    match unauthState.SignInState, result with
    | Some _, Ok authUser ->
        let currentPage = UnauthPage unauthState.CurrentUnauthPage |> Some
        let state, cmd = defaultAuthState authUser currentPage (unauthState |> Some) state
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; sprintf "You have signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Some signInState, Error error ->
        let toastCmd = sprintf "Unable to sign in as <strong>%s</strong>" signInState.UserNameText |> errorToastCmd
        let errorText =
            match error with
            | InvalidCredentials (Some errorText) -> errorText
            | InvalidCredentials None -> sprintf "Unable to sign in as %s" signInState.UserNameText
            | SignInCmdJwtError _ | OtherSignInCmdError _ -> unexpectedErrorWhen "signing in"
        let errorText = ifDebug (sprintf "SignInCmdResult error -> %A" error) errorText
        let signInState = { signInState with SignInStatus = Failed errorText |> Some }
        { state with AppState = Unauth { unauthState with SignInState = signInState |> Some } }, toastCmd
    | None, _ -> shouldNeverHappen (sprintf "Unexpected SignInCmdResult when SignInState is None -> %A" result) state

let private handleAutoSignInResult (result:Result<AuthUser,AutoSignInCmdError<string>>) userName lastPage state =
    match result with
    | Ok authUser ->
        let showPageCmd = match lastPage with | Some lastPage -> lastPage |> ShowPage |> AuthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultAuthState authUser None None state
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ showPageCmd ; cmd ; sprintf "You have been automatically signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Error error ->
        let (UserName userName) = userName
        let toastCmd = sprintf "Unable to automatically sign in as <strong>%s</strong>" userName |> errorToastCmd
        let errorText = ifDebug (sprintf "AutoSignInCmdResult error -> %A" error) (unexpectedErrorWhen "automatically signing in")
        let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
        let signInState = defaultSignInState (userName |> Some) (Failed errorText |> Some)
        let showPageCmd = match lastPage with | Some lastPage -> lastPage |> ShowUnauthPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = defaultUnauthState None (signInState |> Some) state
        state, Cmd.batch [ cmd ; showPageCmd ; toastCmd ]

let private handleSignOutResult (result:Result<unit, AuthCmdError<string>>) authState state =
    let toastCmd = "You have signed out" |> successToastCmd
    match authState.SigningOut, result with
    | true, Ok _ ->
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = defaultUnauthState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; toastCmd ]
    | true, Error error ->
        let state, _ = ifDebug (addDebugError (sprintf "SignOutCmdResult error -> %A" error) None state) (addError (unexpectedErrorWhen "signing out") state, Cmd.none)
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = defaultUnauthState currentPage None state
        state, Cmd.batch [ cmd ; writePreferencesCmd state ; toastCmd ]
    | false, _ -> shouldNeverHappen (sprintf "Unexpected SignOutCmdResult when not SigningOut -> %A" result) state

let private handleAutoSignOut (_reason:AutoSignOutReason option) authState state =
    // TODO-NMB-MEDIUM: Do something if reason is Some [PasswordReset resetBy | PermissionsChanges isPersonaNotGrata], e.g. add NotificationMessage?...
    let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
    let state, cmd = defaultUnauthState currentPage None state
    state, Cmd.batch [ cmd ; writePreferencesCmd state ; "You have been automatically signed out" |> warningToastCmd ]

let private handleServerAppMsg serverAppMsg state =
    match serverAppMsg, state.AppState with
    | ServerUiMsgErrorMsg serverUiMsgError, _ -> handleServerUiMsgError serverUiMsgError state
    | ConnectedMsg (otherConnections, signedIn), Connecting (user, lastPage) -> handleConnected (otherConnections, signedIn) user lastPage state
    | SignInCmdResult result, Unauth unauthState -> handleSignInResult result unauthState state
    | AutoSignInCmdResult result, AutomaticallySigningIn ((userName, _), lastPage) -> handleAutoSignInResult result userName lastPage state
    | SignOutCmdResult result, Auth authState -> handleSignOutResult result authState state
    | AutoSignOutMsg reason, Auth authState -> handleAutoSignOut reason authState state
    | OtherUserSignedInMsgOLD userName, Auth _ -> state, sprintf "<strong>%s</strong> has signed in" userName |> infoToastCmd
    | OtherUserSignedOutMsgOLD userName, Auth _ -> state, sprintf "<strong>%s</strong> has signed out" userName |> infoToastCmd
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerAppMsg when %s -> %A" (appStateText appState) serverAppMsg) state

let private handleServerMsg serverMsg state =
    match serverMsg, state.AppState with
    | ServerAppMsg serverAppMsg, _ -> handleServerAppMsg serverAppMsg state
    | ServerChatMsg serverChatMsg, Auth _ -> state, serverChatMsg |> ReceiveServerChatMsg |> ChatInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | _, appState -> shouldNeverHappen (sprintf "Unexpected ServerMsg when %s -> %A" (appStateText appState) serverMsg) state

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = (preferences.User, preferences.LastPage) |> Connecting }, Cmd.ofSub initializeWsSub
    | Ok None -> { state with AppState = (None, None) |> Connecting }, Cmd.ofSub initializeWsSub
    | Error exn ->
        let state, _ = addDebugError (sprintf "ReadPreferencesResult -> %s" exn.Message) None state // note: no need for toast
        state, None |> Ok |> ReadingPreferencesInput |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with Ws = ws |> Some }, Cmd.none

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
        let unauthState = { unauthState with SignInState = defaultSignInState None None |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (UserNameTextChanged userNameText), Some signInState ->
        let signInState = { signInState with UserNameText = userNameText ; UserNameErrorText = validateUserName [] (UserName userNameText) }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (PasswordTextChanged passwordText), Some signInState ->
        let signInState = { signInState with PasswordText = passwordText ; PasswordErrorText = validatePassword (Password passwordText) }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput SignIn, Some signInState -> // note: assume no need to validate signInState.UserNameText or signInState.PasswordText (i.e. because App.Render.renderSignInModal will ensure that SignIn can only be dispatched when valid)
        let signInState = { signInState with SignInStatus = Some Pending }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        let cmd = (state.SessionId, UserName signInState.UserNameText, Password signInState.PasswordText) |> SignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.Ws
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
                    chatState |> Some, chatCmd
                | _, Some chatState -> chatState |> Some, ToggleChatIsCurrentPage (page = AuthPage ChatPage) |> Cmd.ofMsg
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
    | PageInput (APageInput (ChatInput ShowMarkdownSyntaxModal)), false -> { state with StaticModal = MarkdownSyntax |> Some }, Cmd.none
    | PageInput (APageInput (ChatInput (SendUiAuthMsg uiAuthMsg))), false ->
        state, uiAuthMsg |> sendAuthMsgCmd state.Ws authState.AuthUser.Jwt
    | PageInput (APageInput (ChatInput chatInput)), false ->
        match authState.AuthPageStates.ChatState with
        | Some chatState ->
            let chatState, chatCmd = Chat.State.transition chatInput chatState
            let authPageStates = { authState.AuthPageStates with ChatState = chatState |> Some }
            { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
        | None -> shouldNeverHappen "Unexpected ChatInput when ChatState is None" state
    | ChangePassword, false -> state, "Change password functionality is coming soon" |> warningToastCmd
    | SignOut, false ->
        let cmd = SignOutCmd |> UiAuthAppMsg |> sendAuthMsgCmd state.Ws authState.AuthUser.Jwt
        { state with AppState = Auth { authState with SigningOut = true } }, cmd
    | UserAdministration, false -> // TODO-NMB-LOW: Check that authState.AuthUser has appropriate permissions?...
        state, "User administration functionality is coming soon" |> warningToastCmd
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
        let state = { state with UseDefaultTheme = (state.UseDefaultTheme |> not) }
        setBodyClass state.UseDefaultTheme
        state, writePreferencesCmd state
    | ToggleNavbarBurger -> { state with NavbarBurgerIsActive = (state.NavbarBurgerIsActive |> not) }, Cmd.none
    | ShowStaticModal staticModal -> { state with StaticModal = staticModal |> Some }, Cmd.none
    | HideStaticModal -> { state with StaticModal = None }, Cmd.none
    | WritePreferencesResult (Ok _) -> state, Cmd.none
    | WritePreferencesResult (Error exn) -> addDebugError (sprintf "WritePreferencesResult -> %s" exn.Message) None state // note: no need for toast
    | WsError wsError -> handleWsError wsError state
    | HandleServerMsg serverMsg -> handleServerMsg serverMsg state
    | AppInput appInput -> handleAppInput appInput state
