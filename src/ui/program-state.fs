module Aornota.Sweepstake2018.UI.Program.State

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LocalStorage
open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Common.Toasts
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.User
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

#if TICK
let [<Literal>] private WIFF_INTERVAL = 30.<second>
#endif

let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>

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
    let receiveServerMsg (wsMessage:Brw.MessageEvent) =
        try // note: expect wsMessage.data to be deserializable to ServerMsg
            let serverMsg = wsMessage.data |> unbox |> ofJson<ServerMsg>
            ifDebugFakeErrorFailWith (sprintf "Fake error deserializing %A" serverMsg)
            serverMsg |> HandleServerMsg |> dispatch
        with exn -> exn.Message |> DeserializeServerMsgError |> WsError |> dispatch
    let wsUrl =
#if AZURE
        "wss://sweepstake-2018.azurewebsites.net:443" // note: WS_PORT irrelevant for Azure (since effectively "internal")
#else
        sprintf "ws://localhost:%i" WS_PORT
#endif
    let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
    try
        let ws = Brw.WebSocket.Create wsApiUrl
        ws.onopen <- (fun _ -> ws |> ConnectingInput |> AppInput |> dispatch)
        ws.onerror <- (fun _ -> wsApiUrl |> WsOnError |> WsError |> dispatch)
        ws.onmessage <- receiveServerMsg
        ()
    with _ -> wsApiUrl |> WsOnError |> WsError |> dispatch

let private sendMsg (ws:Brw.WebSocket) (uiMsg:UiMsg) =
    if ws.readyState <> ws.OPEN then uiMsg |> SendMsgWsNotOpenError |> WsError |> Cmd.ofMsg
    else
        try
            ifDebugFakeErrorFailWith "Fake sendMsg error"
            uiMsg |> toJson |> ws.send
            Cmd.none
        with exn -> (uiMsg, exn.Message) |> SendMsgOtherError |> WsError |> Cmd.ofMsg

let private sendUnauthMsgCmd connectionState uiUnauthMsg =
    match connectionState with
    | Connected connectedState ->
        uiUnauthMsg |> UiUnauthMsg |> sendMsg connectedState.Ws
    | NotConnected | InitializingConnection _ ->
        shouldNeverHappenText "sendUnauthMsgCmd called when ConnectionState is not Connected" |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private sendAuthMsgCmd connectionState authState uiAuthMsg =
    match connectionState with
    | Connected connectedState ->
        let authState = { authState with LastUserActivity = DateTimeOffset.UtcNow }
        authState, (authState.AuthUser.Jwt, uiAuthMsg) |> UiAuthMsg |> sendMsg connectedState.Ws
    | NotConnected | InitializingConnection _ ->
        authState, shouldNeverHappenText "sendAuthMsgCmd called when ConnectionState is not Connected" |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private addNotificationMessage notificationMessage state = { state with NotificationMessages = notificationMessage :: state.NotificationMessages }

let private addDebugMessage debugText state = state |> addNotificationMessage (debugText |> debugDismissableMessage)
let private addInfoMessage infoText state = state |> addNotificationMessage (infoText |> infoDismissableMessage)
let private addWarningMessage warningText state = state |> addNotificationMessage (warningText |> warningDismissableMessage)
let private addDangerMessage dangerText state = state |> addNotificationMessage (dangerText |> dangerDismissableMessage)

let private shouldNeverHappen debugText state : State * Cmd<Input> = state |> addDebugMessage (shouldNeverHappenText debugText), Cmd.none

let private addDebugError debugText toastText state : State * Cmd<Input> =
    state |> addDebugMessage (sprintf "ERROR -> %s" debugText), match toastText with | Some toastText -> toastText |> errorToastCmd | None -> Cmd.none

let private addError errorText state = state |> addDangerMessage errorText

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
        CurrentUnauthPage = match currentPage with | Some currentPage -> currentPage | None -> NewsPage
        UnauthPageStates = { NewsState = () ; SquadsState = () }
        SignInState = signInState }
    { state with AppState = Unauth unauthState }, Cmd.none

let defaultChangePasswordState mustChangePasswordReason changePasswordStatus = {
    MustChangePasswordReason = mustChangePasswordReason
    NewPasswordKey = Guid.NewGuid ()
    NewPasswordText = String.Empty
    NewPasswordErrorText = None
    ConfirmPasswordKey = Guid.NewGuid ()
    ConfirmPasswordText = String.Empty
    ConfirmPasswordErrorText = None
    ChangePasswordStatus = changePasswordStatus }

let private defaultAuthState authUser currentPage (unauthState:UnauthState option) state =
    let currentPage = match currentPage with | Some currentPage -> currentPage | None -> AuthPage ChatPage
    // Note: No actual need to call Chat.State.initialize here as will be initialized on demand - i.e. by ShowPage (AuthPage ChatPage) - but no harm in being pre-emptive.
    let chatState, chatCmd = Chat.State.initialize authUser (currentPage = AuthPage ChatPage)
    let authState = {
        AuthUser = authUser
        LastUserActivity = DateTimeOffset.UtcNow
        CurrentPage = currentPage
        UnauthPageStates = match unauthState with | Some unauthState -> unauthState.UnauthPageStates | None -> { NewsState = () ; SquadsState = () }
        AuthPageStates = { DraftsState = () ; ChatState = chatState |> Some ; UserAdministrationState = () }
        ChangePasswordState =
            match authUser.MustChangePasswordReason with
            | Some mustChangePasswordReason -> defaultChangePasswordState (mustChangePasswordReason |> Some) None |> Some
            | None -> None
        SigningOut = false }
    let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
    { state with AppState = Auth authState }, chatCmd

let initialize () =
    let state = {
        Ticks = 0<tick>
        LastWiff = DateTimeOffset.UtcNow
        NotificationMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
        StaticModal = None
        ConnectionState = NotConnected
        AppState = ReadingPreferences }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let private handleWsError wsError state : State * Cmd<Input> =
    match wsError, state.AppState with
    | WsOnError wsApiUrl, Connecting _ ->
        let state = { state with ConnectionState = NotConnected ; AppState = ServiceUnavailable }
        state |> addDebugError (sprintf "WsOnError when Connecting -> %s" wsApiUrl) ("Unable to create a connection to the web server<br><br>Please try again later" |> Some)
    | WsOnError wsApiUrl, _ ->
        state |> addDebugError (sprintf "WsOnError when not Connecting -> %s" wsApiUrl) (UNEXPECTED_ERROR |> Some)
    | SendMsgWsNotOpenError uiMsg, _ ->
        state |> addDebugError (sprintf "SendMsgWsNotOpenError -> %A" uiMsg) ("The connection to the web server has been closed<br><br>Please try refreshing the page" |> Some)
    | SendMsgOtherError (uiMsg, errorText), _ ->
        state |> addDebugError (sprintf "SendMsgOtherError -> %s -> %A" errorText uiMsg) (unexpectedErrorWhen "sending a message" |> Some)
    | DeserializeServerMsgError errorText, _ ->
        state |> addDebugError (sprintf "DeserializeServerMsgError -> %s" errorText) (unexpectedErrorWhen "processing a received message" |> Some)

let private handleServerUiMsgError serverUiMsgError state =
    match serverUiMsgError with
    | ReceiveUiMsgError errorText ->
        state |> addDebugError (sprintf "Server ReceiveUiMsgError -> %s" errorText) ("The web server was unable to receive a message<br><br>Please try refreshing the page" |> Some)
    | DeserializeUiMsgError errorText ->
        state |> addDebugError (sprintf "Server DeserializeUiMsgError -> %s" errorText) ("The web server was unable to process a message<br><br>Please try refreshing the page" |> Some)

let private handleConnected ws (serverStarted:DateTimeOffset) otherConnectionCount signedInUserCount user lastPage state =
    let toastCmd =
#if DEBUG
        let serverStarted = ago serverStarted.LocalDateTime
        let otherConnections = if otherConnectionCount > 0 then sprintf "<strong>%i</strong>" otherConnectionCount else sprintf "%i" otherConnectionCount
        let signedInUsers = if signedInUserCount > 0 then sprintf "<strong>%i</strong>" signedInUserCount else sprintf "%i" signedInUserCount
        sprintf "Server started: %s<br>Other web socket connections: %s<br>Signed-in users: %s" serverStarted otherConnections signedInUsers |> infoToastCmd
#else
        Cmd.none
#endif
    let state = { state with ConnectionState = Connected { Ws = ws ; ServerStarted = serverStarted } }
    let state, cmd =
        match user with
        | Some (userName, jwt) ->
            let state = { state with AppState = ((userName, jwt), lastPage) |> AutomaticallySigningIn }
            let cmd = (state.SessionId, jwt) |> AutoSignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
            state, cmd
        | None ->
            let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
            let showPageCmd = match lastPage with | Some lastPage -> lastPage |> ShowUnauthPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
            let showSignInCmd = ShowSignInModal |> UnauthInput |> AppInput |> Cmd.ofMsg
            let state, cmd = state |> defaultUnauthState None None
            state, Cmd.batch [ cmd ; showPageCmd ; showSignInCmd ]
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult (result:Result<AuthUser,SignInCmdError<string>>) unauthState state =
    match unauthState.SignInState, result with
    | Some _, Ok authUser ->
        let currentPage = UnauthPage unauthState.CurrentUnauthPage |> Some
        let state, cmd = state |> defaultAuthState authUser currentPage (unauthState |> Some)
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; sprintf "You have signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Some signInState, Error error ->
        let toastCmd = sprintf "Unable to sign in as <strong>%s</strong>" signInState.UserNameText |> errorToastCmd
        let errorText =
            match error with
            | InvalidCredentials (Some errorText) -> errorText
            | InvalidCredentials None -> sprintf "Unable to sign in as %s" signInState.UserNameText
            | SignInCmdJwtError _ | OtherSignInCmdError _ -> unexpectedErrorWhen "signing in"
        let errorText = ifDebug (sprintf "SignInCmdResult error -> %A" error) errorText
        let signInState = { signInState with SignInStatus = errorText |> SignInFailed |> Some }
        { state with AppState = Unauth { unauthState with SignInState = signInState |> Some } }, toastCmd
    | None, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected SignInCmdResult when SignInState is None -> %A" result)

let private handleAutoSignInResult (result:Result<AuthUser,AutoSignInCmdError<string>>) userName lastPage state =
    match result with
    | Ok authUser ->
        let state, cmd = state |> defaultAuthState authUser lastPage None
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ cmd ; sprintf "You have been automatically signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Error error ->
        let (UserName userName) = userName
        let toastCmd = sprintf "Unable to automatically sign in as <strong>%s</strong>" userName |> errorToastCmd
        let errorText = ifDebug (sprintf "AutoSignInCmdResult error -> %A" error) (unexpectedErrorWhen "automatically signing in")
        let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
        let signInState = defaultSignInState (userName |> Some) (errorText |> SignInFailed |> Some)
        let showPageCmd = match lastPage with | Some lastPage -> lastPage |> ShowUnauthPage |> UnauthInput |> AppInput |> Cmd.ofMsg | None -> Cmd.none
        let state, cmd = state |> defaultUnauthState None (signInState |> Some)
        state, Cmd.batch [ cmd ; showPageCmd ; state |> writePreferencesCmd ; toastCmd ]

let private handleChangePasswordResult (result:Result<Rvn, AuthCmdError<string>>) authState state =
    match authState.ChangePasswordState with
    | Some changePasswordState ->
        match changePasswordState.ChangePasswordStatus, result with
        | Some ChangePasswordPending, Ok rvn ->
            let authUser = { authState.AuthUser with Rvn = rvn }
            let authState = { authState with AuthUser = authUser ; ChangePasswordState = None }
            { state with AppState = Auth authState }, "Your password has been changed" |> successToastCmd
        | Some ChangePasswordPending, Error error ->
            let errorText =
                match error with
                | OtherAuthCmdError (OtherError errorText) -> errorText
                | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> unexpectedErrorWhen "changing password"
            let errorText = ifDebug (sprintf "ChangePasswordCmdResult error -> %A" error) errorText
            let changePasswordState = { changePasswordState with ChangePasswordStatus = errorText |> ChangePasswordFailed |> Some }
            let authState = { authState with ChangePasswordState = changePasswordState |> Some }
            { state with AppState = Auth authState }, "Unable to change password" |> errorToastCmd
        | Some _, _ | None, _ ->
            state |> shouldNeverHappen (sprintf "Unexpected ChangePasswordCmdResult when ChangePasswordState is Some but not ChangePasswordPending -> %A" result)
    | None ->
        state |> shouldNeverHappen (sprintf "Unexpected ChangePasswordCmdResult when ChangePasswordState is None -> %A" result)

let private handleSignOutResult (result:Result<unit, AuthCmdError<string>>) authState state =
    let toastCmd = "You have signed out" |> successToastCmd
    match authState.SigningOut, result with
    | true, Ok _ ->
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = state |> defaultUnauthState currentPage None
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; toastCmd ]
    | true, Error error ->
        let state, _ = ifDebug (state |> addDebugError (sprintf "SignOutCmdResult error -> %A" error) None) (state |> addError (unexpectedErrorWhen "signing out"), Cmd.none)
        let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = state |> defaultUnauthState currentPage None
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; toastCmd ]
    | false, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected SignOutCmdResult when not SigningOut -> %A" result)

let private handleAutoSignOut autoSignOutReason authState state =
    let because reasonText = sprintf "You have been automatically signed out because %s" reasonText
    let state, toastCmd =
        match autoSignOutReason with
        | Some PasswordReset -> state |> addWarningMessage (because "your password has been reset by a system administrator"), warningToastCmd
        | Some (PermissionsChanged false) -> state |> addWarningMessage (because "your permissions have been changed by a system administrator"), warningToastCmd
        | Some (PermissionsChanged true) -> state |> addDangerMessage (because "you are no longer permitted to access this system"), errorToastCmd
        | None -> state, infoToastCmd
    let currentPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
    let state, cmd = state |> defaultUnauthState currentPage None
    state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; "You have been automatically signed out" |> toastCmd ]

let private handleServerAppMsg serverAppMsg state =
    match serverAppMsg, state.AppState, state.ConnectionState with
    | ServerUiMsgErrorMsg serverUiMsgError, _, _ ->
        state |> handleServerUiMsgError serverUiMsgError
    | ConnectedMsg (serverStarted, otherConnections, signedIn), Connecting (user, lastPage), InitializingConnection ws ->
        state |> handleConnected ws serverStarted otherConnections signedIn user lastPage
    | SignInCmdResult result, Unauth unauthState, Connected _ ->
        state |> handleSignInResult result unauthState
    | AutoSignInCmdResult result, AutomaticallySigningIn ((userName, _), lastPage), Connected _ ->
        state |> handleAutoSignInResult result userName lastPage
    | ChangePasswordCmdResult result, Auth authState, Connected _ ->
        state |> handleChangePasswordResult result authState
    | SignOutCmdResult result, Auth authState, Connected _ ->
        state |> handleSignOutResult result authState
    | AutoSignOutMsg reason, Auth authState, Connected _ ->
        state |> handleAutoSignOut reason authState
    | _, _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected ServerAppMsg when %A (%A) -> %A" state.AppState state.ConnectionState serverAppMsg)

let private handleServerMsg serverMsg state =
    match serverMsg, state.AppState with
    | Waff, _ -> // note: silently ignored
        state, Cmd.none
    | ServerAppMsg serverAppMsg, _ ->
        state |> handleServerAppMsg serverAppMsg
    | ServerChatMsg serverChatMsg, Auth _ ->
        state, serverChatMsg |> ReceiveServerChatMsg |> ChatInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerChatMsg _, Unauth _ -> // note: silently ignore ServerChatMsg if Unauth
        state, Cmd.none
    | _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected ServerMsg when %A -> %A" state.AppState serverMsg)

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = (preferences.User, preferences.LastPage) |> Connecting }, initializeWsSub |> Cmd.ofSub
    | Ok None ->
        { state with AppState = (None, None) |> Connecting }, initializeWsSub |> Cmd.ofSub
    | Error exn ->
        let state, _ = state |> addDebugError (sprintf "ReadPreferencesResult -> %s" exn.Message) None // note: no need for toast
        state, None |> Ok |> ReadingPreferencesInput |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with ConnectionState = InitializingConnection ws }, Cmd.none

let private handleUnauthInput unauthInput unauthState state =
    match unauthInput, unauthState.SignInState with
    | ShowUnauthPage unauthPage, _ ->
        if unauthState.CurrentUnauthPage <> unauthPage then
            // TODO-NMB-MEDIUM: Initialize "optional" pages (if required) and toggle "IsCurrent" for relevant pages...
            let unauthState = { unauthState with CurrentUnauthPage = unauthPage }
            let state = { state with AppState = Unauth unauthState }
            state, state |> writePreferencesCmd
        else state, Cmd.none
    | UnauthPageInput NewsInput, None ->
        state |> shouldNeverHappen "Unexpected NewsInput -> NYI"
    | UnauthPageInput SquadsInput, None ->
        state |> shouldNeverHappen "Unexpected SquadsInput -> NYI"
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
    | SignInInput SignIn, Some signInState -> // note: assume no need to validate UserNameText or PasswordText (i.e. because App.Render.renderSignInModal will ensure that SignIn can only be dispatched when valid)
        let signInState = { signInState with SignInStatus = SignInPending |> Some }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        let signInCmdParams = state.SessionId, UserName (signInState.UserNameText.Trim ()), Password (signInState.PasswordText.Trim ())
        let cmd = signInCmdParams |> SignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
        { state with AppState = Unauth unauthState }, cmd
    | SignInInput CancelSignIn, Some signInState ->
        match signInState.SignInStatus with
        | Some SignInPending -> state |> shouldNeverHappen "Unexpected CancelSignIn when SignInPending"
        | Some _ | None ->
            let unauthState = { unauthState with SignInState = None }
            { state with AppState = Unauth unauthState }, Cmd.none
    | _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected UnauthInput when SignIsState is %A -> %A" unauthState.SignInState unauthInput)

let private handleAuthInput authInput authState state =
    match authInput, authState.ChangePasswordState, authState.SigningOut with
    | ShowPage page, None, false ->
        if authState.CurrentPage <> page then
            match page, authState.AuthUser.Permissions.UserAdministrationPermissions with
            | AuthPage UserAdministrationPage, None -> // note: would expect "Permissions mismatch" AutoSignInCmdResult error instead
                let state, cmd = state |> shouldNeverHappen "Unexpected ShowPage UserAdministrationPage when UserAdministrationPermissions is None"
                state, cmd, false
            | _, _ ->
                let chatState, chatCmd =
                    match page, authState.AuthPageStates.ChatState with
                    | AuthPage ChatPage, None ->
                        let chatState, chatCmd = Chat.State.initialize authState.AuthUser true
                        chatState |> Some, chatCmd
                    | _, Some chatState -> chatState |> Some, page = AuthPage ChatPage |> ToggleChatIsCurrentPage |> Cmd.ofMsg
                    | _, None -> None, Cmd.none
                // TODO-NMB-MEDIUM: Initialize other "optional" pages (if required) and toggle "IsCurrent" for other relevant pages...
                let authPageStates = { authState.AuthPageStates with ChatState = chatState }
                let authState = { authState with CurrentPage = page ; AuthPageStates = authPageStates }
                let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
                let state = { state with AppState = Auth authState }
                state, Cmd.batch [ chatCmd ; state |> writePreferencesCmd ], true
        else state, Cmd.none, true
    | PageInput (UPageInput NewsInput), None, false ->
        let state, cmd = state |> shouldNeverHappen "Unexpected NewsInput -> NYI"
        state, cmd, false
    | PageInput (UPageInput SquadsInput), None, false ->
        let state, cmd = state |> shouldNeverHappen "Unexpected SquadsInput -> NYI"
        state, cmd, false
    | PageInput (APageInput DraftsInput), None, false ->
        let state, cmd = state |> shouldNeverHappen "Unexpected DraftsInput -> NYI"
        state, cmd, false
    | PageInput (APageInput (ChatInput (Chat.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (APageInput (ChatInput ShowMarkdownSyntaxModal)), None, false ->
        { state with StaticModal = MarkdownSyntax |> Some }, Cmd.none, true
    | PageInput (APageInput (ChatInput (SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (APageInput (ChatInput chatInput)), _, _ ->
        match authState.AuthPageStates.ChatState with
        | Some chatState ->
            let chatState, chatCmd, isUserNonApiActivity = chatState |> Chat.State.transition chatInput
            let authPageStates = { authState.AuthPageStates with ChatState = chatState |> Some }
            let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
            { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, chatCmd, isUserNonApiActivity
        | None ->
            let state, cmd = state |> shouldNeverHappen "Unexpected ChatInput when ChatState is None"
            state, cmd, false
    | PageInput (APageInput UserAdministrationInput), None, false ->
        let state, cmd = state |> shouldNeverHappen "Unexpected UserAdministrationInput -> NYI"
        state, cmd, false
    | ShowChangePasswordModal, None, false ->
        let authState = { authState with ChangePasswordState = defaultChangePasswordState None None |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput (NewPasswordTextChanged newPasswordText), Some changePasswordState, false ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace changePasswordState.ConfirmPasswordText then changePasswordState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password changePasswordState.ConfirmPasswordText)
        let changePasswordState = { changePasswordState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput (ConfirmPasswordTextChanged confirmPasswordText), Some changePasswordState, false ->
        let confirmPasswordErrorText = validateConfirmPassword (Password changePasswordState.NewPasswordText) (Password confirmPasswordText)
        let changePasswordState = { changePasswordState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput ChangePassword, Some changePasswordState, false -> // note: assume no need to validate NewPasswordText or ConfirmPasswordText (i.e. because App.Render.renderChangePasswordModal will ensure that ChangePassword can only be dispatched when valid)
        let changePasswordState = { changePasswordState with ChangePasswordStatus = ChangePasswordPending |> Some }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        let changePasswordCmdParams = authState.AuthUser.Rvn, Password (changePasswordState.NewPasswordText.Trim ())
        let authState, cmd = changePasswordCmdParams |> ChangePasswordCmd |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | ChangePasswordInput CancelChangePassword, Some changePasswordState, false ->
        match changePasswordState.MustChangePasswordReason, changePasswordState.ChangePasswordStatus with
        | Some _, _ ->
            let state, cmd = state |> shouldNeverHappen "Unexpected CancelChangePassword when MustChangePasswordReason is Some"
            state, cmd, false
        | None, Some ChangePasswordPending ->
            let state, cmd = state |> shouldNeverHappen "Unexpected CancelChangePassword when ChangePasswordPending"
            state, cmd, false
        | None, Some _ | None, None ->
            let authState = { authState with ChangePasswordState = None }
            { state with AppState = Auth authState }, Cmd.none, true
    | SignOut, None, false ->
        let authState, cmd = SignOutCmd |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth { authState with SigningOut = true } }, cmd, false
    | _, _, false ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AuthInput when not SigningOut and ChangePasswordState is %A -> %A" authState.ChangePasswordState authInput)
        state, cmd, false
    | _, _, true ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AuthInput when SigningOut and ChangePasswordState is %A -> %A" authState.ChangePasswordState authInput)
        state, cmd, false

let private handleAppInput appInput state =
    match appInput, state.AppState with
    | ReadingPreferencesInput result, ReadingPreferences ->
        let state, cmd = state |> handleReadingPreferencesInput result
        state, cmd, false
    | ConnectingInput ws, Connecting _ ->
        let state, cmd = state |> handleConnectingInput ws
        state, cmd, false
    | UnauthInput unauthInput, Unauth unauthState ->
        let state, cmd = state |> handleUnauthInput unauthInput unauthState
        state, cmd, false
    | AuthInput authInput, Auth authState ->
        state |> handleAuthInput authInput authState
    | _, _ ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AppInput when %A -> %A" state.AppState appInput)
        state, cmd, false

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input with
#if TICK
        | Tick ->
            // Note: Only sending Wiff messages to server to see if this resolves issue with WebSocket "timeouts" for MS Edge (only seen with Azure, not dev-server).
            let lastWiff, cmd =
                match state.ConnectionState with
                | Connected connectedState ->
                    let now = DateTimeOffset.UtcNow
                    if (now.DateTime - state.LastWiff.DateTime).TotalSeconds * 1.<second> >= WIFF_INTERVAL then now, Wiff |> sendMsg connectedState.Ws
                    else state.LastWiff, Cmd.none
                | NotConnected | InitializingConnection _ -> state.LastWiff, Cmd.none
            { state with Ticks = state.Ticks + 1<tick> ; LastWiff = lastWiff }, cmd
#endif
        | AddNotificationMessage notificationMessage ->
            state |> addNotificationMessage notificationMessage, Cmd.none, false
        | DismissNotificationMessage notificationId -> // note: silently ignore unknown notificationId
            { state with NotificationMessages = state.NotificationMessages |> removeNotificationMessage notificationId }, Cmd.none, true
        | ToggleTheme ->
            let state = { state with UseDefaultTheme = (state.UseDefaultTheme |> not) }
            setBodyClass state.UseDefaultTheme
            state, state |> writePreferencesCmd, true
        | ToggleNavbarBurger ->
            { state with NavbarBurgerIsActive = (state.NavbarBurgerIsActive |> not) }, Cmd.none, true
        | ShowStaticModal staticModal ->
            { state with StaticModal = staticModal |> Some }, Cmd.none, true
        | HideStaticModal ->
            { state with StaticModal = None }, Cmd.none, true
        | WritePreferencesResult (Ok _) ->
            state, Cmd.none, false
        | WritePreferencesResult (Error exn) -> // note: no need for toast
            let state, cmd = state |> addDebugError (sprintf "WritePreferencesResult -> %s" exn.Message) None
            state, cmd, false
        | WsError wsError ->
            let state, cmd = state |> handleWsError wsError
            state, cmd, false
        | HandleServerMsg serverMsg ->
            let state, cmd = state |> handleServerMsg serverMsg
            state, cmd, false
        | AppInput appInput ->
            state |> handleAppInput appInput
    let appState, userNonApiActivityCmd =
        match state.AppState, isUserNonApiActivity with
        | Auth authState, true ->
            let authState, userNonApiActivityCmd =
                if (DateTimeOffset.UtcNow - authState.LastUserActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE then authState, Cmd.none
                else UserNonApiActivity |> sendAuthMsgCmd state.ConnectionState authState // note: updates authState.LastUserActivity
            Auth authState, userNonApiActivityCmd
        | _, _ ->
            state.AppState, Cmd.none
    { state with AppState = appState }, Cmd.batch [ cmd ; userNonApiActivityCmd ]
