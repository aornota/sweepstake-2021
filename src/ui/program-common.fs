module Aornota.Sweepstake2018.UI.Program.Common

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages

open Aornota.UI.Common.Notifications

open System

module Brw = Fable.Import.Browser

type UnauthenticatedPage =
    | ToDoUP

type AuthenticatedPage =
    | ToDoAP
    | ChatPage

type Page =
    | UnauthenticatedPage of unauthenticatedPage : UnauthenticatedPage
    | AuthenticatedPage of authenticatedPage : AuthenticatedPage

type Preferences = {
    UseDefaultTheme : bool
    SessionId : SessionId
    LastPage : Page option
    Jwt : Jwt option }

type UiWsError =
    | WsOnError of wsApiUrl : string
    | SendWsNotOpenError of uiWsApi : UiWsApi
    | SendWsOtherError of uiWsApi : UiWsApi * errorText : string
    | DeserializeServerWsApiError of errorText : string

type SignInInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn
    | CancelSignIn

type UnauthenticatedInput =
    | ShowSignIn
    | ShowUnauthenticatedPage of unauthenticatedPage : UnauthenticatedPage
    | ToDoUPInputUI
    | SignInInput of signInInput : SignInInput

type AuthenticatedInput =
    | ShowPage of page : Page
    | ToDoUPInputAI
    | ToDoAPInputAI
    | ChatInput of chatInput : Chat.Common.Input
    | SignOut
    | CancelSignOut

type AppInput =
    | ReadingPreferencesInput of result : Result<Preferences option, exn>
    | ConnectingInput of ws : Brw.WebSocket
    | UnauthenticatedInput of unauthenticatedInput : UnauthenticatedInput
    | AuthenticatedInput of authenticatedInput : AuthenticatedInput

type Input =
#if TICK
    | Tick
#endif
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | DismissNotificationMessage of notificationId : NotificationId
    | ToggleTheme
    | ToggleNavbarBurger
    | WritePreferencesResult of result : Result<unit, exn>
    | OnUiWsError of uiWsError : UiWsError
    | HandleServerWsApi of serverWsApi : ServerWsApi
    | AppInput of appInput : AppInput

type ToDo = unit

type Status =
    | Pending
    | Failed of errorText : string

type SignInState = {
    UserNameKey : Guid
    UserNameText : string
    UserNameErrorText : string option
    PasswordKey : Guid
    PasswordText : string
    PasswordErrorText : string option
    FocusPassword : bool
    SignInStatus : Status option }

type UnauthenticatedState = {
    CurrentPage : UnauthenticatedPage
    ToDoUPState : ToDo
    SignInState : SignInState option }

type AuthenticatedState = {
    AuthenticatedUser : AuthenticatedUser
    CurrentPage : Page
    ToDoUPState : ToDo
    ToDoAPState : ToDo
    ChatState : Chat.Common.State option
    SignOutStatus : Status option }

type AppState =
    | ReadingPreferences
    | Connecting of jwt : Jwt option * lastPage : Page option
    | ServiceUnavailable
    | AutomaticallySigningIn of jwt : Jwt * lastPage : Page option
    | Unauthenticated of unauthenticatedState : UnauthenticatedState
    | Authenticated of authenticatedState : AuthenticatedState

type State = {
    Ticks : int<tick> // note: will only be updated when TICK defined (see webpack.config.js)
    NotificationMessages : NotificationMessage list
    UseDefaultTheme : bool
    SessionId : SessionId
    NavbarBurgerIsActive : bool
    Ws : Brw.WebSocket option
    AppState : AppState }

let [<Literal>] SWEEPSTAKE_2018 = "sweepstake 2018 (α)"

let validateUserNameText userNameText = if String.IsNullOrWhiteSpace userNameText then Some "Username must not be blank" else None
let validatePasswordText passwordText = 
    // TEMP-NMB: Allow a blank password...
    //if String.IsNullOrWhiteSpace passwordText then None else None
    // ...or not...
    if String.IsNullOrWhiteSpace passwordText then Some "Password must not be blank" else None
    // ...NMB-TEMP
