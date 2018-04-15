module Aornota.Sweepstake2018.UI.Program.Common

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.UI.Common.Notifications

open System

open Elmish

module Brw = Fable.Import.Browser

type Preferences = { 
    UseDefaultTheme : bool
    SessionId : SessionId
    Jwt : Jwt option }

type UiWsError =
    | WsOnError of wsApiUrl : string
    | SendWsNotOpenError of uiWsApi : UiWsApi
    | SendWsOtherError of uiWsApi : UiWsApi * errorText : string
    | DeserializeServerWsApiError of errorText : string

type UnauthenticatedInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn

type AuthenticatedInput =
    | ChatInput of chatInput : Input
    | SignOut

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

type Status =
    | Pending
    | Failed of errorText : string

type UnauthenticatedState = {
    SendUiUnauthenticatedWsApi : (UiUnauthenticatedWsApi -> Cmd<Input>) // TODO-NMB-HIGH: Rethink this, e.g. since functions do not play well with lazyView?...
    UserNameKey : Guid
    UserNameText : string
    UserNameErrorText : string option
    PasswordKey : Guid
    PasswordText : string
    PasswordErrorText : string option
    FocusPassword : bool
    SignInStatus : Status option }

type Page =
    | ChatPage

type AuthenticatedState = {
    SendUiWsApi : (UiWsApi -> Cmd<Input>) // TODO-NMB-HIGH: Rethink this, e.g. since functions do not play well with lazyView?...
    AuthenticatedUser : AuthenticatedUser
    Page : Page
    ChatState : State // TODO-NMB-MEDIUM: Should this be ChatState option, i.e. only initialize "on demand" (rather than automatically "on connection")?...
    SignOutStatus : Status option }

type AppState =
    | ReadingPreferences
    | Connecting of jwt : Jwt option
    | ServiceUnavailable
    | AutomaticallySigningIn of jwt : Jwt
    | Unauthenticated of unauthenticatedState : UnauthenticatedState
    | Authenticated of authenticatedState : AuthenticatedState

type State = {
    Ticks : int<tick> // note: will only be updated when TICK defined
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
