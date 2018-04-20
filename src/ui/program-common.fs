module Aornota.Sweepstake2018.UI.Program.Common

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages

open Aornota.UI.Common.Notifications

open System

module Brw = Fable.Import.Browser

type UnauthPage =
    | News
    | Squads

type AuthPage =
    | Drafts
    | ChatPage

type Page =
    | UnauthPage of unauthPage : UnauthPage
    | AuthPage of authPage: AuthPage

type Preferences = {
    UseDefaultTheme : bool
    SessionId : SessionId
    LastPage : Page option
    Jwt : Jwt option }

type StaticModal =
    | ScoringSystem
    | Payouts
    | MarkdownSyntax

type UiWsError =
    | WsOnError of wsApiUrl : string
    | SendWsNotOpenError of uiWsApi : UiWsApi
    | SendWsOtherError of uiWsApi : UiWsApi * errorText : string
    | DeserializeServerWsApiError of errorText : string

type UnauthPageInput =
    | NewsInput
    | SquadsInput

type SignInInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn
    | CancelSignIn

type UnauthInput =
    | ShowUnauthPage of unauthPage : UnauthPage
    | UnauthPageInput of unauthPageInput : UnauthPageInput
    | ShowSignInModal
    | SignInInput of signInInput : SignInInput

type AuthPageInput =
    | DraftsInput
    | ChatInput of chatInput : Chat.Common.Input

type PageInput =
    | UPageInput of unauthPageInput : UnauthPageInput
    | APageInput of authPageInput : AuthPageInput

type AuthInput =
    | ShowPage of page : Page
    | PageInput of pageInput : PageInput
    | ChangePassword
    | SignOut
    | UserAdministration

type AppInput =
    | ReadingPreferencesInput of result : Result<Preferences option, exn>
    | ConnectingInput of ws : Brw.WebSocket
    | UnauthInput of unauthInput : UnauthInput
    | AuthInput of authInput : AuthInput

type Input =
#if TICK
    | Tick
#endif
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | DismissNotificationMessage of notificationId : NotificationId
    | ToggleTheme
    | ToggleNavbarBurger
    | ShowStaticModal of staticModal : StaticModal
    | HideStaticModal
    | WritePreferencesResult of result : Result<unit, exn>
    | OnUiWsError of uiWsError : UiWsError
    | HandleServerWsApi of serverWsApi : ServerWsApi
    | AppInput of appInput : AppInput

type ToDo = unit

type SignInStatus =
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
    SignInStatus : SignInStatus option }

type UnauthPageStates = {
    NewsState : ToDo
    SquadsState : ToDo }

type UnauthState = {
    CurrentUnauthPage : UnauthPage
    UnauthPageStates : UnauthPageStates
    SignInState : SignInState option }

type AuthPageStates = {
    DraftsState : ToDo
    ChatState : Chat.Common.State option }

type AuthState = {
    AuthUser : AuthUser
    CurrentPage : Page
    UnauthPageStates : UnauthPageStates
    AuthPageStates : AuthPageStates
    SigningOut : bool }

type AppState =
    | ReadingPreferences
    | Connecting of jwt : Jwt option * lastPage : Page option
    | ServiceUnavailable
    | AutomaticallySigningIn of jwt : Jwt * lastPage : Page option
    | Unauth of unauthState : UnauthState
    | Auth of authState : AuthState

type State = {
    Ticks : int<tick> // note: will only be updated when TICK (see webpack.config.js) is defined
    NotificationMessages : NotificationMessage list
    UseDefaultTheme : bool
    SessionId : SessionId
    NavbarBurgerIsActive : bool
    StaticModal : StaticModal option
    Ws : Brw.WebSocket option
    AppState : AppState }

let [<Literal>] SWEEPSTAKE_2018 = "sweepstake 2018 (α)"

let validateUserNameText userNameText = if String.IsNullOrWhiteSpace userNameText then Some "Username must not be blank" else None
let validatePasswordText passwordText = if String.IsNullOrWhiteSpace passwordText then Some "Password must not be blank" else None
