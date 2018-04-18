module Aornota.Sweepstake2018.UI.Program.Common

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages

open Aornota.UI.Common.Notifications

open System

module Brw = Fable.Import.Browser

type UnauthenticatedPage = // TODO-NMB-MEDIUM: More (e.g. Standings | Fixtures | Results | ...)...
    | News
    | Squads

type AuthenticatedPage = // TODO-NMB-MEDIUM: More...
    | Drafts
    | ChatPage

type Page =
    | UnauthenticatedPage of unauthenticatedPage : UnauthenticatedPage
    | AuthenticatedPage of authenticatedPage : AuthenticatedPage

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

type SignInInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn
    | CancelSignIn

type UnauthenticatedInput = // TODO-NMB-MEDIUM: More (e.g. Standings[U] | Fixtures[U] | Results[U] | ...)...
    | ShowSignIn
    | ShowUnauthenticatedPage of unauthenticatedPage : UnauthenticatedPage
    | NewsInputU
    | SquadsInputU
    | SignInInput of signInInput : SignInInput

type AuthenticatedInput = // TODO-NMB-MEDIUM: More (e.g. Standings[A] | Fixtures[A] | Results[A] | ...)...
    | ShowPage of page : Page
    | NewsInputA
    | SquadsInputA
    | DraftsInput
    | ChatInput of chatInput : Chat.Common.Input
    | SignOut
    | CancelSignOut
    | ChangePassword

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
    | ShowStaticModal of staticModal : StaticModal
    | HideStaticModal
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

type UnauthenticatedState = { // TODO-NMB-MEDIUM: More (e.g. Standings | Fixtures | Results | ...)...
    CurrentPage : UnauthenticatedPage
    NewsState : ToDo
    SquadsState : ToDo
    SignInState : SignInState option }

type AuthenticatedState = { // TODO-NMB-MEDIUM: More (e.g. Standings | Fixtures | Results | ...)...
    AuthenticatedUser : AuthenticatedUser
    CurrentPage : Page
    NewsState : ToDo
    SquadsState : ToDo
    DraftsState : ToDo
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
