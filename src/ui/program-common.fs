module Aornota.Sweepstake2018.UI.Program.Common

open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages

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
    User : (UserName * Jwt) option }

type StaticModal =
    | ScoringSystem
    | Payouts
    | MarkdownSyntax

type WsError =
    | WsOnError of wsApiUrl : string
    | SendMsgWsNotOpenError of uiMsg : UiMsg
    | SendMsgOtherError of uiMsg : UiMsg * errorText : string
    | DeserializeServerMsgError of errorText : string

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
    | WsError of wsError : WsError
    | HandleServerMsg of serverMsg : ServerMsg
    | AppInput of appInput : AppInput

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
    | Connecting of user : (UserName * Jwt) option * lastPage : Page option
    | ServiceUnavailable
    | AutomaticallySigningIn of user : (UserName * Jwt) * lastPage : Page option
    | Unauth of unauthState : UnauthState
    | Auth of authState : AuthState

type State = {
    Ticks : int<tick> // note: will only be updated when TICK (see webpack.config.js) is defined
    NotificationMessages : NotificationMessage list
    UseDefaultTheme : bool
    SessionId : SessionId
    NavbarBurgerIsActive : bool
    StaticModal : StaticModal option
    Ws : Brw.WebSocket option // TODO-NMB-MEDIUM: Switch to using Fable.Websockets.Elmish?...
    AppState : AppState }

let [<Literal>] SWEEPSTAKE_2018 = "sweepstake 2018 (α)"
