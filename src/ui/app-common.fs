module Aornota.Sweepstake2018.UI.App.Common

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui
open Aornota.Sweepstake2018.UI.Pages.Chat.Common

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Theme.Dark
open Aornota.UI.Theme.Default

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

type ReadingPreferencesInput =
    | ReadPreferencesResult of result : Result<Preferences option, exn>

type ConnectingInput<'a> =
    | WsOnOpen of ws : Brw.WebSocket

type UnauthenticatedInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn

type AuthenticatedInput =
    | ChatInput of chatInput : ChatInput
    | SignOut

type AppInput<'a> =
    | ReadingPreferencesInput of readingPreferencesInput : ReadingPreferencesInput
    | ConnectingInput of connectingInput : ConnectingInput<'a>
    | UnauthenticatedInput of unauthenticatedInput : UnauthenticatedInput
    | AuthenticatedInput of authenticatedInput : AuthenticatedInput

type UiInput =
    | AddDebugMessageApp of message : string
    | DismissDebugMessage of debugId : DebugId
    | ToggleTheme
    | ToggleNavbarBurger
    | WritePreferencesResult of result : Result<unit, exn>
    | OnUiWsError of uiWsError : UiWsError
    | HandleServerWsApi of serverWsApi : ServerWsApi
    | AppInput of appInput : AppInput<UiInput>

type Status =
    | Pending
    | Failed of errorText : string

type UnauthenticatedState = {
    SendUiUnauthenticatedWsApi : (UiUnauthenticatedWsApi -> Cmd<UiInput>)
    UserNameKey : Guid
    UserNameText : string
    UserNameErrorText : string option
    PasswordKey : Guid
    PasswordText : string
    PasswordErrorText : string option
    SignInStatus : Status option }

type Page =
    | ChatPage

type AuthenticatedState = {
    SendUiWsApi : (UiWsApi -> Cmd<UiInput>)
    AuthenticatedUser : AuthenticatedUser
    Page : Page
    ChatState : ChatState // TODO-NMB: Should this be ChatState option, i.e. only initialize "on demand" (rather than automatically "on connection")?...
    SignOutStatus : Status option }

type AppState =
    | ReadingPreferences
    | Connecting of jwt : Jwt option
    | ServiceUnavailable
    | AutomaticallySigningIn of jwt : Jwt
    | Unauthenticated of unauthenticatedState : UnauthenticatedState
    | Authenticated of authenticatedState : AuthenticatedState

type UiState = {
    DebugMessages : DebugMessage list
    UseDefaultTheme : bool
    SessionId : SessionId
    NavbarBurgerIsActive : bool
    Ws : Brw.WebSocket option
    AppState : AppState }

let [<Literal>] SWEEPSTAKE_2018 = "sweepstake 2018 (pre-α prototype)"

let getTheme useDefaultTheme = if useDefaultTheme then themeDefault else themeDark

let validateUserNameText userNameText = if String.IsNullOrWhiteSpace userNameText then Some "Username must not be blank" else None
let validatePasswordText passwordText = 
    // TEMP-NMB: Allow a blank password...
    //if String.IsNullOrWhiteSpace passwordText then None else None
    // ...or not...
    if String.IsNullOrWhiteSpace passwordText then Some "Password must not be blank" else None
    // ...NMB-TEMP
