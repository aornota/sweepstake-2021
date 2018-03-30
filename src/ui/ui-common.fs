module Aornota.Sweepstake2018.UI.Common

open Aornota.Sweepstake2018.Shared

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Theme.Dark
open Aornota.UI.Theme.Default

type Preferences = { UseDefaultTheme : bool }

type Input =
    | DismissDebugMessage of debugId : DebugId
    | PreferencesRead of preferences : Preferences option
    | ErrorReadingPreferences of exn : exn
    | PreferencesWritten
    | ErrorWritingPreferences of exn : exn
    | ToggleTheme
    | ToggleNavbarBurger
    | InitializeCounter of Result<Counter, exn>
    | IncrementCounter
    | DecrementCounter

type Status =
    | ReadingPreferences
    | InitializingCounter
    | Ready

type State = {
    DebugMessages : DebugMessage list
    Status : Status
    UseDefaultTheme : bool
    NavbarBurgerIsActive : bool
    Counter : Counter }

let [<Literal>] SWEEPSTAKE_2018 = "sweepstake 2018 (pre-α)"

let getTheme useDefaultTheme = if useDefaultTheme then themeDefault else themeDark
