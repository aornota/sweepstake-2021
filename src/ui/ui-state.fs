module Aornota.Sweepstake2018.UI.State

open Aornota.Sweepstake2018.Shared

open Aornota.Sweepstake2018.UI.Common

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Common.LocalStorage
open Aornota.UI.Theme.Common

open Elmish
open Elmish.Toastr

open Fable.Core.JsInterop
open Fable.Import
open Fable.PowerPack.Fetch

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2018-ui-app-preferences"

let private setBodyClass useDefaultTheme = Browser.document.body.className <- getThemeClass (getTheme useDefaultTheme).ThemeClass

let private readPreferencesCmd =
    let readPreferences () = async { return Option.map ofJson<Preferences> (readJson APP_PREFERENCES_KEY) }
    Cmd.ofAsync readPreferences () PreferencesRead ErrorReadingPreferences

let private writePreferencesCmd state =
    let writePreferences preferences = async { do writeJson APP_PREFERENCES_KEY (toJson preferences) }
    let preferences = { UseDefaultTheme = state.UseDefaultTheme }
    Cmd.ofAsync writePreferences preferences (fun _ -> PreferencesWritten) ErrorWritingPreferences       

let private initializeCounterCmd =
    Cmd.ofPromise (fetchAs<Counter> "/api/initializeCounter") [] (Ok >> InitializeCounter) (Error >> InitializeCounter)

let private counterInitializedToastCmd fromServer counter : Cmd<_> =
    Toastr.message (sprintf "Counter initialized to %i %s" counter (if fromServer then "(from server)" else "(default value)"))
    |> Toastr.position TopRight
    |> Toastr.timeout 3000
    |> Toastr.hideEasing Easing.Swing
    |> Toastr.showCloseButton
    |> Toastr.success
    
let private counterChangedCmd incremented counter : Cmd<_> =
    Toastr.message (sprintf "Counter %s to %i" (if incremented then "incremented" else "decremented") counter)
    |> Toastr.position TopRight
    |> Toastr.timeout 3000
    |> Toastr.hideEasing Easing.Swing
    |> Toastr.showCloseButton
    |> Toastr.info
    
let initialize () =
    let state = {
        DebugMessages = []
        Status = ReadingPreferences
        UseDefaultTheme = true
        NavbarBurgerIsActive = false
        Counter = 0 }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let transition input state =
    match input with
    | DismissDebugMessage debugId ->
        let state = { state with DebugMessages = state.DebugMessages |> removeDebugMessage debugId }
        state, Cmd.none
    | PreferencesRead (Some preferences) ->
        let state = { state with Status = InitializingCounter ; UseDefaultTheme = preferences.UseDefaultTheme }
        setBodyClass state.UseDefaultTheme
        state, initializeCounterCmd
    | PreferencesRead None ->
        let state = { state with Status = InitializingCounter }
        state, initializeCounterCmd
    | ErrorReadingPreferences exn ->
        let state = { state with DebugMessages = debugMessage (sprintf "Error reading preferences from local storage -> %s" exn.Message) :: state.DebugMessages }
        state, Cmd.ofMsg (PreferencesRead None)
    | PreferencesWritten ->
        state, Cmd.none
    | ErrorWritingPreferences exn ->
        let state = { state with DebugMessages = debugMessage (sprintf "Error writing preferences to local storage -> %s" exn.Message) :: state.DebugMessages }
        state, Cmd.none
    | ToggleTheme ->
        let state = { state with UseDefaultTheme = not state.UseDefaultTheme }
        setBodyClass state.UseDefaultTheme
        state, writePreferencesCmd state
    | ToggleNavbarBurger ->
        let state = { state with NavbarBurgerIsActive = not state.NavbarBurgerIsActive }
        state, Cmd.none
    | InitializeCounter (Ok counter) ->
        let state = { state with Status = Ready ; Counter = counter }
        state, counterInitializedToastCmd true state.Counter
    | InitializeCounter (Error exn) ->
        let state = { state with DebugMessages = debugMessage (sprintf "Error initializing counter from server -> %s" exn.Message) :: state.DebugMessages ; Status = Ready }
        state, counterInitializedToastCmd false state.Counter
    | IncrementCounter ->
        let state = { state with Counter = state.Counter + 1 }
        state, counterChangedCmd true state.Counter
    | DecrementCounter ->
        let state = { state with Counter = state.Counter - 1 }
        state, counterChangedCmd false state.Counter
