module Aornota.Sweepstake2021.Ui.Program.Run

#if TICK
open Aornota.Sweepstake2021.Common.UnitsOfMeasure
#endif
open Aornota.Sweepstake2021.Ui.Common.Marked
open Aornota.Sweepstake2021.Ui.Program.Common
open Aornota.Sweepstake2021.Ui.Program.Render
open Aornota.Sweepstake2021.Ui.Program.State

open Elmish
#if DEBUG
open Elmish.Debug
#endif
open Elmish.React
#if HMR
open Elmish.HMR // note: needs to be last open Elmish.Xyz (see https://elmish.github.io/hmr/)
#endif

open Fable.Core.JsInterop

#if TICK
let [<Literal>] private SECONDS_PER_TICK = 1<second/tick>

let private ticker dispatch =
    let secondsPerTick = if SECONDS_PER_TICK > 0<second/tick> then SECONDS_PER_TICK else 1<second/tick>
    let millisecondsPerTick = ((float secondsPerTick) * 1.<second/tick>) * MILLISECONDS_PER_SECOND
    Browser.Dom.window.setInterval (fun _ ->
        dispatch Tick
    , int millisecondsPerTick) |> ignore

let private tickSubscription (_:State) = ticker |> Cmd.ofSub
#endif

Globals.marked.setOptions (unbox (createObj [ "sanitize" ==> true ])) |> ignore // note: "sanitize" ensures Html rendered as text

Program.mkProgram initialize transition render
#if TICK
|> Program.withSubscription tickSubscription
#endif
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app" // i.e. <div id="elmish-app"> in index.html
#if DEBUG
// TEMP-NMB: Commented-out - else get Cannot generate auto encoder for Browser.Types.WebSocket errors...|> Program.withDebugger
#endif
|> Program.run
