module Aornota.Sweepstake2018.UI.App

open Elmish
#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif
open Elmish.React
 
Program.mkProgram State.initialize State.transition Render.render
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app" // note: needs to match id of div in index.html
#if DEBUG
//|> Program.withDebugger
#endif
|> Program.run
