module Aornota.Sweepstake2021.Ui.Theme.Shared

open Aornota.Sweepstake2021.Ui.Theme.Light
open Aornota.Sweepstake2021.Ui.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeLight else themeDark
