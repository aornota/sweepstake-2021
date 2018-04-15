module Aornota.UI.Theme.Shared

open Aornota.UI.Theme.Default
open Aornota.UI.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeDefault else themeDark
