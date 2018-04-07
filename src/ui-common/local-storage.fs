module Aornota.UI.Common.LocalStorage

open Fable.Import

let readJson key = Option.map string (unbox (Browser.localStorage.getItem key))
let writeJson key json = Browser.localStorage.setItem (key, json)
let delete key = Browser.localStorage.removeItem key
