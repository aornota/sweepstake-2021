module Aornota.UI.Common.LocalStorage

open Aornota.Common.Json

open Fable.Import

type Key = | Key of key : string

let readJson (Key key) = unbox (Browser.localStorage.getItem key) |> Option.map (string >> Json)
let writeJson (Key key) (Json json) = Browser.localStorage.setItem (key, json)
let delete (Key key) = Browser.localStorage.removeItem key
