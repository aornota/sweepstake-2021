module Aornota.UI.Common.LocalStorage

open Aornota.Common.Json

open Fable.Import

type Key = | Key of key : string

let readJson (Key key) = key |> Browser.localStorage.getItem |> unbox |> Option.map (string >> Json)
let writeJson (Key key) (Json json) = (key, json) |> Browser.localStorage.setItem
let delete (Key key) = key |> Browser.localStorage.removeItem
