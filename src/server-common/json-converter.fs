module Aornota.Server.Common.JsonConverter

open Aornota.Common.Json

open Newtonsoft.Json

let private jsonConverter = Fable.JsonConverter () :> JsonConverter

let toJson value = Json (JsonConvert.SerializeObject (value, [| jsonConverter |]))

let ofJson<'a> (Json json) : 'a = JsonConvert.DeserializeObject<'a> (json, [| jsonConverter |])
