module Aornota.Server.Common.Helpers

let thingAsync thing = async { return thing }

let discardOk result = result |> Result.map ignore
let tupleError thing result = match result with | Ok ok -> Ok ok | Error error -> Error (thing, error)