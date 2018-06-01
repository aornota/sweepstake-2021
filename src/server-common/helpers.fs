module Aornota.Server.Common.Helpers

let thingAsync thing = async { return thing }

let discardOk result = result |> Result.map ignore
