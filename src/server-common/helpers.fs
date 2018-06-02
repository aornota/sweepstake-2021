module Aornota.Server.Common.Helpers

open System

let thingAsync thing = async { return thing }

let discardOk result = result |> Result.map ignore

let dateTimeOffsetUtc (year, month, day, hour, minute) = DateTime (year, month, day, hour, minute, 00, DateTimeKind.Utc) |> DateTimeOffset
