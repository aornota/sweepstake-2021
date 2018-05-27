module Aornota.Sweepstake2018.Server.Agents.Projections.Squads

// Note: Squads agent broadcasts TODO-NMB-HIGH... - and subscribes to TODO-NMB-HIGH...

//open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
//open Aornota.Sweepstake2018.Server.Events.SquadEvents
//open Aornota.Sweepstake2018.Server.Signal

type private SquadsInput = | ToDo | Temp

let private log category = (Projection Squads, category) |> consoleLogger.Log

// TODO-NMB-HIGH: Squads agent | broadcaster.Subscribe (SquadsRead | SquadEventWritten [...] | Disconnected | ...) | ...

// TODO-NMB-HIGH: Separate (mutually-recursive) states, e.g. ignore other broadcasts until OnSquadsRead (cf. OnSquadsEventsRead for Squads [entity] agent)? with Reset [as well as Start]?...

// TODO-NMB-HIGH: Will need to keep track of connectionIds to send deltas...

let squads = ()
