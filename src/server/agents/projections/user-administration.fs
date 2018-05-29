module Aornota.Sweepstake2018.Server.Agents.Projections.UserAdministration

(* Broadcasts: TBC...
   Subscribes: TBC... *)

//open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
//open Aornota.Sweepstake2018.Server.Events.UserEvents
//open Aornota.Sweepstake2018.Server.Signal

type private UserAdministrationInput = | ToDo

let private log category = (Projection UserAdministration, category) |> consoleLogger.Log

// TODO-NMB-HIGH: UserAdministration agent | broadcaster.Subscribe (UsersRead | UserEventWritten [only UserCreated and UserTypeChanged?] | Disconnected | ConnectionsSignedOut | ...) | ...

// TODO-NMB-HIGH: Separate (mutually-recursive) states, e.g. ignore other broadcasts until OnUsersRead (cf. OnUsersEventsRead for Users agent)? with Reset [as well as Start]?...

// TODO-NMB-HIGH: Will need to keep track of connectionIds to send deltas...

let userAdministration = ()
