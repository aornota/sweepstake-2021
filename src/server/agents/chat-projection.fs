module Aornota.Sweepstake2018.Server.Agents.ChatProjection

open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

type private ChatProjectionInput = | ToDo

let private log category = consoleLogger.Log (ChatProjection, category)

// TODO-NMB-HIGH: ChatProjection agent | broadcaster.Subscribe (UsersRead _ | UserEventWritten _ | UserSignedIn _ | UserApi _ | UserSignedOut _ | ...) | ...

// TODO-NMB-HIGH: Separate (mutually-recursive) states (with inbox.Scan (...) &c.), e.g. ignore other broadcasts until UsersRead (cf. Users agent)?...

let chatProjection = ()

let ensureInstantiated () = () // note: ChatProjection agent [chatProjection] is static - so will only be instantiated when ChatProjection module (effectively a static class) is first referenced
