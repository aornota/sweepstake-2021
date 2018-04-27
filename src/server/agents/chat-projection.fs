module Aornota.Sweepstake2018.Server.Agents.ChatProjection

(*open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User*)

type private ChatProjectionInput = | ToDo

// TODO-NMB-HIGH: agent [MailboxProcessor] | broadcaster.Subscribe (UserEventWritten _ | UserSignedIn | UserApi | UserSignedOut | ...) | ...
