module Aornota.Sweepstake2018.Server.Agents.Users

(*open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User*)

type private UsersInput = | ToDo

// TODO-NMB-HIGH: agent [MailboxProcessor] | broadcaster.Broadcast (UsersRead _ | ...) | broadcaster.Subscribe (UserEventsRead _ | [UserEventWritten _ ?] | ...) | ...

// TODO-NMB-HIGH: Validate that changes (i.e. ChangePassword[Cmd] | ResetPassword[Cmd] | ChangeUserType[Cmd] | &c.) are made to "current" revision?...

// TODO-NMB-HIGH: When handling ChamgePassword[Cmd] (&c.), "require" specific IAuthznToken/s?...
