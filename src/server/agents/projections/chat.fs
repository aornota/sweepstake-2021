module Aornota.Sweepstake2018.Server.Agents.Projections.Chat

// Note: Chat agent broadcasts TODO-NMB-HIGH... - and subscribes to TODO-NMB-HIGH...

//open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
//open Aornota.Sweepstake2018.Server.Events.Event
//open Aornota.Sweepstake2018.Server.Events.User

type private ChatInput = | ToDo

let private log category = (Projection Chat, category) |> consoleLogger.Log

// TODO-NMB-HIGH: Chat agent | broadcaster.Subscribe (UsersRead | UserEventWritten [only UserCreated and UserTypeChanged?] | UserSignedIn | UserApi | UserSignedOut | Disconnected | ConnectionsSignedOut |...) | ...

// TODO-NMB-HIGH: Separate (mutually-recursive) states, e.g. ignore other broadcasts until OnUsersRead (cf. OnUsersEventsRead for Users agent)? with Reset [as well as Start]?...

// TODO-NMB-HIGH: Will need to keep track of connectionIds to send deltas...

let chat = ()
