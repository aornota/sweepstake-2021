﻿module Aornota.Sweepstake2018.Server.Host

open Aornota.Common.UnitsOfMeasure

open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.Connections
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Agents.Ticker
open Aornota.Sweepstake2018.Server.DefaultData
open Aornota.Sweepstake2018.Server.WsMiddleware

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

let [<Literal>] private SECONDS_PER_TICK = 1<second/tick>

let private log category = consoleLogger.Log (Host, category)

let private uiPath = // note: relative to current [server] directory, "ui" folder might be sibling (e.g. when running with webpack-dev-server) or child (e.g. once published)
    let uiPath = Path.Combine ("..", "ui") |> Path.GetFullPath
    if Directory.Exists uiPath then uiPath else Path.GetFullPath "ui"

let private configureApp (app:IApplicationBuilder) =
    app.UseStaticFiles () |> ignore
    app.UseWebSockets () |> ignore
    app.UseMiddleware<WsMiddleware> () |> ignore

let private configureServices (services:IServiceCollection) = services.AddGiraffe () |> ignore

let private builder = WebHost.CreateDefaultBuilder ()

builder.UseWebRoot uiPath |> ignore
builder.UseContentRoot uiPath |> ignore
builder.Configure (Action<IApplicationBuilder> configureApp) |> ignore
// TODO-NMB-MEDIUM: Suppress ASP.Net Core logging (since can get mixed up with ConsoleLogger output, i.e. since Console not thread-safe)?... builder.ConfigureLogging (...) |> ignore
builder.ConfigureServices configureServices |> ignore
builder.UseUrls (sprintf "http://0.0.0.0:%i/" WS_PORT) |> ignore

log (Info "starting ConsoleLogger agent") // note: will be logged as IgnoredInput (since ConsoleLogger agent not yet started)
logEverythingExceptVerboseAndTicker |> consoleLogger.Start
log (Info "starting core agents")
logAllEventsExceptTick |> broadcaster.Start
SECONDS_PER_TICK |> ticker.Start
() |> persistence.Start

// TODO-NMB-HIGH: *Temporarily* remove #if DEBUG restriction to create default persisted events on Azure site (note: also requires similar change in authorization.fs)?...
#if DEBUG
log (Info "creating default persisted events (if necessary)")
createDefaultPersistedEvents |> Async.RunSynchronously
#endif

// Note: If entity agents were started by #if DEBUG code above [and then "reset"], they will just "bypass" subsequent Start calls (i.e. no new subscription) and not block the caller.
log (Info "starting entity agents")
() |> users.Start

log (Info "reading persisted events")
readPersistedEvents ()

log (Info "starting Connections agent")
() |> connections.Start

log (Info "ready")

let private host = builder.Build ()

host.Run ()
