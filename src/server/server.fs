module Aornota.Sweepstake2018.Server.Host

open Aornota.Sweepstake2018.Server.WsMiddleware
open Aornota.Sweepstake2018.Shared.Literals

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

// Note: Relative to current [server] directory, "ui" folder might be sibling (e.g. when running with webpack-dev-server) or child (e.g. once published).
let private uiPath =
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
builder.ConfigureServices configureServices |> ignore
builder.UseUrls (sprintf "http://0.0.0.0:%i/" WS_PORT) |> ignore

let private host = builder.Build ()

host.Run()
