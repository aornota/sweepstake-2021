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

let private uiPath = Path.Combine ("..", "ui") |> Path.GetFullPath

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
