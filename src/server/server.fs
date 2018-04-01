module Aornota.Sweepstake2018.Server.Server

open Aornota.Sweepstake2018.Server.Ws

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

let private uiPath = Path.Combine ("..", "ui") |> Path.GetFullPath
let private port = 8088us

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
builder.UseUrls (sprintf "http://0.0.0.0:%i/" port) |> ignore

let private host = builder.Build ()

host.Run()
