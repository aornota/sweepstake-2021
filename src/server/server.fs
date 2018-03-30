module Aornota.Sweepstake2018.UI.App

open Aornota.Sweepstake2018.Shared

open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Hosting

open Giraffe

let private uiPath = Path.Combine ("..", "ui") |> Path.GetFullPath
let private port = 8088us

let private random = Random ()

let private api (fApi:unit -> 'a) next ctx = task {
#if DEBUG
    do! Async.Sleep (random.Next (250, 1250)) |> Async.StartAsTask
#endif
    let! apiResult = fApi ()
    return! Successful.OK apiResult next ctx }

let private initializeCounter () : Task<Counter> = task { return 42 }

let webApp : HttpHandler =
    route "/api/initializeCounter" >=> api initializeCounter

let private configureApp (app:IApplicationBuilder) =
    app.UseStaticFiles () |> ignore
    app.UseGiraffe webApp

let private configureServices (services:IServiceCollection) =
    services.AddGiraffe () |> ignore

let private builder = WebHost.CreateDefaultBuilder ()

builder.UseWebRoot (uiPath) |> ignore
builder.UseContentRoot (uiPath) |> ignore
builder.Configure (Action<IApplicationBuilder> configureApp) |> ignore
builder.ConfigureServices (configureServices) |> ignore
builder.UseUrls (sprintf "http://0.0.0.0:%i/" port) |> ignore

let private host = builder.Build ()

host.Run()
