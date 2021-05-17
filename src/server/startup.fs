module Aornota.Sweepstake2021.Server.Startup

open Aornota.Sweepstake2021.Server.WsMiddleware

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection

type Startup(_configuration:IConfiguration) =
    member __.Configure(applicationBuilder:IApplicationBuilder) =
        applicationBuilder
            .UseDefaultFiles()
            .UseStaticFiles()
            .UseWebSockets()
            .UseMiddleware<WsMiddleware>() |> ignore
    member __.ConfigureServices(services:IServiceCollection) = ()
