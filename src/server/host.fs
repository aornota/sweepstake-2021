module Aornota.Sweepstake2018.Server.Host

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Server.Agents
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.WsMiddleware

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

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

// TODO-NMB-HIGH: *Temporarily* remove #if DEBUG restriction to create default persisted events on Azure site (note: also requires similar change in authorization.fs)?...
#if DEBUG
async {
    let createDefaultUsersEvents = async {
        let usersDir = Persistence.directory Persistence.EntityType.Users
        // TEMP-NMB: Force (re-)creation of default User/s events even if directory already exists...
        if Directory.Exists usersDir then
            log (Info (sprintf "deleting existing User/s events -> %s" usersDir))
            Directory.GetFiles usersDir |> Array.iter File.Delete
            Directory.Delete usersDir
        // ...NMB-TEMP
        if Directory.Exists usersDir then log (Info (sprintf "preserving existing User/s events -> %s" usersDir))
        else
            log (Info (sprintf "creating existing User/s events -> %s" usersDir))
            log (Info "sending Users agent dummy (empty) OnUsersEventsRead")
            // Note: Need to send dummy (empty) OnUsersEventsRead to Users agent else HandleCreateUserCmdAsync (&c.) will be skipped (and hence will block).
            // Note: Although other events will be broadcast (e.g. UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these.
            [] |> Users.users.OnUsersEventsRead
            let log cmd result = match result with | Ok ok -> log (Info (sprintf "%s succeeded -> %A" cmd ok)) | Error error -> log (Danger (sprintf "%ss failed -> %A" cmd error))
            let nephId, rosieId, hughId, willId, trollId = UserId Guid.Empty, UserId (Guid "10000000-0000-0000-0000-000000000000"), UserId (Guid "11000000-0000-0000-0000-000000000000"), UserId (Guid "20000000-0000-0000-0000-000000000000"), UserId (Guid "f0000000-0000-0000-0000-000000000000")
            let createUserToken, defaultPassword, initialRvn, newPassword = Authorization.createUserAnyToken, Password "password", Rvn 1, Password "arseword"
            let! _ = (createUserToken, nephId, nephId, UserName "neph", defaultPassword, SuperUser) |> Users.users.HandleCreateUserCmdAsync
            let! _ = (Authorization.changePasswordToken nephId, nephId, Rvn 1, newPassword) |> Users.users.HandleChangePasswordCmdAsync
            let! _ = (createUserToken, nephId, rosieId, UserName "rosie", defaultPassword, Administrator) |> Users.users.HandleCreateUserCmdAsync
            let! _ = (createUserToken, nephId, hughId, UserName "hugh", defaultPassword, Pleb) |> Users.users.HandleCreateUserCmdAsync
            let! _ = (Authorization.changeUserTypeToken, nephId, hughId, initialRvn, Administrator) |> Users.users.HandleChangeUserTypeCmdAsync
            let! _ = (createUserToken, nephId, willId, UserName "will", defaultPassword, Pleb) |> Users.users.HandleCreateUserCmdAsync
            let! _ = (Authorization.resetPasswordToken, nephId, willId, initialRvn, newPassword) |> Users.users.HandleResetPasswordCmdAsync
            let! _ = (createUserToken, nephId, trollId, UserName "troll", defaultPassword, PersonaNotGrata) |> Users.users.HandleCreateUserCmdAsync
            // TODO-NMB-HIGH: Test various failing scenarios...
            ()
        return () }
    do! createDefaultUsersEvents } |> Async.RunSynchronously
#endif

// Note: Only for agents with an ensureInstantiated function (e.g. not ConsoleLogger | Broadcaster | Ticker | Persistence | &c.).
log (Info "ensuring agents instantiated before reading persisted events")
log (Info "ensuring Users agent instantiated")
Users.ensureInstantiated ()
(* TODO-NMB-HIGH: Once ChatProjection agent implemented... log "ensuring ChatProjection agent instantiated"
ChatProjection.ensureInstantiated ()*)

log (Info "reading persisted events")
Persistence.readPersistedEvents ()

log (Info "starting Ticker agent")
Ticker.ticker.Start ()

let private host = builder.Build ()

host.Run ()
