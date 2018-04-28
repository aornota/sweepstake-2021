module Aornota.Sweepstake2018.Server.Host

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Literals
open Aornota.Sweepstake2018.Server.Agents
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Agents.Ticker
open Aornota.Sweepstake2018.Server.WsMiddleware

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open Giraffe

//#if DEBUG // TODO-NMB-HIGH: *Temporarily* remove #if DEBUG restriction to create default persisted events on Azure site (note: might also require similar change in users.fs)?...
// Note: Although this will broadcast events (e.g. UserEventWritten), no agents should yet be subscribed to these.
let private createDefaultPersistedEvents () =
    let createDefaultUsersEvents = async {
        if Directory.Exists (directory EntityType.Users) |> not then // note: only create "default" UsersEvents if directory (e.g. ./persisted/users) does not exist
            let nephId, rosieId, hughId, willId, trollId = UserId Guid.Empty, UserId (Guid "10000000-0000-0000-0000-000000000000"), UserId (Guid "11000000-0000-0000-0000-000000000000"), UserId (Guid "20000000-0000-0000-0000-000000000000"), UserId (Guid "f0000000-0000-0000-0000-000000000000")
            // TEMP-NMB: Create (indirectly) via Users agent...
            // Note: Although this will instatiate the Users agent, this will just ignore the later UsersEventsRead broadcast (caused by readPersistedEvents); see OnUsersEventsRead implementation.
            let createUserToken, defaultPassword, initialRvn, newPassword = Users.createUserAnyToken, Password "password", Rvn 1, Password "passw0rd"           
            let! _ = Users.users.HandleCreateUserCmdAsync (createUserToken, nephId, nephId, UserName "neph", defaultPassword, SuperUser) // note: silently ignore HandleCreateUserCmdAsync (&c.) result/s
            let! _ = Users.users.HandleChangePasswordCmdAsync (Users.changePasswordToken nephId, nephId, Rvn 1, newPassword)
            let! _ = Users.users.HandleCreateUserCmdAsync (createUserToken, nephId, rosieId, UserName "rosie", defaultPassword, Administrator)
            let! _ = Users.users.HandleCreateUserCmdAsync (createUserToken, nephId, hughId, UserName "hugh", defaultPassword, Pleb)
            let! _ = Users.users.HandleChangeUserTypeCmdAsync (Users.changeUserTypeToken, nephId, hughId, initialRvn, Administrator)
            let! _ = Users.users.HandleCreateUserCmdAsync (createUserToken, nephId, willId, UserName "will", defaultPassword, Pleb)
            let! _ = Users.users.HandleResetPasswordCmdAsync (Users.resetPasswordToken, nephId, willId, initialRvn, newPassword)
            let! _ = Users.users.HandleCreateUserCmdAsync (createUserToken, nephId, trollId, UserName "troll", defaultPassword, PersonaNotGrata)
            // ...or (directly) via Persistence agent...
            (*let initialRvn, defaultSalt, defaultHash, secondRvn, newSalt, newHash =
                Rvn 1, Events.User.Salt "Fake Salt", Events.User.Hash "Fake Hash", Rvn 2, Events.User.Salt "New fake Salt", Events.User.Hash "New fake Hash"
            let! _ = persistence.WriteUserEventAsync (nephId, initialRvn, Events.User.UserCreated (nephId, UserName "neph", defaultSalt, defaultHash, SuperUser)) // note: silently ignore WriteUserEventAsync result/s
            let! _ = persistence.WriteUserEventAsync (nephId, secondRvn, Events.User.PasswordChanged (nephId, newSalt, newHash))
            let! _ = persistence.WriteUserEventAsync (nephId, initialRvn, Events.User.UserCreated (rosieId, UserName "rosie", defaultSalt, defaultHash, Administrator))
            let! _ = persistence.WriteUserEventAsync (nephId, initialRvn, Events.User.UserCreated (hughId, UserName "hugh", defaultSalt, defaultHash, Pleb))
            let! _ = persistence.WriteUserEventAsync (nephId, secondRvn, Events.User.UserTypeChanged (hughId, Administrator))
            let! _ = persistence.WriteUserEventAsync (nephId, initialRvn, Events.User.UserCreated (willId, UserName "will", defaultSalt, defaultHash, Pleb))
            let! _ = persistence.WriteUserEventAsync (nephId, secondRvn, Events.User.PasswordReset (willId, newSalt, newHash))
            let! _ = persistence.WriteUserEventAsync (nephId, initialRvn, Events.User.UserCreated (trollId, UserName "troll", defaultSalt, defaultHash, PersonaNotGrata))*)
            // ...NMB-TEMP
            ()
        return () }
    createDefaultUsersEvents |> Async.Start
//#endif

// Note: Only for agents with an ensureInstantiated function - e.g. not Persistence agent (instantiated via readPersistedEvents), nor Broadcaster | Connections | Ticker | (&c.).
let private ensureAgentsInstantiated () =
    Users.ensureInstantiated ()
    ChatProjection.ensureInstantiated ()

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
builder.ConfigureServices configureServices |> ignore
builder.UseUrls (sprintf "http://0.0.0.0:%i/" WS_PORT) |> ignore

#if DEBUG
createDefaultPersistedEvents ()
#endif

ensureAgentsInstantiated ()
readPersistedEvents ()
ticker.Start ()

let private host = builder.Build ()

host.Run()
