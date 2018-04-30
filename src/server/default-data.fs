module Aornota.Sweepstake2018.Server.DefaultData

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Persistence

open System
open System.IO

let private log category = consoleLogger.Log (Host, category)

let createDefaultPersistedEvents = async { // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these.
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createDefaultPersistedEvents", function | Host | Entity _ -> allCategories | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    let createDefaultUsersEvents = async {
        let usersDir = directory EntityType.Users
        // TEMP-NMB: Force (re-)creation of default User/s events even if directory already exists...
        if Directory.Exists usersDir then
            log (Info (sprintf "deleting existing User/s events -> %s" usersDir))
            Directory.GetFiles usersDir |> Array.iter File.Delete
            Directory.Delete usersDir
        // ...NMB-TEMP
        if Directory.Exists usersDir then log (Info (sprintf "preserving existing User/s events -> %s" usersDir))
        else
            log (Info (sprintf "creating default User/s events -> %s" usersDir))
            log (Info "starting Users agent")
            () |> users.Start
            // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored.
            log (Info "sending dummy OnUsersEventsRead to Users agent")
            [] |> users.OnUsersEventsRead
            let nephId, rosieId, hughId, willId, trollId = UserId Guid.Empty, UserId (Guid "10000000-0000-0000-0000-000000000000"), UserId (Guid "11000000-0000-0000-0000-000000000000"), UserId (Guid "20000000-0000-0000-0000-000000000000"), UserId (Guid "f0000000-0000-0000-0000-000000000000")
            let createUserToken, defaultPassword, initialRvn, newPassword = Authorization.createUserAnyToken, Password "password", Rvn 1, Password "arseword"
            let! _ = (createUserToken, nephId, nephId, UserName "neph", defaultPassword, SuperUser) |> users.HandleCreateUserCmdAsync
            let! _ = (Authorization.changePasswordToken nephId, nephId, Rvn 1, newPassword) |> users.HandleChangePasswordCmdAsync
            let! _ = (createUserToken, nephId, rosieId, UserName "rosie", defaultPassword, Administrator) |> users.HandleCreateUserCmdAsync
            let! _ = (createUserToken, nephId, hughId, UserName "hugh", defaultPassword, Pleb) |> users.HandleCreateUserCmdAsync
            let! _ = (Authorization.changeUserTypeToken, nephId, hughId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync
            let! _ = (createUserToken, nephId, willId, UserName "will", defaultPassword, Pleb) |> users.HandleCreateUserCmdAsync
            let! _ = (Authorization.resetPasswordToken, nephId, willId, initialRvn, newPassword) |> users.HandleResetPasswordCmdAsync
            let! _ = (createUserToken, nephId, trollId, UserName "troll", defaultPassword, PersonaNotGrata) |> users.HandleCreateUserCmdAsync

            (* TODO-NMB-HIGH: Test various failing scenarios...
            let logResult cmd result = match result with | Ok ok -> log (Info (sprintf "%s succeeded -> %A" cmd ok)) | Error error -> log (Danger (sprintf "%s failed -> %A" cmd error))
            let! result = (Authorization.changePasswordToken nephId, nephId, Rvn 69, Password "shmarseword") |> users.HandleChangePasswordCmdAsync
            logResult "HandleChangePasswordCmdAsync" result*)

            // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
            log (Info "resetting Users agent")
            () |> users.Reset
        return () }
    do! createDefaultUsersEvents
    previousLogFilter |> consoleLogger.ChangeLogFilter }
