module Aornota.Sweepstake2018.Server.DefaultData

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Authorization

open System
open System.IO

let private log category = consoleLogger.Log (Host, category)

let private logResult shouldSucceed scenario result =
    match shouldSucceed, result with
    | true, Ok _ -> log (Verbose (sprintf "%s -> succeeded (as expected)" scenario))
    | true, Error error -> log (Danger (sprintf "%s -> unexpectedly failed -> %A" scenario error))
    | false, Ok _ -> log (Danger (sprintf "%s -> unexpectedly succeeded" scenario))
    | false, Error error -> log (Verbose (sprintf "%s -> failed (as expected) -> %A" scenario error))
let private logShouldSucceed scenario result = logResult true scenario result
let private logShouldFail scenario result = logResult false scenario result

let private delete dir =
    Directory.GetFiles dir |> Array.iter File.Delete
    Directory.Delete dir

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    (* TEMP-NMB: Force re-creation of initial User/s events if directory already exists...
    if Directory.Exists usersDir then
        log (Info (sprintf "deleting existing User/s events -> %s" usersDir))
        delete usersDir *)

    if Directory.Exists usersDir then log (Info (sprintf "preserving existing User/s events -> %s" usersDir))
    else
        log (Info (sprintf "creating initial User/s events -> %s" usersDir))
        log (Info "starting Users agent")
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        log (Info "sending dummy OnUsersEventsRead to Users agent")
        [] |> users.OnUsersEventsRead
        // Note: Only create initial SuperUser.
        let nephId, neph, dummyPassword = UserId Guid.Empty, UserName "neph", Password "password"
        let createUserToken, auditUserId = CreateUserToken [ SuperUser ; Administrator ; Pleb ; PersonaNotGrata ], nephId
        let! result = (createUserToken, auditUserId, nephId, neph, dummyPassword, SuperUser) |> users.HandleCreateUserCmdAsync
        logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph) result
            
        (* TEMP-NMB: Test various scenarios...
        let changePasswordToken, resetPasswordToken, changeUserTypeToken = Authorization.ChangePasswordToken nephId, Authorization.ResetPasswordToken, Authorization.ChangeUserTypeToken
        let dummySessionId, initialRvn, newDummyPassword = SessionId (Guid.NewGuid ()), Rvn 1, Password "drowssap"
        let rosieId, rosie = UserId (Guid "10000000-0000-0000-0000-000000000000"), UserName "rosie"
        let hughId, hugh = UserId (Guid "11000000-0000-0000-0000-000000000000"), UserName "hugh"
        let willId, will = UserId (Guid "20000000-0000-0000-0000-000000000000"), UserName "will"
        let trollId, troll = UserId (Guid "f0000000-0000-0000-0000-000000000000"), UserName "troll"
        let unknownUserId, unknownUser = UserId (Guid.NewGuid ()), UserName "unknown"
        // Test HandleSignInCmdAsync:
        let! result = (dummySessionId, neph, dummyPassword) |> users.HandleSignInCmdAsync
        logShouldSucceed (sprintf "HandleSignInCmdAsync (%A)" neph) result
        let! result = (dummySessionId, UserName String.Empty, dummyPassword) |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (invalid userName: blank)" result
        let! result = (dummySessionId, UserName "bob", dummyPassword) |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (invalid userName: too short)" result
        let! result = (dummySessionId, neph, Password String.Empty) |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (invalid password: blank)" result
        let! result = (dummySessionId, neph, Password "1234") |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (invalid password: too short)" result
        let! result = (dummySessionId, neph, Password "Password") |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (incorrect password: case-sensitive)" result
        let! result = (dummySessionId, unknownUser, dummyPassword) |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (unknown userName)" result
        let! result = (createUserToken, nephId, trollId, troll, dummyPassword, PersonaNotGrata) |> users.HandleCreateUserCmdAsync
        logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" troll) result
        let! result = (dummySessionId, troll, dummyPassword) |> users.HandleSignInCmdAsync
        logShouldFail "HandleSignInCmdAsync (PersonaNonGrata)" result
        // Test HandleChangePasswordCmdAsync:
        let! result = (changePasswordToken, auditUserId, initialRvn, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        logShouldSucceed (sprintf "HandleChangePasswordCmdAsync (%A)" neph) result
        let! result = (Authorization.ChangePasswordToken rosieId, auditUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (invalid ChangePasswordToken)" result
        let! result = (Authorization.ChangePasswordToken unknownUserId, unknownUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (unknown auditUserId)" result
        let! result = (changePasswordToken, auditUserId, Rvn 2, Password String.Empty) |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (invalid password: blank)" result
        let! result = (changePasswordToken, auditUserId, Rvn 2, Password "1234") |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (invalid password: too short)" result
        let! result = (changePasswordToken, auditUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (invalid password: same as current)" result
        let! result = (changePasswordToken, auditUserId, Rvn 3, Password "pa$$word") |> users.HandleChangePasswordCmdAsync
        logShouldFail "HandleChangePasswordCmdAsync (invalid current Rvn)" result
        // Test HandleCreateUserCmdAsync:
        let! result = (createUserToken, auditUserId, rosieId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie) result
        let! result = (createUserToken, auditUserId, hughId, hugh, dummyPassword, Pleb) |> users.HandleCreateUserCmdAsync
        logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh) result
        let! result = (createUserToken, auditUserId, willId, will, dummyPassword, Pleb) |> users.HandleCreateUserCmdAsync
        logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will) result
        let! result = (Authorization.CreateUserToken [ Pleb ], auditUserId, unknownUserId, unknownUser, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (invalid CreateUserToken)" result
        let! result = (createUserToken, auditUserId, unknownUserId, UserName String.Empty, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (invalid userName: blank)" result
        let! result = (createUserToken, auditUserId, unknownUserId, UserName "bob", dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (invalid userName: too short)" result
        let! result = (createUserToken, auditUserId, unknownUserId, unknownUser, Password String.Empty, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (invalid password: blank)" result
        let! result = (createUserToken, auditUserId, unknownUserId, unknownUser, Password "1234", Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (invalid password: too short)" result
        let! result = (createUserToken, auditUserId, rosieId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (userId already exists)" result
        let! result = (createUserToken, auditUserId, unknownUserId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        logShouldFail "HandleCreateUserCmdAsync (userName already exists)" result
        // Test HandleResetPasswordCmdAsync:
        let! result = (resetPasswordToken, auditUserId, willId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync
        logShouldSucceed (sprintf "HandleResetPasswordCmdAsync (%A)" will) result
        // TODO-NMB-HIGH: Invalid ResetPasswordToken...
        let! result = (resetPasswordToken, auditUserId, unknownUserId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync
        logShouldFail "HandleResetPasswordCmdAsync (unknown userId)" result
        let! result = (resetPasswordToken, auditUserId, willId, Rvn 2, Password String.Empty) |> users.HandleResetPasswordCmdAsync
        logShouldFail "HandleResetPasswordCmdAsync (invalid password; blank)" result
        let! result = (resetPasswordToken, auditUserId, willId, Rvn 2, Password "1234") |> users.HandleResetPasswordCmdAsync
        logShouldFail "HandleResetPasswordCmdAsync (invalid password; too short)" result
        let! result = (resetPasswordToken, auditUserId, willId, Rvn 0, Password "pa$$word") |> users.HandleResetPasswordCmdAsync
        logShouldFail "HandleResetPasswordCmdAsync (invalid current Rvn)" result
        // Test HandleChangeUserTypeCmdAsync:
        let! result = (changeUserTypeToken, auditUserId, hughId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync
        logShouldSucceed (sprintf "HandleChangeUserTypeCmdAsync (%A %A)" hugh Administrator) result
        // TODO-NMB-HIGH: Invalid ChangeUserTypeToken...
        let! result = (changeUserTypeToken, auditUserId, unknownUserId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync
        logShouldFail "HandleChangeUserTypeCmdAsync (unknown userId)" result
        let! result = (changeUserTypeToken, auditUserId, hughId, Rvn 2, Administrator) |> users.HandleChangeUserTypeCmdAsync
        logShouldFail "HandleChangeUserTypeCmdAsync (invalid userType: same as current)" result
        let! result = (changeUserTypeToken, auditUserId, hughId, initialRvn, SuperUser) |> users.HandleChangeUserTypeCmdAsync
        logShouldFail "HandleChangeUserTypeCmdAsync (invalid current Rvn)" result *)

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        log (Info "resetting Users agent")
        () |> users.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    log (Info "creating initial persisted events (if necessary)")
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
