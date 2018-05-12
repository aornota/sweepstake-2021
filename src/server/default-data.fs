module Aornota.Sweepstake2018.Server.DefaultData

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Authorization

open System
open System.IO

let private log category = (Host, category) |> consoleLogger.Log

let private logResult shouldSucceed scenario result =
    match shouldSucceed, result with
    | true, Ok _ -> sprintf "%s -> succeeded (as expected)" scenario |> Verbose |> log
    | true, Error error -> sprintf "%s -> unexpectedly failed -> %A" scenario error |> Danger |> log
    | false, Ok _ -> sprintf "%s -> unexpectedly succeeded" scenario |> Danger |> log
    | false, Error error -> sprintf "%s -> failed (as expected) -> %A" scenario error |> Verbose |> log
let private logShouldSucceed scenario result = result |> logResult true scenario
let private logShouldFail scenario result = result |> logResult false scenario

let private delete dir =
    Directory.GetFiles dir |> Array.iter File.Delete
    Directory.Delete dir

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    (* TEMP-NMB: Force re-creation of initial User/s events if directory already exists... *)
    if Directory.Exists usersDir then
        sprintf "deleting existing User/s events -> %s" usersDir |> Info |> log
        delete usersDir

    if Directory.Exists usersDir then sprintf "preserving existing User/s events -> %s" usersDir |> Info |> log
    else
        sprintf "creating initial User/s events -> %s" usersDir |> Info |> log
        "starting Users agent" |> Info |> log
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnUsersEventsRead to Users agent" |> Info |> log
        [] |> users.OnUsersEventsRead
        // Note: Only create initial SuperUser.
        let nephId, neph, dummyPassword, nephType = UserId Guid.Empty, UserName "neph", Password "password", SuperUser
        let nephTokens, auditUserId = permissions nephId nephType |> UserTokens, nephId
        let! result = (nephTokens.CreateUserToken, auditUserId, nephId, neph, dummyPassword, nephType) |> users.HandleCreateUserCmdAsync
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph)
            
        (* TEMP-NMB: Test various scenarios... *)
        let sessionId, initialRvn, newDummyPassword = SessionId.Create (), Rvn 1, Password "drowssap"
        let rosieId, rosie = UserId (Guid "10000000-0000-0000-0000-000000000000"), UserName "rosie"
        let hughId, hugh = UserId (Guid "11000000-0000-0000-0000-000000000000"), UserName "hugh"
        let willId, will = UserId (Guid "20000000-0000-0000-0000-000000000000"), UserName "will"
        let personaNonGrataId, personaNonGrata = UserId (Guid "f0000000-0000-0000-0000-000000000000"), UserName "persona non grata"
        let unknownUserId, unknownUser = UserId (Guid.NewGuid ()), UserName "unknown"
        let rosieTokens = permissions rosieId Administrator |> UserTokens
        let personaNonGrataTokens = permissions personaNonGrataId PersonaNonGrata |> UserTokens
        let unknownUserTokens = permissions unknownUserId Pleb |> UserTokens
        // Test HandleSignInCmdAsync:
        let! result = (sessionId, neph, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldSucceed (sprintf "HandleSignInCmdAsync (%A)" neph)
        let! result = (sessionId, UserName String.Empty, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid userName: blank)"
        let! result = (sessionId, UserName "bob", dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid userName: too short)"
        let! result = (sessionId, neph, Password String.Empty) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid password: blank)"
        let! result = (sessionId, neph, Password "1234") |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid password: too short)"
        let! result = (sessionId, neph, Password "PASSWORD") |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (incorrect password: case-sensitive)"
        let! result = (sessionId, unknownUser, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (unknown userName)"
        let! result = (nephTokens.CreateUserToken, nephId, personaNonGrataId, personaNonGrata, dummyPassword, PersonaNonGrata) |> users.HandleCreateUserCmdAsync
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" personaNonGrata)
        let! result = (sessionId, personaNonGrata, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (PersonaNonGrata)"
        // Test HandleChangePasswordCmdAsync:
        let! result = (nephTokens.ChangePasswordToken, auditUserId, initialRvn, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        result |> logShouldSucceed (sprintf "HandleChangePasswordCmdAsync (%A)" neph)
        let! result = (rosieTokens.ChangePasswordToken, auditUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid ChangePasswordToken: userId differs from auditUserId)"
        let! result = (personaNonGrataTokens.ChangePasswordToken, auditUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (no ChangePasswordToken)"
        let! result = (unknownUserTokens.ChangePasswordToken, unknownUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (unknown auditUserId)"
        let! result = (nephTokens.ChangePasswordToken, auditUserId, Rvn 2, Password String.Empty) |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: blank)"
        let! result = (nephTokens.ChangePasswordToken, auditUserId, Rvn 2, Password "1234") |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: too short)"
        let! result = (nephTokens.ChangePasswordToken, auditUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: same as current)"
        let! result = (nephTokens.ChangePasswordToken, auditUserId, Rvn 3, Password "pa$$word") |> users.HandleChangePasswordCmdAsync
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid current Rvn)"
        // Test HandleCreateUserCmdAsync:
        let! result = (nephTokens.CreateUserToken, auditUserId, rosieId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie)
        let! result = (nephTokens.CreateUserToken, auditUserId, hughId, hugh, dummyPassword, Pleb) |> users.HandleCreateUserCmdAsync
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh)
        let! result = (nephTokens.CreateUserToken, auditUserId, willId, will, dummyPassword, Pleb) |> users.HandleCreateUserCmdAsync
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will)
        let! result = (rosieTokens.CreateUserToken, auditUserId, unknownUserId, unknownUser, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid CreateUserToken: UserType not allowed)"
        let! result = (personaNonGrataTokens.CreateUserToken, auditUserId, unknownUserId, unknownUser, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (no CreateUserToken)"
        let! result = (nephTokens.CreateUserToken, auditUserId, unknownUserId, UserName String.Empty, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid userName: blank)"
        let! result = (nephTokens.CreateUserToken, auditUserId, unknownUserId, UserName "bob", dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid userName: too short)"
        let! result = (nephTokens.CreateUserToken, auditUserId, unknownUserId, unknownUser, Password String.Empty, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid password: blank)"
        let! result = (nephTokens.CreateUserToken, auditUserId, unknownUserId, unknownUser, Password "1234", Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid password: too short)"
        let! result = (nephTokens.CreateUserToken, auditUserId, rosieId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (userId already exists)"
        let! result = (nephTokens.CreateUserToken, auditUserId, unknownUserId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync
        result |> logShouldFail "HandleCreateUserCmdAsync (userName already exists)"
        // Test HandleResetPasswordCmdAsync:
        let! result = (nephTokens.ResetPasswordToken, auditUserId, willId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync
        result |> logShouldSucceed (sprintf "HandleResetPasswordCmdAsync (%A)" will)

        // TODO-NMB-HIGH: Invalid ResetPasswordToken...

        let! result = (nephTokens.ResetPasswordToken, auditUserId, unknownUserId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync
        result |> logShouldFail "HandleResetPasswordCmdAsync (unknown userId)"
        let! result = (nephTokens.ResetPasswordToken, auditUserId, willId, Rvn 2, Password String.Empty) |> users.HandleResetPasswordCmdAsync
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid password; blank)"
        let! result = (nephTokens.ResetPasswordToken, auditUserId, willId, Rvn 2, Password "1234") |> users.HandleResetPasswordCmdAsync
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid password; too short)"
        let! result = (nephTokens.ResetPasswordToken, auditUserId, willId, Rvn 0, Password "pa$$word") |> users.HandleResetPasswordCmdAsync
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid current Rvn)"
        // Test HandleChangeUserTypeCmdAsync:
        let! result = (nephTokens.ChangeUserTypeToken, auditUserId, hughId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync
        result |> logShouldSucceed (sprintf "HandleChangeUserTypeCmdAsync (%A %A)" hugh Administrator)

        // TODO-NMB-HIGH: Invalid ChangeUserTypeToken...

        let! result = (nephTokens.ChangeUserTypeToken, auditUserId, unknownUserId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (unknown userId)"
        let! result = (nephTokens.ChangeUserTypeToken, auditUserId, hughId, Rvn 2, Administrator) |> users.HandleChangeUserTypeCmdAsync
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (invalid userType: same as current)"
        let! result = (nephTokens.ChangeUserTypeToken, auditUserId, hughId, initialRvn, SuperUser) |> users.HandleChangeUserTypeCmdAsync
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (invalid current Rvn)"

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
