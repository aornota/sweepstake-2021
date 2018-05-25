module Aornota.Sweepstake2018.Server.DefaultData

open Aornota.Common.IfDebug
open Aornota.Common.Revision

open Aornota.Server.Common.Helpers

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Entities.Squads
open Aornota.Sweepstake2018.Server.Agents.Entities.Users
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Authorization

open System
open System.IO

let private deleteExistingUsersEvents = ifDebug false false // note: should *not* generally set to true for Release
let private deleteExistingSquadsEvents = ifDebug false false // note: should *not* generally set to true for Release

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

let private ifToken fCmdAsync token = async { return! match token with | Some token -> token |> fCmdAsync | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync }

let private superUser = SuperUser
let private nephId = Guid.Empty |> UserId
let private nephTokens = permissions nephId superUser |> UserTokens

// #region SquadIds
let egyptId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let russiaId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let saudiArabiaId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let uruguayId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId
let iranId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let moroccoId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let portugalId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let spainId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId
let australiaId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let denmarkId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let franceId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let peruId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId
let argentinaId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let croatiaId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let icelandId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let nigeriaId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId
let brazilId = Guid "00000051-0000-0000-0000-000000000000" |> SquadId
let costaRicaId = Guid "00000052-0000-0000-0000-000000000000" |> SquadId
let serbiaId = Guid "00000053-0000-0000-0000-000000000000" |> SquadId
let switzerlandId = Guid "00000054-0000-0000-0000-000000000000" |> SquadId
let germanyId = Guid "00000061-0000-0000-0000-000000000000" |> SquadId
let mexicoId = Guid "00000062-0000-0000-0000-000000000000" |> SquadId
let southKoreaId = Guid "00000063-0000-0000-0000-000000000000" |> SquadId
let swedenId = Guid "00000064-0000-0000-0000-000000000000" |> SquadId
let belgiumId = Guid "00000071-0000-0000-0000-000000000000" |> SquadId
let englandId = Guid "00000072-0000-0000-0000-000000000000" |> SquadId
let panamaId = Guid "00000073-0000-0000-0000-000000000000" |> SquadId
let tunisiaId = Guid "00000074-0000-0000-0000-000000000000" |> SquadId
let colombiaId = Guid "00000081-0000-0000-0000-000000000000" |> SquadId
let japanId = Guid "00000082-0000-0000-0000-000000000000" |> SquadId
let polandId = Guid "00000083-0000-0000-0000-000000000000" |> SquadId
let senegalId = Guid "00000084-0000-0000-0000-000000000000" |> SquadId
// #endregion

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    // #region: Force re-creation of initial User/s events if directory already exists (if requested)
    if deleteExistingUsersEvents && Directory.Exists usersDir then
        sprintf "deleting existing User/s events -> %s" usersDir |> Info |> log
        delete usersDir
    // #endregion

    if Directory.Exists usersDir then sprintf "preserving existing User/s events -> %s" usersDir |> Info |> log
    else
        sprintf "creating initial User/s events -> %s" usersDir |> Info |> log
        "starting Users agent" |> Info |> log
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnUsersEventsRead to Users agent" |> Info |> log
        [] |> users.OnUsersEventsRead

        // #region: Create initial SuperUser | Administators - and a couple of Plebs
        let neph = UserName "neph"
        let dummyPassword = Password "password"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nephId, neph, dummyPassword, superUser) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph)
        let administrator = Administrator
        let rosieId, rosie = Guid "ffffffff-0001-0000-0000-000000000000" |> UserId, UserName "rosie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, rosieId, rosie, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie)
        let hughId, hugh = Guid "ffffffff-0002-0000-0000-000000000000" |> UserId, UserName "hugh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, hughId, hugh, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh)
        let pleb = Pleb
        let robId, rob = Guid "ffffffff-ffff-0001-0000-000000000000" |> UserId, UserName "rob"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, robId, rob, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rob)
        let joshId, josh = Guid "ffffffff-ffff-0002-0000-000000000000" |> UserId, UserName "josh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, joshId, josh, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" josh)
        // #endregion

        // #region: TEMP-NMB: Test various scenarios (note: expects initial SuperUser | Administrators | Plebs to have been created)...
        (*let initialRvn, newDummyPassword = Rvn 1, Password "drowssap"
        let rosieTokens = permissions rosieId adminType |> UserTokens
        let willId, will = Guid "ffffffff-ffff-0003-0000-000000000000" |> UserId, UserName "will"
        let personaNonGrataId, personaNonGrata = Guid "ffffffff-ffff-ffff-0001-000000000000" |> UserId, UserName "persona non grata"
        let unknownUserId, unknownUser = Guid.NewGuid () |> UserId, UserName "unknown"
        let personaNonGrataTokens = permissions personaNonGrataId PersonaNonGrata |> UserTokens
        let unknownUserTokens = permissions unknownUserId Pleb |> UserTokens
        // Test HandleSignInCmdAsync:
        let! result = (neph, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldSucceed (sprintf "HandleSignInCmdAsync (%A)" neph)
        let! result = (UserName String.Empty, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid userName: blank)"
        let! result = (UserName "bob", dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid userName: too short)"
        let! result = (neph, Password String.Empty) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid password: blank)"
        let! result = (neph, Password "1234") |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (invalid password: too short)"
        let! result = (neph, Password "PASSWORD") |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (incorrect password: case-sensitive)"
        let! result = (unknownUser, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (unknown userName)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, personaNonGrataId, personaNonGrata, dummyPassword, PersonaNonGrata) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" personaNonGrata)
        let! result = (personaNonGrata, dummyPassword) |> users.HandleSignInCmdAsync
        result |> logShouldFail "HandleSignInCmdAsync (PersonaNonGrata)"
        // Test HandleChangePasswordCmdAsync:
        let! result = nephTokens.ChangePasswordToken |> ifToken (fun token -> (token, nephId, initialRvn, newDummyPassword) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldSucceed (sprintf "HandleChangePasswordCmdAsync (%A)" neph)
        let! result = rosieTokens.ChangePasswordToken |> ifToken (fun token -> (token, hughId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid ChangePasswordToken: userId differs from auditUserId)"
        let! result = personaNonGrataTokens.ChangePasswordToken |> ifToken (fun token -> (token, personaNonGrataId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (no ChangePasswordToken)"
        let! result = unknownUserTokens.ChangePasswordToken |> ifToken (fun token -> (token, unknownUserId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (unknown auditUserId)"
        let! result = nephTokens.ChangePasswordToken |> ifToken (fun token -> (token, nephId, Rvn 2, Password String.Empty) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: blank)"
        let! result = nephTokens.ChangePasswordToken |> ifToken (fun token -> (token, nephId, Rvn 2, Password "1234") |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: too short)"
        let! result = nephTokens.ChangePasswordToken |> ifToken (fun token -> (token, nephId, Rvn 2, newDummyPassword) |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid password: same as current)"
        let! result = nephTokens.ChangePasswordToken |> ifToken (fun token -> (token, nephId, Rvn 3, Password "pa$$word") |> users.HandleChangePasswordCmdAsync)
        result |> logShouldFail "HandleChangePasswordCmdAsync (invalid current Rvn)"
        // Test HandleCreateUserCmdAsync:
        let! result = rosieTokens.CreateUserToken |> ifToken (fun token -> (token, rosieId, willId, will, dummyPassword, Pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will)
        let! result = rosieTokens.CreateUserToken |> ifToken (fun token -> (token, rosieId, unknownUserId, unknownUser, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid CreateUserToken: UserType not allowed)"
        let! result = personaNonGrataTokens.CreateUserToken |> ifToken (fun token -> (token, personaNonGrataId, unknownUserId, unknownUser, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (no CreateUserToken)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, unknownUserId, UserName String.Empty, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid userName: blank)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, unknownUserId, UserName "bob", dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid userName: too short)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, unknownUserId, unknownUser, Password String.Empty, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid password: blank)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, unknownUserId, unknownUser, Password "1234", Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (invalid password: too short)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, rosieId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (userId already exists)"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, unknownUserId, rosie, dummyPassword, Administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldFail "HandleCreateUserCmdAsync (userName already exists)"
        // Test HandleResetPasswordCmdAsync:
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, rosieId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldSucceed (sprintf "HandleResetPasswordCmdAsync (%A)" will)
        let! result = rosieTokens.ResetPasswordToken |> ifToken (fun token -> (token, rosieId, willId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldSucceed (sprintf "HandleResetPasswordCmdAsync (%A)" will)
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, nephId, Rvn 2, dummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid ResetPasswordToken: valid UserTarget is NotSelf)"
        let! result = rosieTokens.ResetPasswordToken |> ifToken (fun token -> (token, rosieId, nephId, Rvn 2, dummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid ResetPasswordToken: UserType for UserTarget not allowed)"
        let! result = personaNonGrataTokens.ResetPasswordToken |> ifToken (fun token -> (token, personaNonGrataId, nephId, Rvn 2, dummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (no ResetPasswordToken)"
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, unknownUserId, initialRvn, newDummyPassword) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (unknown userId)"
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, willId, Rvn 2, Password String.Empty) |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid password; blank)"
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, willId, Rvn 2, Password "1234") |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid password; too short)"
        let! result = nephTokens.ResetPasswordToken |> ifToken (fun token -> (token, nephId, willId, Rvn 0, Password "pa$$word") |> users.HandleResetPasswordCmdAsync)
        result |> logShouldFail "HandleResetPasswordCmdAsync (invalid current Rvn)"
        // Test HandleChangeUserTypeCmdAsync:
        let! result = nephTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, nephId, hughId, initialRvn, Pleb) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldSucceed (sprintf "HandleChangeUserTypeCmdAsync (%A %A)" hugh Pleb)
        let! result = nephTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, nephId, nephId, Rvn 2, Administrator) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (invalid ChangeUserTypeToken: valid UserTarget is NotSelf)"
        // Note: Cannot test "UserType for UserTarget not allowed" or "UserType not allowed" as only SuperUsers have ChangeUserTypePermission - and they have it for all UserTypes.
        let! result = rosieTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, rosieId, nephId, Rvn 2, Administrator) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (no ChangeUserTypeToken)"
        let! result = nephTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, nephId, unknownUserId, initialRvn, Administrator) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (unknown userId)"
        let! result = nephTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, nephId, hughId, Rvn 2, Pleb) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (invalid userType: same as current)"
        let! result = nephTokens.ChangeUserTypeToken |> ifToken (fun token -> (token, nephId, hughId, initialRvn, SuperUser) |> users.HandleChangeUserTypeCmdAsync)
        result |> logShouldFail "HandleChangeUserTypeCmdAsync (invalid current Rvn)"*)
        // #endregion

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let private createInitialSquadsEventsIfNecessary = async {
    let squadsDir = directory EntityType.Squads

    // #region: Force re-creation of initial Squads/s events if directory already exists (if requested)
    if deleteExistingSquadsEvents && Directory.Exists squadsDir then
        sprintf "deleting existing Squad/s events -> %s" squadsDir |> Info |> log
        delete squadsDir
    // #endregion

    if Directory.Exists squadsDir then sprintf "preserving existing Squads/s events -> %s" squadsDir |> Info |> log
    else
        sprintf "creating initial Squads/s events -> %s" squadsDir |> Info |> log
        "starting Squads agent" |> Info |> log
        () |> squads.Start
        // Note: Send dummy OnSquadsEventsRead to Squads agent to ensure that it transitions [from pendingOnSquadsEventsRead] to managingSquads; otherwise HandleCreateSquadCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnSquadsEventsRead to Squads agent" |> Info |> log
        [] |> squads.OnSquadsEventsRead

        // #region: Create initial Squads - and subset of Players.

        // TODO-NMB-HIGH: Decide what to do about Russia's seeding (and update SCORING_SYSTEM_MARKDOWN accordingly)...

        // #region: Group A
        let egypt = SquadName "Egypt"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, egyptId, egypt, GroupA, Seeding 22, CoachName "Héctor Cúper") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" egypt)
        let russia = SquadName "Russia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, russiaId, russia, GroupA, Seeding 1, CoachName "Stanislav Cherchesov") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" russia)
        let saudiArabia = SquadName "Saudi Arabia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, saudiArabiaId, saudiArabia, GroupA, Seeding 32, CoachName "Juan Antonio Pizzi") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" saudiArabia)
        let uruguay = SquadName "Uruguay"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, uruguayId, uruguay, GroupA, Seeding 15, CoachName "Óscar Tabárez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" uruguay)
        // #endregion
        // #region: Group B
        let iran = SquadName "Iran"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, iranId, iran, GroupB, Seeding 24, CoachName "Carlos Queiroz") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" iran)
        let morocco = SquadName "Morocco"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, moroccoId, morocco, GroupB, Seeding 29, CoachName "Hervé Renard") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" morocco)
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupB, Seeding 4, CoachName "Fernando Santos") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" portugal)
        let spain = SquadName "Spain"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, spainId, spain, GroupB, Seeding 9, CoachName "Julen Lopetegui") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" spain)
        // #endregion
        // #region: Group C
        let australia = SquadName "Australia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, australiaId, australia, GroupC, Seeding 27, CoachName "Bert van Marwijk") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" australia)
        let denmark = SquadName "Denmark"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, denmarkId, denmark, GroupC, Seeding 17, CoachName "Åge Hareide") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" denmark)
        let france = SquadName "France"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupC, Seeding 8, CoachName "Didier Deschamps") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let peru = SquadName "Peru"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, peruId, peru, GroupC, Seeding 10, CoachName "Ricardo Gareca") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" peru)
        // #endregion
        // #region: Group D
        let argentina = SquadName "Argentina"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, argentinaId, argentina, GroupD, Seeding 5, CoachName "Jorge Sampaoli") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" argentina)
        let croatia = SquadName "Croatia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, croatiaId, croatia, GroupD, Seeding 16, CoachName "Zlatko Dalić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" croatia)
        let iceland = SquadName "Iceland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, icelandId, iceland, GroupD, Seeding 18, CoachName "Heimir Hallgrímsson") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" iceland)
        let nigeria = SquadName "Nigeria"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, nigeriaId, nigeria, GroupD, Seeding 26, CoachName "Gernot Rohr") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" nigeria)
        // #endregion
        // #region: Group E
        let brazil = SquadName "Brazil"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, brazilId, brazil, GroupE, Seeding 3, CoachName "Tite") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" brazil)
        let costaRica = SquadName "Costa Rica"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, costaRicaId, costaRica, GroupE, Seeding 19, CoachName "Óscar Ramírez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" costaRica)
        let serbia = SquadName "Serbia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, serbiaId, serbia, GroupE, Seeding 25, CoachName "Mladen Krstajić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" serbia)
        let switzerland = SquadName "Switzerland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, switzerlandId, switzerland, GroupE, Seeding 11, CoachName "Vladimir Petković") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" switzerland)
        // #endregion
        // #region: Group F
        let germany = SquadName "Germany"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, germanyId, germany, GroupF, Seeding 2, CoachName "Joachim Löw") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" germany)
        let mexico = SquadName "Mexico"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, mexicoId, mexico, GroupF, Seeding 14, CoachName "Juan Carlos Osorio") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" mexico)
        let southKorea = SquadName "South Korea"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, southKoreaId, southKorea, GroupF, Seeding 31, CoachName "Shin Tae-yong") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" southKorea)
        let sweden = SquadName "Sweden"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, swedenId, sweden, GroupF, Seeding 20, CoachName "Janne Andersson") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" sweden)
        // #endregion
        // #region: Group G
        let belgium = SquadName "Belgium"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, belgiumId, belgium, GroupG, Seeding 6, CoachName "Roberto Martínez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" belgium)
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupG, Seeding 12, CoachName "Gareth Southgate") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" england)
        // #region: England Players
        let jackButlandId, jackButland = Guid "00000072-0001-0000-0000-000000000000" |> PlayerId, PlayerName "Jack Butland"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 1, jackButlandId, jackButland, Goalkeeper) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england jackButland)
        let jordanPickfordId, jordanPickford = Guid "00000072-0002-0000-0000-000000000000" |> PlayerId, PlayerName "Jordan Pickford"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 2, jordanPickfordId, jordanPickford, Goalkeeper) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england jordanPickford)
        let nickPopeId, nickPope = Guid "00000072-0003-0000-0000-000000000000" |> PlayerId, PlayerName "Nick Pope"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 3, nickPopeId, nickPope, Goalkeeper) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england nickPope)
        let trentAlexanderArnoldId, trentAlexanderArnold = Guid "00000072-0000-0001-0000-000000000000" |> PlayerId, PlayerName "Trent Alexander-Arnold"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 4, trentAlexanderArnoldId, trentAlexanderArnold, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england trentAlexanderArnold)
        let garyCahillId, garyCahill = Guid "00000072-0000-0002-0000-000000000000" |> PlayerId, PlayerName "Gary Cahill"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 5, garyCahillId, garyCahill, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england garyCahill)
        let fabianDelphId, fabianDelph = Guid "00000072-0000-0003-0000-000000000000" |> PlayerId, PlayerName "Fabian Delph"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 6, fabianDelphId, fabianDelph, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england fabianDelph)
        let philJonesId, philJones = Guid "00000072-0000-0004-0000-000000000000" |> PlayerId, PlayerName "Phil Jones"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 7, philJonesId, philJones, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england philJones)
        let harryMaguireId, harryMaguire = Guid "00000072-0000-0005-0000-000000000000" |> PlayerId, PlayerName "Harry Maguire"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 8, harryMaguireId, harryMaguire, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england harryMaguire)
        let dannyRoseId, dannyRose = Guid "00000072-0000-0006-0000-000000000000" |> PlayerId, PlayerName "Danny Rose"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 9, dannyRoseId, dannyRose, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england dannyRose)
        let johnStonesId, johnStones = Guid "00000072-0000-0007-0000-000000000000" |> PlayerId, PlayerName "John Stones"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 10, johnStonesId, johnStones, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england johnStones)
        let kieranTrippierId, kieranTrippier = Guid "00000072-0000-0008-0000-000000000000" |> PlayerId, PlayerName "Kieran Trippier"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 11, kieranTrippierId, kieranTrippier, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england kieranTrippier)
        let kyleWalkerId, kyleWalker = Guid "00000072-0000-0009-0000-000000000000" |> PlayerId, PlayerName "Kyle Walker"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 12, kyleWalkerId, kyleWalker, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england kyleWalker)
        let ashleyYoungId, ashleyYoung = Guid "00000072-0000-0010-0000-000000000000" |> PlayerId, PlayerName "Ashley Young"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 13, ashleyYoungId, ashleyYoung, Defender) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england ashleyYoung)
        let deleAlliId, deleAlli = Guid "00000072-0000-0000-0001-000000000000" |> PlayerId, PlayerName "Dele Alli"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 14, deleAlliId, deleAlli, Midfielder) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england deleAlli)
        let ericDierId, ericDier = Guid "00000072-0000-0000-0002-000000000000" |> PlayerId, PlayerName "Eric Dier"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 15, ericDierId, ericDier, Midfielder) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england ericDier)
        let jordanHendersonId, jordanHenderson = Guid "00000072-0000-0000-0003-000000000000" |> PlayerId, PlayerName "Jordan Henderson"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 16, jordanHendersonId, jordanHenderson, Midfielder) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england jordanHenderson)
        let jesseLindgardId, jesseLindgard = Guid "00000072-0000-0000-0004-000000000000" |> PlayerId, PlayerName "Jesse Lindgard"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 17, jesseLindgardId, jesseLindgard, Midfielder) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england jesseLindgard)
        let rubenLoftusCheekId, rubenLoftusCheek = Guid "00000072-0000-0000-0005-000000000000" |> PlayerId, PlayerName "Ruben Loftus-Cheek"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 18, rubenLoftusCheekId, rubenLoftusCheek, Midfielder) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england rubenLoftusCheek)
        let harryKaneId, harryKane = Guid "00000072-0000-0000-0000-000000000001" |> PlayerId, PlayerName "Harry Kane"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 19, harryKaneId, harryKane, Forward) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england harryKane)
        let marcusRashfordId, marcusRashford = Guid "00000072-0000-0000-0000-000000000002" |> PlayerId, PlayerName "Marcus Rashford"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 20, marcusRashfordId, marcusRashford, Forward) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england marcusRashford)
        let raheemSterlingId, raheemSterling = Guid "00000072-0000-0000-0000-000000000003" |> PlayerId, PlayerName "Raheem Sterling"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 21, raheemSterlingId, raheemSterling, Forward) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england raheemSterling)
        let jamieVardyId, jamieVardy = Guid "00000072-0000-0000-0000-000000000004" |> PlayerId, PlayerName "Jamie Vardy"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 22, jamieVardyId, jamieVardy, Forward) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england jamieVardy)
        let dannyWelbeckId, dannyWelbeck = Guid "00000072-0000-0000-0000-000000000005" |> PlayerId, PlayerName "Danny Welbeck"
        let! result = nephTokens.AddOrEditPlayerToken |> ifToken (fun token -> (token, nephId, englandId, Rvn 23, dannyWelbeckId, dannyWelbeck, Forward) |> squads.HandleAddPlayerCmdAsync)
        result |> logShouldSucceed (sprintf "HandleAddPlayerCmdAsync (%A %A)" england dannyWelbeck)
        // #endregion
        let panama = SquadName "Panama"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, panamaId, panama, GroupG, Seeding 30, CoachName "Hernán Darío Gómez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" panama)
        let tunisia = SquadName "Tunisia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, tunisiaId, tunisia, GroupG, Seeding 21, CoachName "Nabil Maâloul") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" tunisia)
        // #endregion
        // #region: Group H
        let colombia = SquadName "Colombia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, colombiaId, colombia, GroupH, Seeding 13, CoachName "José Pékerman") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" colombia)
        let japan = SquadName "Japan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, japanId, japan, GroupH, Seeding 28, CoachName "Akira Nishino") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" japan)
        let poland = SquadName "Poland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, polandId, poland, GroupH, Seeding 7, CoachName "Adam Nawałka") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" poland)
        let senegal = SquadName "Senegal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, senegalId, senegal, GroupH, Seeding 23, CoachName "Aliou Cissé") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" senegal)
        // #endregion
        // #endregion

        // Note: Reset Squads agent [to pendingOnSquadsEventsRead] so that it handles subsequent SquadsEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Squads agent" |> Info |> log
        () |> squads.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialSquadsEventsIfNecessary // note: although this can cause various events to be broadcast (SquadsRead | SquadEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
