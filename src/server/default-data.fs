module Aornota.Sweepstake2021.Server.DefaultData

open Aornota.Sweepstake2021.Common.Domain.Core
open Aornota.Sweepstake2021.Common.Domain.Draft
open Aornota.Sweepstake2021.Common.Domain.Fixture
open Aornota.Sweepstake2021.Common.Domain.Squad
open Aornota.Sweepstake2021.Common.Domain.User
open Aornota.Sweepstake2021.Common.IfDebug
open Aornota.Sweepstake2021.Common.WsApi.ServerMsg
open Aornota.Sweepstake2021.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2021.Server.Agents.Entities.Drafts
open Aornota.Sweepstake2021.Server.Agents.Entities.Fixtures
open Aornota.Sweepstake2021.Server.Agents.Entities.Squads
open Aornota.Sweepstake2021.Server.Agents.Entities.Users
open Aornota.Sweepstake2021.Server.Agents.Persistence
open Aornota.Sweepstake2021.Server.Authorization
open Aornota.Sweepstake2021.Server.Common.Helpers

open System
open System.IO

let private deleteExistingUsersEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingSquadsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingFixturesEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingDraftsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)

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

let private turkeyId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let private italyId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let private walesId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let private switzerlandId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId

let private denmarkId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let private finlandId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let private belgiumId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let private russiaId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId

let private netherlandsId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let private ukraineId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let private austriaId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let private northMacedoniaId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId

let private englandId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let private croatiaId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let private scotlandId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let private czechRepublicId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId

let private spainId = Guid "00000051-0000-0000-0000-000000000000" |> SquadId
let private swedenId = Guid "00000052-0000-0000-0000-000000000000" |> SquadId
let private polandId = Guid "00000053-0000-0000-0000-000000000000" |> SquadId
let private slovakiaId = Guid "00000054-0000-0000-0000-000000000000" |> SquadId

let private hungaryId = Guid "00000061-0000-0000-0000-000000000000" |> SquadId
let private portugalId = Guid "00000062-0000-0000-0000-000000000000" |> SquadId
let private franceId = Guid "00000063-0000-0000-0000-000000000000" |> SquadId
let private germanyId = Guid "00000064-0000-0000-0000-000000000000" |> SquadId

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    // Force re-creation of initial User/s events if directory already exists (if requested).
    if deleteExistingUsersEvents && Directory.Exists usersDir then
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

        // Create initial SuperUser | Administators.
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

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let private createInitialSquadsEventsIfNecessary = async {
    let squadsDir = directory EntityType.Squads

    // Force re-creation of initial Squad/s events if directory already exists (if requested).
    if deleteExistingSquadsEvents && Directory.Exists squadsDir then
        sprintf "deleting existing Squad/s events -> %s" squadsDir |> Info |> log
        delete squadsDir

    if Directory.Exists squadsDir then sprintf "preserving existing Squad/s events -> %s" squadsDir |> Info |> log
    else
        sprintf "creating initial Squad/s events -> %s" squadsDir |> Info |> log
        "starting Squads agent" |> Info |> log
        () |> squads.Start
        // Note: Send dummy OnSquadsEventsRead to Squads agent to ensure that it transitions [from pendingOnSquadsEventsRead] to managingSquads; otherwise HandleCreateSquadCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnSquadsEventsRead to Squads agent" |> Info |> log
        [] |> squads.OnSquadsEventsRead

        // Create initial Squads.

        // Group A
        let turkey = SquadName "Turkey"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, turkeyId, turkey, GroupA, Some (Seeding 14), CoachName "Şenol Güneş") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" turkey)
        let italy = SquadName "Italy"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, italyId, italy, GroupA, Some (Seeding 2), CoachName "Roberto Mancini") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" italy)
        let wales = SquadName "Wales"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, walesId, wales, GroupA, Some (Seeding 19), CoachName "Rob Page") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" wales)
        let switzerland = SquadName "Switzerland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, switzerlandId, switzerland, GroupA, Some (Seeding 9), CoachName "Vladimir Petković") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" switzerland)

        // #Group B
        let denmark = SquadName "Denmark"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, denmarkId, denmark, GroupB, Some (Seeding 15), CoachName "Kasper Hjulmand") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" denmark)
        let finland = SquadName "Finland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, finlandId, finland, GroupB, Some (Seeding 20), CoachName "Markku Kanerva") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" finland)
        let belgium = SquadName "Belgium"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, belgiumId, belgium, GroupB, Some (Seeding 1), CoachName "Roberto Martínez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" belgium)
        let russia = SquadName "Russia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, russiaId, russia, GroupB, Some (Seeding 12), CoachName "Stanislav Cherchesov") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" russia)

        // Group C
        let netherlands = SquadName "Netherlands"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, netherlandsId, netherlands, GroupC, Some (Seeding 11), CoachName "Frank de Boer") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" netherlands)
        let ukraine = SquadName "Ukraine"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ukraineId, ukraine, GroupC, Some (Seeding 6), CoachName "Andriy Shevchenko") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ukraine)
        let austria = SquadName "Austria"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, austriaId, austria, GroupC, Some (Seeding 16), CoachName "Franco Foda") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" austria)
        let northMacedonia = SquadName "North Macedonia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, northMacedoniaId, northMacedonia, GroupC, None, CoachName "Igor Angelovski") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" northMacedonia)

        // Group D
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupD, Some (Seeding 3), CoachName "Gareth Southgate") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" england)
        let croatia = SquadName "Croatia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, croatiaId, croatia, GroupD, Some (Seeding 10), CoachName "Zlatko Dalić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" croatia)
        let scotland = SquadName "Scotland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, scotlandId, scotland, GroupD, None, CoachName "Steve Clarke") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" scotland)
        let czechRepublic = SquadName "Czech Republic"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, czechRepublicId, czechRepublic, GroupD, Some (Seeding 18), CoachName "Jaroslav Šilhavý") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" czechRepublic)

        // Group E
        let spain = SquadName "Spain"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, spainId, spain, GroupE, Some (Seeding 5), CoachName "Luis Enrique") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" spain)
        let sweden = SquadName "Sweden"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, swedenId, sweden, GroupE, Some (Seeding 17), CoachName "Janne Andersson") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" sweden)
        let poland = SquadName "Poland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, polandId, poland, GroupE, Some (Seeding 8), CoachName "Paulo Sousa") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" poland)
        let slovakia = SquadName "Slovakia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, slovakiaId, slovakia, GroupE, None, CoachName "Štefan Tarkovič") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" slovakia)

        // Group F
        let hungary = SquadName "Hungary"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, hungaryId, hungary, GroupF, None, CoachName "Marco Rossi") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" hungary)
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupF, Some (Seeding 13), CoachName "Fernando Santos") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" portugal)
        let france = SquadName "France"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupF, Some (Seeding 7), CoachName "Didier Deschamps") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let germany = SquadName "Germany"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, germanyId, germany, GroupF, Some (Seeding 4), CoachName "Joachim Löw") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" germany)
        
        // Note: Reset Squads agent [to pendingOnSquadsEventsRead] so that it handles subsequent SquadsEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Squads agent" |> Info |> log
        () |> squads.Reset
    return () }

let private createInitialFixturesEventsIfNecessary = async {
    let fixtureId matchNumber =
        if matchNumber < 10u then sprintf "00000000-0000-0000-0000-00000000000%i" matchNumber |> Guid |> FixtureId
        else if matchNumber < 100u then sprintf "00000000-0000-0000-0000-0000000000%i" matchNumber |> Guid |> FixtureId
        else FixtureId.Create ()

    let fixturesDir = directory EntityType.Fixtures

    // Force re-creation of initial Fixture/s events if directory already exists (if requested).
    if deleteExistingFixturesEvents && Directory.Exists fixturesDir then
        sprintf "deleting existing Fixture/s events -> %s" fixturesDir |> Info |> log
        delete fixturesDir

    if Directory.Exists fixturesDir then sprintf "preserving existing Fixture/s events -> %s" fixturesDir |> Info |> log
    else
        sprintf "creating initial Fixture/s events -> %s" fixturesDir |> Info |> log
        "starting Fixtures agent" |> Info |> log
        () |> fixtures.Start
        // Note: Send dummy OnFixturesEventsRead to Users agent to ensure that it transitions [from pendingOnFixturesEventsRead] to managingFixtures; otherwise HandleCreateFixtureCmdAsync would be ignored (and block).
        "sending dummy OnFixturesEventsRead to Fixtures agent" |> Info |> log
        [] |> fixtures.OnFixturesEventsRead

        // Group A
        let turkeyVsItalyKO = (2021, 06, 11, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 1u, Group GroupA, Confirmed turkeyId, Confirmed italyId, turkeyVsItalyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 1u)
        
        let walesVsSwitzerlandKO = (2021, 06, 12, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 2u, Group GroupA, Confirmed walesId, Confirmed switzerlandId, walesVsSwitzerlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 2u)

        let turkeyVsWalesKO = (2021, 06, 16, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 13u, Group GroupA, Confirmed turkeyId, Confirmed walesId, turkeyVsWalesKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 13u)

        let italyVsSwitzerlandKO = (2021, 06, 16, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 14u, Group GroupA, Confirmed italyId, Confirmed switzerlandId, italyVsSwitzerlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 14u)

        let switzerlandVsTurkeyKO = (2021, 06, 20, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 25u, Group GroupA, Confirmed switzerlandId, Confirmed turkeyId, switzerlandVsTurkeyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 25u)

        let italyVsWalesKO = (2021, 06, 20, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 26u, Group GroupA, Confirmed italyId, Confirmed walesId, italyVsWalesKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 26u)

        // Group B
        let denmarkVsFinlandKO = (2021, 06, 12, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 3u, Group GroupB, Confirmed denmarkId, Confirmed finlandId, denmarkVsFinlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 3u)

        let belgiumVsRussiaKO = (2021, 06, 12, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 4u, Group GroupB, Confirmed belgiumId, Confirmed russiaId, belgiumVsRussiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 4u)

        let finlandVsRussiaKO = (2021, 06, 16, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 15u, Group GroupB, Confirmed finlandId, Confirmed russiaId, finlandVsRussiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 15u)

        let denmarkVsBelgiumKO = (2021, 06, 17, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 16u, Group GroupB, Confirmed denmarkId, Confirmed belgiumId, denmarkVsBelgiumKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 16u)

        let russiaVsDenmarkKO = (2021, 06, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 27u, Group GroupB, Confirmed russiaId, Confirmed denmarkId, russiaVsDenmarkKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 27u)

        let finlandVsBelgiumKO = (2021, 06, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 28u, Group GroupB, Confirmed finlandId, Confirmed belgiumId, finlandVsBelgiumKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 28u)

        // Group C
        let austriaVsNorthMacedoniaKO = (2021, 06, 13, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 6u, Group GroupC, Confirmed austriaId, Confirmed northMacedoniaId, austriaVsNorthMacedoniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 6u)

        let netherlandsVsUkraineKO = (2021, 06, 13, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 5u, Group GroupC, Confirmed netherlandsId, Confirmed ukraineId, netherlandsVsUkraineKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 5u)

        let ukraineVsNorthMacedoniaKO = (2021, 06, 17, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 18u, Group GroupC, Confirmed ukraineId, Confirmed northMacedoniaId, ukraineVsNorthMacedoniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 18u)

        let netherlandsVsAustriaKO = (2021, 06, 17, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 17u, Group GroupC, Confirmed netherlandsId, Confirmed austriaId, netherlandsVsAustriaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 17u)

        let northMacedoniaVsNetherlandsKO = (2021, 06, 21, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 29u, Group GroupC, Confirmed northMacedoniaId, Confirmed netherlandsId, northMacedoniaVsNetherlandsKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 29u)

        let ukraineVsAustriaKO = (2021, 06, 21, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 30u, Group GroupC, Confirmed ukraineId, Confirmed austriaId, ukraineVsAustriaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 30u)

        // Group D
        let englandVsCroatiaKO = (2021, 06, 13, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 7u, Group GroupD, Confirmed englandId, Confirmed croatiaId, englandVsCroatiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 7u)

        let scotlandVsCzechRepublicKO = (2021, 06, 14, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 8u, Group GroupD, Confirmed scotlandId, Confirmed czechRepublicId, scotlandVsCzechRepublicKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 8u)

        let croatiaVsCzechRepulicKO = (2021, 06, 18, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 19u, Group GroupD, Confirmed croatiaId, Confirmed czechRepublicId, croatiaVsCzechRepulicKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 19u)

        let englandVsScotlandKO = (2021, 06, 18, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 20u, Group GroupD, Confirmed englandId, Confirmed scotlandId, englandVsScotlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 20u)

        let croatiaVsScotlandKO = (2021, 06, 22, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 31u, Group GroupD, Confirmed croatiaId, Confirmed scotlandId, croatiaVsScotlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 31u)

        let czechRepublicVsEnglandKO = (2021, 06, 22, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 32u, Group GroupD, Confirmed czechRepublicId, Confirmed englandId, czechRepublicVsEnglandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 32u)

        // Group E
        let polandVsSlovakiaKO = (2021, 06, 14, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 10u, Group GroupE, Confirmed polandId, Confirmed slovakiaId, polandVsSlovakiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 10u)

        let spainVsSwedenKO = (2021, 06, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 9u, Group GroupE, Confirmed spainId, Confirmed swedenId, spainVsSwedenKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 9u)

        let swedenVsSlovakiaKO = (2021, 06, 18, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 21u, Group GroupE, Confirmed swedenId, Confirmed slovakiaId, swedenVsSlovakiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 21u)

        let spainVsPolandKO = (2021, 06, 19, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 22u, Group GroupE, Confirmed spainId, Confirmed polandId, spainVsPolandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 22u)

        let slovakiaVsSpainKO = (2021, 06, 23, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 33u, Group GroupE, Confirmed slovakiaId, Confirmed spainId, slovakiaVsSpainKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 33u)

        let swedenVsPolandKO = (2021, 06, 23, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 34u, Group GroupE, Confirmed swedenId, Confirmed polandId, swedenVsPolandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 34u)

        // Group F
        let hungaryVsPortugalKO = (2021, 06, 15, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 11u, Group GroupF, Confirmed hungaryId, Confirmed portugalId, hungaryVsPortugalKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 11u)

        let franceVsGermanyKO = (2021, 06, 15, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 12u, Group GroupF, Confirmed franceId, Confirmed germanyId, franceVsGermanyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 12u)

        let hungaryVsFranceKO = (2021, 06, 19, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 23u, Group GroupF, Confirmed hungaryId, Confirmed franceId, hungaryVsFranceKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 23u)

        let portugalVsGermnayKO = (2021, 06, 19, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 24u, Group GroupF, Confirmed portugalId, Confirmed germanyId, portugalVsGermnayKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 24u)

        let portugalVsFranceKO = (2021, 06, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 35u, Group GroupF, Confirmed portugalId, Confirmed franceId, portugalVsFranceKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 35u)

        let germanyVsHungaryKO = (2021, 06, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 36u, Group GroupF, Confirmed germanyId, Confirmed hungaryId, germanyVsHungaryKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 36u)

        // Round-of-16
        let runnerUpAVsRunnerUpBKO = (2021, 06, 26, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, RoundOf16 38u, Unconfirmed (RunnerUp GroupA), Unconfirmed (RunnerUp GroupB), runnerUpAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)

        let winnerAVsRunnerUpCKO = (2021, 06, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 37u, RoundOf16 37u, Unconfirmed (Winner (Group GroupA)), Unconfirmed (RunnerUp GroupC), winnerAVsRunnerUpCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 37u)

        let winnerCVsThirdPlaceDEFKO = (2021, 06, 27, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 40u, RoundOf16 40u, Unconfirmed (Winner (Group GroupC)), Unconfirmed (ThirdPlace [ GroupD ; GroupE ; GroupF ]), winnerCVsThirdPlaceDEFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 40u)

        let winnerBVsThirdPlaceADEFKO = (2021, 06, 27, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 39u, RoundOf16 39u, Unconfirmed (Winner (Group GroupB)), Unconfirmed (ThirdPlace [ GroupA ; GroupD ; GroupE ; GroupF ]), winnerBVsThirdPlaceADEFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 39u)

        let runnerUpDVsRunnerUpEKO = (2021, 06, 28, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 42u, RoundOf16 42u, Unconfirmed (RunnerUp GroupD), Unconfirmed (RunnerUp GroupE), runnerUpDVsRunnerUpEKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 42u)

        let winnerFVsThirdPlaceABCKO = (2021, 06, 28, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 41u, RoundOf16 41u, Unconfirmed (Winner (Group GroupF)), Unconfirmed (ThirdPlace [ GroupA ; GroupB ; GroupC ]), winnerFVsThirdPlaceABCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 41u)

        let winnerDVsRunnerUpFKO = (2021, 06, 29, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 44u, RoundOf16 44u, Unconfirmed (Winner (Group GroupD)), Unconfirmed (RunnerUp GroupF), winnerDVsRunnerUpFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 44u)

        let winnerEVsThirdPlaceABCDKO = (2021, 06, 29, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 43u, RoundOf16 43u, Unconfirmed (Winner (Group GroupE)), Unconfirmed (ThirdPlace [ GroupA ; GroupB ; GroupC ; GroupD ]), winnerEVsThirdPlaceABCDKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 43u)

        // Quarter-finals
        let winner41VsWinner42KO = (2021, 07, 02, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 45u, QuarterFinal 1u, Unconfirmed (Winner (RoundOf16 41u)), Unconfirmed (Winner (RoundOf16 42u)), winner41VsWinner42KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)

        let winner39VsWinner37KO = (2021, 07, 02, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, QuarterFinal 2u, Unconfirmed (Winner (RoundOf16 39u)), Unconfirmed (Winner (RoundOf16 37u)), winner39VsWinner37KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 46u)

        let winner40VsWinner38KO = (2021, 07, 03, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 47u, QuarterFinal 3u, Unconfirmed (Winner (RoundOf16 40u)), Unconfirmed (Winner (RoundOf16 38u)), winner40VsWinner38KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 47u)

        let winner43VsWinner44KO = (2021, 07, 03, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 48u, QuarterFinal 4u, Unconfirmed (Winner (RoundOf16 43u)), Unconfirmed (Winner (RoundOf16 44u)), winner43VsWinner44KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 48u)

        // Semi-finals
        let winnerQF2VsWinnerQF1KO = (2021, 07, 06, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 49u, SemiFinal 1u, Unconfirmed (Winner (QuarterFinal 2u)), Unconfirmed (Winner (QuarterFinal 1u)), winnerQF2VsWinnerQF1KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 49u)

        let winnerQF4VsWinnerQF3KO = (2021, 07, 07, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 50u, SemiFinal 2u, Unconfirmed (Winner (QuarterFinal 4u)), Unconfirmed (Winner (QuarterFinal 3u)), winnerQF4VsWinnerQF3KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 50u)

        // Final
        let winnerSF1VsWinnerSF2KO = (2021, 07, 11, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 51u, Final, Unconfirmed (Winner (SemiFinal 1u)), Unconfirmed (Winner (SemiFinal 2u)), winnerSF1VsWinnerSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 51u)

        // Note: Reset Fixtures agent [to pendingOnFixturesEventsRead] so that it handles subsequent FixturesEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Fixtures agent" |> Info |> log
        () |> fixtures.Reset
    return () }

let private createInitialDraftsEventsIfNecessary = async {
    let draftsDir = directory EntityType.Drafts

    // Force re-creation of initial Draft/s events if directory already exists (if requested).
    if deleteExistingDraftsEvents && Directory.Exists draftsDir then
        sprintf "deleting existing Draft/s events -> %s" draftsDir |> Info |> log
        delete draftsDir

    if Directory.Exists draftsDir then sprintf "preserving existing Draft/s events -> %s" draftsDir |> Info |> log
    else
        sprintf "creating initial Draft/s events -> %s" draftsDir |> Info |> log
        "starting Drafts agent" |> Info |> log
        () |> drafts.Start
        // Note: Send dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent to ensure that it transitions [from pendingAllRead] to managingDrafts; otherwise HandleCreateDraftCmdAsync would be ignored (and block).
        "sending dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent" |> Info |> log
        [] |> drafts.OnSquadsRead
        [] |> drafts.OnDraftsEventsRead
        [] |> drafts.OnUserDraftsEventsRead

        let draft1Id, draft1Ordinal = Guid "00000000-0000-0000-0000-000000000001" |> DraftId, DraftOrdinal 1
        let draft1Starts, draft1Ends = (2021, 06, 02, 17, 30) |> dateTimeOffsetUtc, (2021, 06, 07, 17, 00) |> dateTimeOffsetUtc
        let draft1Type = (draft1Starts, draft1Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft1Id, draft1Ordinal, draft1Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft1Id draft1Ordinal)

        let draft2Id, draft2Ordinal = Guid "00000000-0000-0000-0000-000000000002" |> DraftId, DraftOrdinal 2
        let draft2Starts, draft2Ends = (2021, 06, 07, 17, 15) |> dateTimeOffsetUtc, (2021, 06, 09, 17, 00) |> dateTimeOffsetUtc
        let draft2Type = (draft2Starts, draft2Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft2Id, draft2Ordinal, draft2Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft2Id draft2Ordinal)

        let draft3Id, draft3Ordinal = Guid "00000000-0000-0000-0000-000000000003" |> DraftId, DraftOrdinal 3
        let draft3Type = Unconstrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft3Id, draft3Ordinal, draft3Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft3Id draft3Ordinal)

        // Note: Reset Drafts agent [to pendingAllRead] so that it handles subsequent DraftsEventsRead event (&c.) appropriately (i.e. from readPersistedEvents).
        "resetting Drafts agent" |> Info |> log
        () |> drafts.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialSquadsEventsIfNecessary // note: although this can cause various events to be broadcast (SquadsRead | SquadEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialFixturesEventsIfNecessary // note: although this can cause various events to be broadcast (FixturesRead | FixtureEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialDraftsEventsIfNecessary // note: although this can cause various events to be broadcast (DraftsRead | DraftEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
