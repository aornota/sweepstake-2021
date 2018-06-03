module Aornota.Sweepstake2018.Common.Domain.Squad

open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.Core

open System

type SquadId = | SquadId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SquadId

type SquadName = | SquadName of squadName : string
type CoachName = | CoachName of coachName : string

type Seeding = | Seeding of seeding : int

type PlayerId = | PlayerId of guid : Guid with
    static member Create () = Guid.NewGuid () |> PlayerId

type PlayerName = | PlayerName of playerName : string

type PlayerType = | Goalkeeper | Defender | Midfielder | Forward

type PlayerStatus = | Active | Withdrawn of dateWithdrawn : DateTimeOffset option

type PlayerDto = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus } // TODO-NMB-MEDIUM: pickedBy? score?...

type SquadOnlyDto = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool }

type SquadDto = { SquadOnlyDto : SquadOnlyDto ; PlayerDtos : PlayerDto list }

type SquadsProjectionDto = { SquadDtos : SquadDto list }

let [<Literal>] private MAX_PLAYERS_PER_SQUAD = 23
let [<Literal>] PERU_ID = "00000034-0000-0000-0000-000000000000" // note: not private since used in default-data.fs

let private isPeru (SquadId squadId) = squadId.ToString () = PERU_ID

let maxPlayers squadId = if squadId |> isPeru |> not then MAX_PLAYERS_PER_SQUAD else MAX_PLAYERS_PER_SQUAD + 1 // note: due to temporary revocation of drug ban (or something like that)
let squadIsFullText squadId = sprintf "Squad contains the maximum of %i non-withdrawn players" (squadId |> maxPlayers)

let validateSquadName (squadNames:SquadName list) (SquadName squadName) =
    if String.IsNullOrWhiteSpace squadName then "Squad name must not be blank" |> Some
    else if (squadName.Trim ()).Length < 4 then "Squad name must be at least 4 characters" |> Some
    else if squadNames |> List.map (fun (SquadName squadName) -> (squadName.ToLower ()).Trim ()) |> List.contains ((squadName.ToLower ()).Trim ()) then "Squad name already in use" |> Some
    else None
let validateCoachName (CoachName coachName) =
    if String.IsNullOrWhiteSpace coachName then "Coach name must not be blank" |> Some
    else if (coachName.Trim ()).Length < 4 then "Coach name must be at least 4 characters" |> Some
    else None
let validatePlayerName (playerNames:PlayerName list) (PlayerName playerName) =
    if String.IsNullOrWhiteSpace playerName then "Player name must not be blank" |> Some
    else if (playerName.Trim ()).Length < 4 then "Player name must be at least 4 characters" |> Some
    else if playerNames |> List.map (fun (PlayerName playerName) -> (playerName.ToLower ()).Trim ()) |> List.contains ((playerName.ToLower ()).Trim ()) then "Player name already in use" |> Some
    else None
