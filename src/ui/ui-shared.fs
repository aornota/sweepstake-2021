module Aornota.Sweepstake2018.UI.Shared

open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open System
open System.Collections.Generic

type Projection<'a> =
    | Pending
    | Failed
    | Ready of data : 'a

type User = UserName * UserAuthDto option
type UserDic = Dictionary<UserId, User>

type Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus ; PickedBy : PickedBy option }
type PlayerDic = Dictionary<PlayerId, Player>

type Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic ; PickedBy : PickedBy option }
type SquadDic = Dictionary<SquadId, Squad>

type Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }
type FixtureDic = Dictionary<FixtureId, Fixture>

type Draft = { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; ProcessingDetails : ProcessingDetails option }
type DraftDic = Dictionary<DraftId, Draft>

type UserDraftPickDic = Dictionary<UserDraftPick, int>

let [<Literal>] UNKNOWN = "<unknown>"

let cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
let qryErrorText error = match error with | AuthQryJwtError _ | AuthQryAuthznError _ -> UNEXPECTED_ERROR | OtherAuthQryError (OtherError errorText) -> errorText

let userName (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey then
        let userName, _ = userDic.[userId]
        userName
    else UserName UNKNOWN
let userType (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey then
        let _, userAuthDto = userDic.[userId]
        match userAuthDto with | Some userAuthDto -> userAuthDto.UserType |> Some | None -> None
    else None
let userNames (userDic:UserDic) = userDic |> List.ofSeq |> List.map (fun (KeyValue (_, (userName, _))) -> userName)

let squadName (squadDic:SquadDic) squadId = if squadId |> squadDic.ContainsKey then squadDic.[squadId].SquadName else SquadName UNKNOWN
let defaultSquadId (squadDic:SquadDic) group =
    let groupSquads = squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad) |> List.filter (fun (_, squad) -> squad.Group = group)
    match groupSquads |> List.sortBy (fun (_, squad) -> squad.SquadName) with | (squadId, _) :: _ -> squadId |> Some | [] -> None

let playerName (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let playerDic = squadDic.[squadId].PlayerDic
        if playerId |> playerDic.ContainsKey then playerDic.[playerId].PlayerName else PlayerName UNKNOWN
    else PlayerName UNKNOWN
let playerType (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let playerDic = squadDic.[squadId].PlayerDic
        if playerId |> playerDic.ContainsKey then playerDic.[playerId].PlayerType |> Some else None
    else None
let playerNames (playerDic:PlayerDic) = playerDic |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)

let currentDraft (draftDic:DraftDic) =
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) ->
        match draft.DraftStatus with | Opened _ -> 1 | PendingProcessing false -> 2 | PendingProcessing true -> 3 | PendingOpen _ -> 4 | FreeSelection -> 5 | _ -> 6)
    match drafts with
    | (draftId, draft) :: _ ->
        match draft.DraftStatus with
        | PendingOpen _ | Opened _ | PendingProcessing _ | FreeSelection -> (draftId, draft) |> Some
        | Processed | PendingFreeSelection -> None
    | [] -> None
let activeDraft (draftDic:DraftDic) =
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) ->
        match draft.DraftStatus with | Opened _ -> 1 | PendingProcessing false -> 2 | PendingProcessing true -> 3 | _ -> 4)
    match drafts with
    | (draftId, draft) :: _ -> if draft.DraftStatus |> isActive then (draftId, draft) |> Some else None
    | [] -> None

let draftPickText (squadDic:SquadDic) draftPick =
    match draftPick with
    | TeamPicked squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        squadName
    | PlayerPicked (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        playerName

let userDraftPickDic currentUserDraftDto =
    match currentUserDraftDto with
    | Some currentUserDraftDto ->
        let userDraftPickDic = UserDraftPickDic ()
        currentUserDraftDto.UserDraftPickDtos |> List.iter (fun userDraftPickDto -> (userDraftPickDto.UserDraftPick, userDraftPickDto.Rank) |> userDraftPickDic.Add)
        userDraftPickDic
    | None -> UserDraftPickDic ()
let userDraftPickText (squadDic:SquadDic) userDraftPick =
    match userDraftPick with
    | TeamPick squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        squadName
    | PlayerPick (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        playerName

let pickedByUser (squadDic:SquadDic) userId =
    let squad =
        squadDic |> List.ofSeq |> List.map (fun (KeyValue (_, squad)) -> squad)
        |> List.choose (fun squad ->
            match squad.PickedBy with | Some (pickedByUserId, draftOrdinal, timestamp) when pickedByUserId = userId -> (squad, draftOrdinal, timestamp) |> Some | _ -> None)
    let squad = match squad with (squad, draftOrdinal, timestamp) :: _ -> (squad, draftOrdinal, timestamp) |> Some | [] -> None
    let playerDics =
        squadDic |> List.ofSeq |> List.map (fun (KeyValue (_, squad)) -> squad, squad.PlayerDic)
    let players =
        playerDics |> List.map (fun (squad, playerDic) ->
            playerDic |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player)
            |> List.choose (fun player ->
                match player.PickedBy with | Some (pickedByUserId, draftOrdinal, timestamp) when pickedByUserId = userId -> (squad, player, draftOrdinal, timestamp) |> Some | _ -> None))
        |> List.collect id               
    squad, players
let pickedCounts (squad:(Squad * _ * _) option, players:(Squad * Player * _ * _) list) =
    let teamCount = match squad with | Some _ -> 1 | None -> 0
    let goalkeeperCount = players |> List.filter (fun (_, player, _, _) -> match player.PlayerType, player.PlayerStatus with | Goalkeeper, Active _ -> true | _ -> false) |> List.length
    let outfieldPlayerCount =
        players |> List.filter (fun (_, player, _, _) ->
            match player.PlayerType, player.PlayerStatus with | Goalkeeper, _ -> false | _, Active -> true | _ -> false) |> List.length
    teamCount, goalkeeperCount, outfieldPlayerCount
let required (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) =
    let required = [
        if pickedTeamCount < MAX_TEAM_PICKS then yield "1 team/coach"
        if pickedGoalkeeperCount < MAX_GOALKEEPER_PICKS then yield "1 goalkeeper"
        let requiredOutfieldPlayers = MAX_OUTFIELD_PLAYER_PICKS - pickedOutfieldPlayerCount            
        if requiredOutfieldPlayers > 0 then
            let plural = if requiredOutfieldPlayers > 1 then "s" else String.Empty
            yield sprintf "%i outfield player%s" requiredOutfieldPlayers plural ]
    let items = required.Length
    if items > 0 then
        let required = required |> List.mapi (fun i item -> if i = 0 then item else if i + 1 < items then sprintf ", %s" item else sprintf " and %s" item)
        let required = required |> List.fold (fun text item -> sprintf "%s%s" text item) String.Empty
        required |> Some
    else None
let stillRequired (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) =
    match (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) |> required with
    | Some required -> sprintf "%s still required" required |> Some
    | None -> None
