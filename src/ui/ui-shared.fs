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

type Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus ; PickedBy : UserId option }
type PlayerDic = Dictionary<PlayerId, Player>

type Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic ; PickedBy : UserId option }
type SquadDic = Dictionary<SquadId, Squad>

type Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset }
type FixtureDic = Dictionary<FixtureId, Fixture>

type Draft = { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus }
type DraftDic = Dictionary<DraftId, Draft>

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

let userDraftPickText (squadDic:SquadDic) userDraftPick =
    match userDraftPick with
    | TeamPick squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        squadName
    | PlayerPick (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        playerName
