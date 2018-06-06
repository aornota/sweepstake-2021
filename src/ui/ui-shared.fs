module Aornota.Sweepstake2018.UI.Shared

open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg

open System.Collections.Generic

type Projection<'a> =
    | Pending
    | Failed
    | Ready of data : 'a

type User = UserName * UserAuthDto option
type UserDic = Dictionary<UserId, User>

type Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type PlayerDic = Dictionary<PlayerId, Player>

type Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic }
type SquadDic = Dictionary<SquadId, Squad>

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

let defaultSquadId (squadDic:SquadDic) group =
    let groupSquads = squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad) |> List.filter (fun (_, squad) -> squad.Group = group)
    match groupSquads |> List.sortBy (fun (_, squad) -> squad.SquadName) with | (squadId, _) :: _ -> squadId |> Some | [] -> None

let playerNames (playerDic:PlayerDic) = playerDic |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)
