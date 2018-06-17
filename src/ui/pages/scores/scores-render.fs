module Aornota.Sweepstake2018.UI.Pages.Scores.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.Scores.Common
open Aornota.Sweepstake2018.UI.Shared
open System

module Rct = Fable.Helpers.React

let private renderStandings (useDefaultTheme, users:(UserId * UserName) list, squadDic:SquadDic, fixtureDic:FixtureDic) dispatch =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let userRow userCount (rank, tieCount, rankChange, userId, UserName userName, squad, players, points, pointsChange) =
            let payout =
                let paraPayout = { paraDefaultSmallest with ParaAlignment = RightAligned }
                let payout =
                    match rank, tieCount with
                    | 1, 1 -> 65. |> Some
                    | 1, 2 -> 50. |> Some
                    | 1, 3 -> 40. |> Some
                    | 1, 4 -> 30. |> Some
                    | 1, _ -> None // note: unlikely to happen
                    | 2, 1 -> 35. |> Some
                    | 2, 2 -> 27.5 |> Some
                    | 2, _ -> None // note: unlikely to happen
                    | 3, 1 -> 20. |> Some
                    | 3, 2 -> 10. |> Some
                    | 3, _ -> None // note: unlikely to happen
                    | rank, 1 when rank = userCount -> 10. |> Some
                    | rank, 2 when rank = userCount -> 5. |> Some
                    | rank, _ when rank = userCount -> None // note: unlikely to happen
                    | _ -> None
                match payout with
                | Some payout -> [ str (sprintf "£%.2f" payout) ] |> para theme paraPayout |> Some
                | None -> None
            let rankText = if tieCount > 1 then sprintf "=%i." rank else sprintf "%i." rank
            let rankChange =
                match rankChange with
                | Some rankChange ->
                    if rankChange > 0 then
                        let contents = div divDefault [ icon iconCollapseUpSmall ; bold (sprintf " +%i" rankChange) ]
                        [ contents ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Success } |> Some
                    else if rankChange < 0 then
                        let contents = div divDefault [ icon iconExpandDownSmall ; italic (sprintf " %i" rankChange) ]
                        [ contents ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger } |> Some
                    else None
                | None -> None
            let onClick = (fun _ -> userId |> ShowUser |> dispatch)
            let squad, eliminated =
                match squad with
                | Some squad ->
                    let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
                    let squadText = sprintf "%s (%s)" squadName coachName
                    let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
                    [ str squadText ] |> para theme paraDefaultSmallest |> Some, eliminated
                | None -> None, None
            let playerCount playerType =
                players
                |> List.filter (fun (_, _, player) -> match player.PlayerStatus with | Active -> player.PlayerType = playerType | Withdrawn _ -> false)
                |> List.length
            let defenderCount, midfielderCount, forwardCount = Defender |> playerCount, Midfielder |> playerCount, Forward |> playerCount
            let formation = sprintf "%i-%i-%i" defenderCount midfielderCount forwardCount
            let playersRemaining =
                players
                |> List.filter (fun (_, _, player) -> match player.PlayerStatus with | Active -> true | Withdrawn _ -> false)
                |> List.filter (fun (squadId, _, _) -> if squadId |> squadDic.ContainsKey then squadDic.[squadId].Eliminated |> not else false)
                |> List.length
            let pointsText = sprintf "%i" (int points)
            let score = if points > 0<point> then bold pointsText else if points < 0<point> then italic pointsText else str pointsText
            let pointsChange =
                match pointsChange with
                | Some pointsChange ->
                    let paraPointsChange = { paraDefaultSmallest with ParaAlignment = RightAligned }
                    if pointsChange > 0<point> then [ bold (sprintf "+%i" (int pointsChange)) ] |> para theme paraPointsChange |> Some
                    else if pointsChange < 0<point> then [ italic (sprintf "%i" (int pointsChange)) ] |> para theme paraPointsChange |> Some
                    else None
                | None -> None
            tr false [
                td [ Rct.ofOption payout ]
                td [ [ italic rankText ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ Rct.ofOption rankChange ]
                td [ [ [ str userName ] |> para theme paraDefaultSmallest ] |> link theme (ClickableLink onClick) ]
                td [ Rct.ofOption squad ]
                td [ Rct.ofOption eliminated ]
                td [ [ str formation ] |> para theme paraCentredSmallest ]
                td [ [ str (sprintf "%i" playersRemaining ) ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ Rct.ofOption pointsChange ] ]
        let resultKickOffs = fixtureDic |> List.ofSeq |> List.choose (fun (KeyValue (_, fixture)) -> match fixture.MatchResult with | Some _ -> fixture.KickOff |> Some | None -> None)
        let latestResult = match resultKickOffs with | _ :: _ -> resultKickOffs |> List.max |> Some | [] -> None
        let previousRankAndPoints =
            match latestResult with
            | Some latestResult ->
                let previousFixtureDic = FixtureDic ()
                fixtureDic |> List.ofSeq |> List.iter (fun (KeyValue (fixtureId, fixture)) -> if fixture.KickOff < latestResult then (fixtureId, fixture) |> previousFixtureDic.Add)
                let mutable previousRank = 1
                let mutable previousPoints : int<point> option = None
                users
                |> List.map (fun (userId, userName) ->
                    let squad, players = userId |> pickedByUser squadDic
                    let teamPoints =
                        match squad with
                        | Some (squadId, _, _, timestamp) ->
                            let _, teamPoints = teamPoints previousFixtureDic squadId (timestamp |> Some)
                            teamPoints
                        | None -> None
                    let teamPoints = match teamPoints with | Some teamPoints -> teamPoints | None -> 0<point>
                    let playerPoints =
                        players
                        |> List.choose (fun (squadId, _, playerId, _, _, timestamp) ->
                            let _, playerPoints = playerPoints previousFixtureDic (squadId, playerId) (timestamp |> Some)
                            playerPoints)
                        |> List.sum
                    let points = teamPoints + playerPoints
                    userId, userName, points)
                |> List.sortBy (fun (_, userName, points) -> -points, userName)
                |> List.mapi (fun i (userId, _, points) ->
                    let rank =
                        if i = 0 then
                            previousPoints <- points |> Some
                            i + 1
                        else
                            let rank =
                                if points |> Some = previousPoints then previousRank
                                else
                                    previousRank <- i + 1
                                    previousPoints <- points |> Some
                                    i + 1
                            rank               
                    rank, userId, points)
            | None -> []
        let previousRankAndPoints forUserId =
            match previousRankAndPoints |> List.tryFind (fun (_, userId, _) -> userId = forUserId) with
            | Some (rank, _, points) -> (rank, points) |> Some
            | None -> None
        let users =
            let mutable previousRank = 1
            let mutable previousPoints : int<point> option = None
            users
            |> List.map (fun (userId, userName) ->
                let squad, players = userId |> pickedByUser squadDic
                let squad, teamPoints =
                    match squad with
                    | Some (squadId, squad, _, timestamp) ->
                        let _, teamPoints = teamPoints fixtureDic squadId (timestamp |> Some)
                        squad |> Some, teamPoints
                    | None -> None, None
                let teamPoints = match teamPoints with | Some teamPoints -> teamPoints | None -> 0<point>
                let playerPoints =
                    players
                    |> List.choose (fun (squadId, _, playerId, _, _, timestamp) ->
                        let _, playerPoints = playerPoints fixtureDic (squadId, playerId) (timestamp |> Some)
                        playerPoints)
                    |> List.sum
                let points = teamPoints + playerPoints
                let players = players |> List.map (fun (squadId, _, playerId, player, _, _) -> squadId, playerId, player)
                userId, userName, squad, players, points)
            |> List.sortBy (fun (_, userName, _, _, points) -> -points, userName)
            |> List.mapi (fun i (userId, userName, squadId, squad, points) ->
                let rank =
                    if i = 0 then
                        previousPoints <- points |> Some
                        i + 1
                    else
                        let rank =
                            if points |> Some = previousPoints then previousRank
                            else
                                previousRank <- i + 1
                                previousPoints <- points |> Some
                                i + 1
                        rank
                rank, userId, userName, squadId, squad, points)
            |> List.map (fun (rank, userId, userName, squadId, squad, points) ->
                match userId |> previousRankAndPoints with
                | Some (previousRank, previousPoints) ->
                    let rankChange, pointsChange = previousRank - rank, points - previousPoints
                    rank, rankChange |> Some, userId, userName, squadId, squad, points, pointsChange |> Some
                | None -> rank, None, userId, userName, squadId, squad, points, None)
        let ranks = users |> List.map (fun (rank, _, _, _, _, _, _, _) -> rank)
        let users =
            users
            |> List.map (fun (rank, rankChange, userId, userName, squadId, squad, points, pointsChange) ->
                let tieCount = ranks |> List.filter (fun otherRank -> otherRank = rank) |> List.length
                rank, tieCount, rankChange, userId, userName, squadId, squad, points, pointsChange)
        let userCount = users.Length
        let userRows = users |> List.map (userRow userCount)
        yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
            thead [ 
                tr false [
                    th []
                    th []
                    th []
                    th [ [ bold "Name" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Team/coach"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ bold "Formation" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Players remaining" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    th [] ] ]
            tbody [ yield! userRows ] ] ]

let private userTabs users currentUserId dispatch =
    users |> List.map (fun (userId, UserName userName) ->
        { IsActive = userId = currentUserId ; TabText = userName ; TabLinkType = ClickableLink (fun _ -> userId |> ShowUser |> dispatch) })

// #region customAgo
let private customAgo (timestamp:DateTime) =
#if TICK
    timestamp |> ago
#else
    sprintf "on %s" (timestamp |> dateAndTimeText)
#endif
// #endregion

let private pickedIn theme (draftOrdinal, timestamp:DateTimeOffset) =
    match draftOrdinal with
    | Some draftOrdinal -> [ str (sprintf "%s" (draftOrdinal |> draftText)) ] |> para theme paraCentredSmallest |> Some
    | None -> [ str (sprintf "Free pick (%s)" (customAgo timestamp.LocalDateTime)) ] |> para theme paraCentredSmallest |> Some

let private scoreText score =
    let scoreText = sprintf "%i" score
    if score > 0 then bold scoreText else str scoreText

let private score (points:int<point>) (pickedByPoints:int<point> option) pickedByUserId (userDic:UserDic) =
    let pointsOnly =
        let pointsText = sprintf "%i" (int points)
        if points > 0<point> then bold pointsText else if points < 0<point> then italic pointsText else str pointsText
    match pickedByPoints with
    | Some pickedByPoints ->
        if points = pickedByPoints then pointsOnly
        else
            let (UserName userName) = pickedByUserId |> userName userDic
            let pickedByPointsText = sprintf "%i" (int pickedByPoints)
            let pickedByPoints = if pickedByPoints > 0<point> then bold pickedByPointsText else if pickedByPoints < 0<point> then italic pickedByPointsText else str pickedByPointsText
            let pickedByUser = str (sprintf " for %s)" userName)
            div divDefault [ pointsOnly ; str " (" ; pickedByPoints ; pickedByUser ]
    | _ -> pointsOnly

let private renderCurrentUserSquad (useDefaultTheme, userId, squadId, squad, draftOrdinal, timestamp, userDic:UserDic, fixtureDic:FixtureDic) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let (SquadName squadName), (CoachName coachName), (Seeding seeding) = squad.SquadName, squad.CoachName, squad.Seeding
        let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
        let points, pickedByPoints = teamPoints fixtureDic squadId (timestamp |> Some)
        let score = score points pickedByPoints userId userDic
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [ 
                tr false [
                    th [ [ bold "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ bold "Seeding" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Picked in" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [
                tr false [
                    td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                    td [ Rct.ofOption eliminated ]
                    td [ [ str (sprintf "%i" seeding) ] |> para theme paraCentredSmallest ]
                    td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                    td [ Rct.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                    td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ] ] ]

let private renderCurrentUserPlayers (useDefaultTheme, userId, players:(SquadId * Squad * PlayerId * Player * DraftOrdinal option * DateTimeOffset) list, userDic:UserDic, fixtureDic:FixtureDic) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let playerRow (squadId, squad, playerId, player, draftOrdinal, timestamp) =
            let (SquadName squadName), (PlayerName playerName), playerTypeText = squad.SquadName, player.PlayerName, player.PlayerType |> playerTypeText
            let withdrawn =
                    match player.PlayerStatus with 
                    | Withdrawn _ -> [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some
                    | Active -> None
            let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
            let points, pickedByPoints = playerPoints fixtureDic (squadId, playerId) (timestamp |> Some)
            let score = score points pickedByPoints userId userDic
            tr false [
                td [ [ str playerName ] |> para theme paraDefaultSmallest ]
                td [ Rct.ofOption withdrawn ]
                td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                td [ Rct.ofOption eliminated ]
                td [ [ str playerTypeText ] |> para theme paraCentredSmallest ]
                td [ Rct.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
        let players =
            players |> List.sortBy (fun (_, squad, _, player, _, _) ->
                let active =
                    match squad.Eliminated, player.PlayerStatus with
                    | true, _ -> 1
                    | false, Withdrawn _ -> 1
                    | false, Active -> 0
                active, player.PlayerType, squad.SquadName, player.PlayerName)
        let playerRows = players |> List.map playerRow
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [ 
                tr false [
                    th [ [ bold "Player" ] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ bold "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ bold "Position" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Picked in" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [ yield! playerRows ] ] ]
        
let render (useDefaultTheme, state, usersProjection:Projection<_ * UserDic>, squadsProjection:Projection<_ * SquadDic>, fixturesProjection:Projection<_ * FixtureDic>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "Scores" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection, squadsProjection, fixturesProjection with
        | Pending, _, _ | _, Pending, _ | _, _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _, _ | _, Failed, _ | _, _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic), Ready (_, squadDic), Ready (_, fixtureDic) ->
            let users =
                squadDic |> List.ofSeq |> List.map (fun (KeyValue (_, squad)) ->
                    let users =
                        squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (_, player)) -> player.PickedBy)
                        |> List.map (fun (userId, _, _) -> userId)
                    let squadUserId = match squad.PickedBy with | Some (userId, _, _) -> [ userId ] | None -> []
                    squadUserId @ users)
                |> List.collect id |> List.distinct |> List.map (fun userId -> userId, userId |> userName userDic) |> List.sortBy snd
            let currentUserId =
                match state.CurrentUserId with
                | Some currentUserId -> match users |> List.filter (fun (userId, _) -> userId = currentUserId) with | _ :: _ -> currentUserId |> Some | [] -> None
                | None -> None
            let currentUserId =
                match currentUserId with
                | Some currentUserId -> currentUserId |> Some
                | None -> match users |> List.sortBy snd with | (userId, _) :: _ -> userId |> Some | [] -> None
            yield lazyViewOrHMR2 renderStandings (useDefaultTheme, users, squadDic, fixtureDic) dispatch
            yield br
            match currentUserId with
            | Some currentUserId ->
                let userTabs = userTabs users currentUserId dispatch
                let squad, players = currentUserId |> pickedByUser squadDic
                let pickedCounts = (squad, players) |> pickedCounts
                let stillRequired = pickedCounts |> stillRequired
                yield div divCentred [ tabs theme { tabsDefault with Tabs = userTabs } ]
                yield br
                match stillRequired with
                | Some stillRequired ->
                    yield [ bold stillRequired ] |> para theme paraCentredSmallest
                    yield br
                | None -> ()
                match squad with
                | Some (squadId, squad, draftOrdinal, timestamp) ->
                    yield lazyViewOrHMR renderCurrentUserSquad (useDefaultTheme, currentUserId, squadId, squad, draftOrdinal, timestamp, userDic, fixtureDic)
                | None -> ()
                yield br
                match players with
                | _ :: _ ->
                    yield lazyViewOrHMR renderCurrentUserPlayers (useDefaultTheme, currentUserId, players, userDic, fixtureDic)
                | [] -> ()
            | None -> yield [ str "Coming soon" ] |> para theme paraCentredSmaller ] // note: should never happen
