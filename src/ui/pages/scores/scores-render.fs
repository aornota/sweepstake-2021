module Aornota.Sweepstake2018.UI.Pages.Scores.Render

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

let private userTabs users currentUserId dispatch =
    users |> List.map (fun (userId, UserName userName) ->
        { IsActive = userId = currentUserId ; TabText = userName ; TabLinkType = ClickableLink (fun _ -> userId |> ShowUser |> dispatch ) })

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

let private renderCurrentUserSquad (useDefaultTheme, squad, draftOrdinal, timestamp) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let (SquadName squadName), (CoachName coachName), (Seeding seeding) = squad.SquadName, squad.CoachName, squad.Seeding
        let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
        let score = 0 // TEMP-NMB...
        yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
            thead [ 
                tr false [
                    th [ [ bold "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ bold "Seeding" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Picked in" ] |> para theme paraCentredSmallest ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [
                tr false [
                    td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                    td [ Rct.ofOption eliminated ]
                    td [ [ str (sprintf "%i" seeding) ] |> para theme paraDefaultSmallest ]
                    td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                    td [ Rct.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                    td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ] ] ]

let private renderCurrentUserPlayers (useDefaultTheme, players:(Squad * Player * DraftOrdinal option * DateTimeOffset) list) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let playerRow (squad, player, draftOrdinal, timestamp) =
            let (SquadName squadName), (PlayerName playerName), playerTypeText = squad.SquadName, player.PlayerName, player.PlayerType |> playerTypeText
            let withdrawn =
                    match player.PlayerStatus with 
                    | Withdrawn _ -> [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some
                    | Active -> None
            let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
            let score = 0 // TEMP-NMB...
            tr false [
                td [ [ str playerName ] |> para theme paraDefaultSmallest ]
                td [ Rct.ofOption withdrawn ]
                td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                td [ Rct.ofOption eliminated ]
                td [ [ str playerTypeText ] |> para theme paraCentredSmallest ]
                td [ Rct.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
        let players =
            players |> List.sortBy (fun (squad, player, _, _) ->
                let active =
                    match squad.Eliminated, player.PlayerStatus with
                    | true, _ -> 1
                    | false, Withdrawn _ -> 1
                    | false, Active -> 0
                active, player.PlayerType, squad.SquadName, player.PlayerName)
        let playerRows = players |> List.map playerRow
        yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
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
        
let render (useDefaultTheme, state, usersProjection:Projection<_ * UserDic>, squadsProjection:Projection<_ * SquadDic>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "Scores" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection, squadsProjection with
        | Pending, _ | _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _ | _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic), Ready (_, squadDic) ->
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
                | Some (squad, draftOrdinal, timestamp) ->
                    yield lazyViewOrHMR renderCurrentUserSquad (useDefaultTheme, squad, draftOrdinal, timestamp)
                | None -> ()
                yield br
                match players with
                | _ :: _ ->
                    yield lazyViewOrHMR renderCurrentUserPlayers (useDefaultTheme, players)
                | [] -> ()
            | None -> yield [ str "Coming soon" ] |> para theme paraCentredSmaller ] // note: should never happen
