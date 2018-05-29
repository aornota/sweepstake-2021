module Aornota.Sweepstake2018.UI.Pages.Squads.Render

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.Squads.Common

open System

module Rct = Fable.Helpers.React

let private groups = [ GroupA ; GroupB ; GroupC ; GroupD ; GroupE ; GroupF ; GroupG ; GroupH ]

let private groupText group =
    let groupText = match group with | GroupA -> "A" | GroupB -> "B" | GroupC -> "C" | GroupD -> "D" | GroupE -> "E" | GroupF -> "F" | GroupG -> "G" | GroupH -> "H"
    sprintf "Group %s" groupText

let private playerTypes = [ Goalkeeper ; Defender ; Midfielder ; Forward ]

let private playerTypeSortOrder playerType = match playerType with | Goalkeeper -> 1 | Defender -> 2 | Midfielder -> 3 | Forward -> 4
let private playerTypeText playerType = match playerType with | Goalkeeper -> "Goalkeeper" | Defender -> "Defender" | Midfielder -> "Midfielder" | Forward -> "Forward"

let private playerTypeRadios selectedPlayerType dispatch =
    let onChange playerType = (fun _ -> playerType |> NewPlayerTypeChanged |> dispatch)
    playerTypes
    |> List.sortBy playerTypeSortOrder
    |> List.map (fun playerType ->
        let isSelected = playerType = selectedPlayerType
        let onChange = if isSelected then ignore else playerType |> onChange
        radioInline (playerType |> playerTypeText) isSelected onChange)
    |> List.collect id

let private renderAddPlayersModal (useDefaultTheme, squadDic:SquadDic, addPlayersState) dispatch =
    let theme = getTheme useDefaultTheme
    let squadId = addPlayersState.SquadId
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let titleText =
        match squad with
        | Some squad ->
            let (SquadName squadName) = squad.SquadName
            sprintf "Add player/s for %s" squadName
        | None -> "Add player/s" // note: should never happen
    let onDismiss = match addPlayersState.AddPlayerStatus with | Some AddPlayerPending -> None | Some _ | None -> (fun _ -> CancelAddPlayers |> dispatch) |> Some
    let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
    let nonWithdrawnCount = match squad with | Some squad -> squad.PlayerDic |> List.ofSeq |> List.filter (fun (KeyValue (_, player)) -> player.Withdrawn |> not) |> List.length | None -> 0
    let squadIsFull = nonWithdrawnCount >= MAX_PLAYERS_PER_SQUAD
    let isAddingPlayer, addPlayerInteraction, onEnter =
        let addPlayer = (fun _ -> AddPlayer |> dispatch)
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending -> true, Loading, ignore
        | Some (AddPlayerFailed _) | None ->
            match validatePlayerName playerNames (PlayerName addPlayersState.NewPlayerNameText), squadIsFull with
            | Some _, _ | None, true  -> false, NotEnabled None, ignore
            | None, false -> false, Clickable (addPlayer, None), addPlayer
    let errorText = match addPlayersState.AddPlayerStatus with | Some (AddPlayerFailed errorText) -> errorText |> Some | Some AddPlayerPending | None -> None
    let (PlayerId newPlayerKey) = addPlayersState.NewPlayerId
    let body = [
        if squadIsFull then
            yield notification theme notificationInfo [ [ str squadIsFullText ] |> para theme paraDefaultSmallest ]
            yield br
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the name and position of the new player" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme newPlayerKey addPlayersState.NewPlayerNameText (iconMaleSmall |> Some) false addPlayersState.NewPlayerNameErrorText [] true isAddingPlayer
                (NewPlayerNameTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! playerTypeRadios addPlayersState.NewPlayerType dispatch ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Add player" ] |> button theme { buttonLinkSmall with Interaction = addPlayerInteraction } ] ]
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] onDismiss body

let private group squadId (squadDic:SquadDic) = match squadId with | Some squadId when squadId |> squadDic.ContainsKey -> squadDic.[squadId].Group |> Some | Some _ | None -> None

let private groupTab currentGroup dispatch group =
    let isActive = match currentGroup with | Some currentGroup when currentGroup = group -> true | Some _ | None -> false
    { IsActive = isActive ; TabText = group |> groupText ; TabLinkType = ClickableLink (fun _ -> group |> ShowGroup |> dispatch ) }

let private squadTab currentSquadId dispatch (squadId, squad) =
    let (SquadName squadName) = squad.SquadName
    let isActive = match currentSquadId with | Some currentSquadId when currentSquadId = squadId -> true | Some _ | None -> false
    { IsActive = isActive ; TabText = squadName ; TabLinkType = ClickableLink (fun _ -> squadId |> ShowSquad |> dispatch ) }

let private squadTabs currentSquadId dispatch (squadDic:SquadDic) =
    match squadDic |> group currentSquadId with
    | Some group ->
        let groupSquads = squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad) |> List.filter (fun (_, squad) -> squad.Group = group)
        groupSquads |> List.map (squadTab currentSquadId dispatch)
    | None -> []

let private scoreText score =
    let scoreText = sprintf "%i" score
    if score > 0 then bold scoreText else str scoreText

let private renderSquad (useDefaultTheme, squad:Squad) _dispatch =
    let theme = getTheme useDefaultTheme
    let (CoachName coachName), (Seeding seeding) = squad.CoachName, squad.Seeding
    let score = 0 // TEMP-NMB...
    div divCentred [
        table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
            thead [ 
                tr false [
                    th [ [ bold "Seeding" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Picked by" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [
                tr false [
                    td [ [ str (sprintf "%i" seeding) ] |> para theme paraDefaultSmallest ]
                    td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                    td [ [ italic String.Empty ] |> para theme paraDefaultSmallest ]
                    td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ] ] ]

let private renderPlayers (useDefaultTheme, playerDic:PlayerDic) _dispatch =
    let theme = getTheme useDefaultTheme
    let playerRow (_playerId, player) =
        let (PlayerName playerName), playerTypeText = player.PlayerName, player.PlayerType |> playerTypeText
        let score = 0 // TEMP-NMB...
        tr false [
            td [ [ str playerName ] |> para theme paraDefaultSmallest ]
            td [ [ str playerTypeText ] |> para theme paraCentredSmallest ]
            td [ [ italic String.Empty ] |> para theme paraDefaultSmallest ]
            td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
    let sortedPlayers = playerDic |> List.ofSeq |> List.map (fun (KeyValue (playerId, player)) -> (playerId, player)) |> List.sortBy (fun (_, player) ->
        player.PlayerType |> playerTypeSortOrder, player.PlayerName)
    let playerRows = sortedPlayers |> List.map (fun (playerId, player) -> (playerId, player) |> playerRow)
    div divCentred [
        if playerDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [ 
                    tr false [
                        th [ [ bold "Player" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Position" ] |> para theme paraCentredSmallest ]
                        th [ [ bold "Picked by" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
                tbody [ yield! playerRows ] ]
        else yield [ str "Player details coming soon" ] |> para theme paraCentredSmaller ]    

let addPlayers theme squadId squad authUser dispatch =
    let nonWithdrawnCount = squad.PlayerDic |> List.ofSeq |> List.filter (fun (KeyValue (_, player)) -> player.Withdrawn |> not) |> List.length
    let paraAddPlayers = { paraDefaultSmallest with ParaAlignment = RightAligned }
    match authUser with
    | Some authUser ->
        match authUser.Permissions.SquadPermissions with
        | Some squadPermissions ->
            if squadPermissions.AddOrEditPlayerPermission then
                if nonWithdrawnCount < MAX_PLAYERS_PER_SQUAD then
                    [ [ str "Add player/s" ] |> para theme paraAddPlayers ] |> link theme (ClickableLink (fun _ -> squadId |> ShowAddPlayersModal |> dispatch)) |> Some
                else
                    [ italic squadIsFullText ] |> para theme paraAddPlayers |> Some
            else None
        | None -> None
    | None -> None

let render (useDefaultTheme, state, authUser:AuthUser option) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ str "Squads" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ProjectionState with
        | Initializing _ ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | InitializationFailed _ -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Active activeState ->
            let squadDic, currentSquadId = activeState.SquadsProjection.SquadDic, activeState.CurrentSquadId
            let currentGroup = squadDic |> group currentSquadId
            let groupTabs = groups |> List.map (groupTab currentGroup dispatch)
            let squadTabs = squadDic |> squadTabs currentSquadId dispatch
            match activeState.AddPlayersState with
            | Some addPlayersState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddPlayersModal (useDefaultTheme, squadDic, addPlayersState) (AddPlayersInput >> dispatch) ]
            | None -> ()

            // TODO-NMB-MEDIUM: Search box (e.g. for drafting)?...

            yield div divCentred [ tabs theme { tabsDefault with Tabs = groupTabs } ]
            match squadTabs with
            | _ :: _ ->
                yield div divCentred [ tabs theme { tabsDefault with TabsSize = Normal ; Tabs = squadTabs } ]
            | [] -> () // note: should never happen           
            match currentSquadId with
            | Some currentSquadId when currentSquadId |> squadDic.ContainsKey ->
                let squad = squadDic.[currentSquadId]
                yield br
                yield lazyViewOrHMR2 renderSquad (useDefaultTheme, squad) dispatch
                yield lazyViewOrHMR2 renderPlayers (useDefaultTheme, squad.PlayerDic) dispatch
                yield Rct.ofOption (addPlayers theme currentSquadId squad authUser dispatch)
            | Some _ | None -> () ] // note: should never happen
