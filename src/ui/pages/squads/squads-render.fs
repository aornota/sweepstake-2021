module Aornota.Sweepstake2018.UI.Pages.Squads.Render

open Aornota.Common.IfDebug

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.TimestampHelper
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

let private playerTypeRadios selectedPlayerType disabledPlayerType disableAll dispatch =
    let onChange playerType = (fun _ -> playerType |> dispatch)
    playerTypes
    |> List.sortBy playerTypeSortOrder
    |> List.map (fun playerType ->
        let isSelected = playerType |> Some = selectedPlayerType
        let disabled = disableAll || playerType |> Some = disabledPlayerType
        let onChange = if isSelected || disabled then ignore else playerType |> onChange
        radioInline (playerType |> playerTypeText) isSelected disabled onChange)
    |> List.collect id

let private nonWithdrawnCount squad =
    match squad with
    | Some squad ->
        squad.PlayerDic |> List.ofSeq |> List.filter (fun (KeyValue (_, player)) -> match player.PlayerStatus with | PlayerStatus.Active -> true | Withdrawn _ -> false) |> List.length
    | None -> 0

let private renderAddPlayersModal (useDefaultTheme, squadDic:SquadDic, addPlayersState:AddPlayersState) dispatch =
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
    let nonWithdrawnCount = squad |> nonWithdrawnCount
    let squadIsFull = nonWithdrawnCount >= (squadId |> maxPlayers)
    let isAddingPlayer, addPlayerInteraction, onEnter =
        let addPlayer = (fun _ -> AddPlayer |> dispatch)
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending -> true, Loading, ignore
        | Some (AddPlayerFailed _) | None ->
            match validatePlayerName playerNames (PlayerName addPlayersState.NewPlayerNameText), squadIsFull with
            | None, false -> false, Clickable (addPlayer, None), addPlayer
            | _ -> false, NotEnabled None, ignore
    let errorText = match addPlayersState.AddPlayerStatus with | Some (AddPlayerFailed errorText) -> errorText |> Some | Some AddPlayerPending | None -> None
    let (PlayerId newPlayerKey) = addPlayersState.NewPlayerId
    let body = [
        if squadIsFull then
            yield notification theme notificationWarning [ [ str (squadId |> squadIsFullText) ] |> para theme paraCentredSmallest ]
            yield br
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the name and position for the new player" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme newPlayerKey addPlayersState.NewPlayerNameText (iconMaleSmall |> Some) false addPlayersState.NewPlayerNameErrorText [] true isAddingPlayer
                (NewPlayerNameTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! playerTypeRadios (addPlayersState.NewPlayerType |> Some) None isAddingPlayer (NewPlayerTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Add player" ] |> button theme { buttonLinkSmall with Interaction = addPlayerInteraction } ] ]
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] onDismiss body

let private renderChangePlayerNameModal (useDefaultTheme, squadDic:SquadDic, changePlayerNameState:ChangePlayerNameState) dispatch =
    let theme = getTheme useDefaultTheme
    let squadId = changePlayerNameState.SquadId
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let playerId = changePlayerNameState.PlayerId
    let player = match squad with | Some squad -> (if playerId |> squad.PlayerDic.ContainsKey then squad.PlayerDic.[playerId] |> Some else None) | None -> None
    let currentPlayerName, titleText =
        match player with
        | Some player ->
            let (PlayerName playerName) = player.PlayerName
            player.PlayerName |> Some, sprintf "Edit name for %s" playerName
        | None -> None, "Edit player name" // note: should never happen
    let onDismiss = match changePlayerNameState.ChangePlayerNameStatus with | Some ChangePlayerNamePending -> None | Some _ | None -> (fun _ -> CancelChangePlayerName |> dispatch) |> Some
    let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
    let isChangingPlayerName, changePlayerNameInteraction, onEnter =
        let changePlayerName = (fun _ -> ChangePlayerName |> dispatch)
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending -> true, Loading, ignore
        | Some (ChangePlayerNameFailed _) | None ->
            let isSame = match currentPlayerName with | Some playerName -> playerName = (PlayerName changePlayerNameState.PlayerNameText) | None -> false
            match validatePlayerName playerNames (PlayerName changePlayerNameState.PlayerNameText), isSame with
            | None, false -> false, Clickable (changePlayerName, None), changePlayerName
            | _ -> false, NotEnabled None, ignore
    let errorText = match changePlayerNameState.ChangePlayerNameStatus with | Some (ChangePlayerNameFailed errorText) -> errorText |> Some | Some ChangePlayerNamePending | None -> None
    let (PlayerId playerKey) = changePlayerNameState.PlayerId
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the new name for the player" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme playerKey changePlayerNameState.PlayerNameText (iconMaleSmall |> Some) false changePlayerNameState.PlayerNameErrorText [] true isChangingPlayerName
                (PlayerNameTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Edit name" ] |> button theme { buttonLinkSmall with Interaction = changePlayerNameInteraction } ] ]
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] onDismiss body

let private renderChangePlayerTypeModal (useDefaultTheme, squadDic:SquadDic, changePlayerTypeState:ChangePlayerTypeState) dispatch =
    let theme = getTheme useDefaultTheme
    let squadId = changePlayerTypeState.SquadId
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let playerId = changePlayerTypeState.PlayerId
    let player = match squad with | Some squad -> (if playerId |> squad.PlayerDic.ContainsKey then squad.PlayerDic.[playerId] |> Some else None) | None -> None
    let currentPlayerType, titleText =
        match player with
        | Some player ->
            let (PlayerName playerName) = player.PlayerName
            player.PlayerType |> Some, sprintf "Change position for %s" playerName
        | None -> None, "Change player position" // note: should never happen
    let onDismiss = match changePlayerTypeState.ChangePlayerTypeStatus with | Some ChangePlayerTypePending -> None | Some _ | None -> (fun _ -> CancelChangePlayerType |> dispatch) |> Some
    let isChangingPlayerType, changePlayerTypeInteraction, onEnter =
        let changePlayerType = (fun _ -> ChangePlayerType |> dispatch)
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending -> true, Loading, ignore
        | Some (ChangePlayerTypeFailed _) | None ->
            let isValid = match changePlayerTypeState.PlayerType with | Some playerType -> playerType |> Some <> currentPlayerType | None -> false
            if isValid |> not then false, NotEnabled None, ignore
            else false, Clickable (changePlayerType, None), changePlayerType
    let errorText = match changePlayerTypeState.ChangePlayerTypeStatus with | Some (ChangePlayerTypeFailed errorText) -> errorText |> Some | Some ChangePlayerTypePending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please choose the new position for the player" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! playerTypeRadios changePlayerTypeState.PlayerType currentPlayerType isChangingPlayerType (PlayerTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change position" ] |> button theme { buttonLinkSmall with Interaction = changePlayerTypeInteraction } ] ]
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] onDismiss body

let private renderWithdrawPlayerModal (useDefaultTheme, squadDic:SquadDic, withdrawPlayerState:WithdrawPlayerState) dispatch =
    let theme = getTheme useDefaultTheme
    let squadId = withdrawPlayerState.SquadId
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let playerId = withdrawPlayerState.PlayerId
    let player = match squad with | Some squad -> (if playerId |> squad.PlayerDic.ContainsKey then squad.PlayerDic.[playerId] |> Some else None) | None -> None
    let titleText =
        match player with
        | Some player ->
            let (PlayerName playerName) = player.PlayerName
            sprintf "Withdraw %s" playerName
        | None -> "Withdraw player" // note: should never happen
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmWithdrawPlayer |> dispatch)
        let cancel = (fun _ -> CancelWithdrawPlayer |> dispatch)
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending -> Loading, None
        | Some (WithdrawPlayerFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match withdrawPlayerState.WithdrawPlayerStatus with | Some (WithdrawPlayerFailed errorText) -> errorText |> Some | Some WithdrawPlayerPending | None -> None
    let warning = [
        [ str "Are you sure you want to withdraw this player?" ] |> para theme paraCentredSmallest
        [ str "Please note that this action is irreversible" ] |> para theme paraCentredSmallest ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Withdraw player" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme [ [ str titleText ] |> para theme paraCentredSmall ] onDismiss body

let private renderEliminateSquadModal (useDefaultTheme, squadDic:SquadDic, eliminateSquadState:EliminateSquadState) dispatch =
    let theme = getTheme useDefaultTheme
    let squadId = eliminateSquadState.SquadId
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let titleText =
        match squad with
        | Some squad ->
            let (SquadName squadName) = squad.SquadName
            sprintf "Eliminate %s" squadName
        | None -> "Eliminate team" // note: should never happen
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmEliminateSquad |> dispatch)
        let cancel = (fun _ -> CancelEliminateSquad |> dispatch)
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending -> Loading, None
        | Some (EliminateSquadFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match eliminateSquadState.EliminateSquadStatus with | Some (EliminateSquadFailed errorText) -> errorText |> Some | Some EliminateSquadPending | None -> None
    let warning = [
        [ str "Are you sure you want to eliminate this team?" ] |> para theme paraCentredSmallest
        [ str "Please note that this action is irreversible" ] |> para theme paraCentredSmallest ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Eliminate team" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
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

let private renderSquad (useDefaultTheme, squadId, squad, authUser) dispatch = // TODO-SOON: Enable ShowEliminateSquadModal link in release builds...
    let theme = getTheme useDefaultTheme
    let (SquadName squadName), (CoachName coachName), (Seeding seeding) = squad.SquadName, squad.CoachName, squad.Seeding
    let canEliminate =
        match authUser with
        | Some authUser -> match authUser.Permissions.SquadPermissions with | Some squadPermissions -> squadPermissions.EliminateSquadPermission | None -> false
        | None -> false
    let eliminate =
        if canEliminate && squad.Eliminated |> not then
            let onClick = (fun _ -> squadId |> ShowEliminateSquadModal |> dispatch)
            let eliminate = [ [ str "Eliminate" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink onClick)
            ifDebug (eliminate |> Some) None // TEMP-NMB...
        else if squad.Eliminated then
            [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
        else None
    let score = 0 // TEMP-NMB...
    div divCentred [
        table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
            thead [ 
                tr false [
                    th [ [ bold "Team"] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Seeding" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Picked by" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    th [] ] ]
            tbody [
                tr false [
                    td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                    td [ [ str (sprintf "%i" seeding) ] |> para theme paraDefaultSmallest ]
                    td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                    td [ [ italic String.Empty ] |> para theme paraDefaultSmallest ]
                    td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    td [ Rct.ofOption eliminate ] ] ] ] ]

let private renderPlayers (useDefaultTheme, playerDic:PlayerDic, squadId, squad, authUser) dispatch = // TODO-SOON: Enable ShowWithdrawPlayerModal link in release builds...
    let theme = getTheme useDefaultTheme
    let canEdit, canWithdraw =
        match authUser with
        | Some authUser ->
            match authUser.Permissions.SquadPermissions with
            | Some squadPermissions -> squadPermissions.AddOrEditPlayerPermission, squadPermissions.WithdrawPlayerPermission
            | None -> false, false
        | None -> false, false
    let editName playerId =
        if canEdit then
            let onClick = (fun _ -> (squadId, playerId) |> ShowChangePlayerNameModal |> dispatch)
            [ [ str "Edit name" ] |> para theme paraDefaultSmallest ] |> link theme (ClickableLink onClick) |> Some
        else None
    let changePosition playerId =
        if canEdit then
            let onClick = (fun _ -> (squadId, playerId) |> ShowChangePlayerTypeModal |> dispatch)
            [ [ str "Change position" ] |> para theme paraDefaultSmallest ] |> link theme (ClickableLink onClick) |> Some
        else None
    let withdraw playerId player =
        let isWithdrawn, dateWithdrawn = match player.PlayerStatus with | PlayerStatus.Active -> false, None | Withdrawn dateWithdrawn -> true, dateWithdrawn
        if canWithdraw && squad.Eliminated |> not && isWithdrawn |> not then
            let onClick = (fun _ -> (squadId, playerId) |> ShowWithdrawPlayerModal |> dispatch)
            let withdraw = [ [ str "Withdraw" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink onClick)
            ifDebug (withdraw |> Some) None // TEMP-NMB...
        else if isWithdrawn then
            let withdrawnText = match dateWithdrawn with | Some dateWithdrawn -> sprintf "Withdrawn %s" (ago dateWithdrawn.LocalDateTime) | None -> "Withdrawn"
            [ [ str withdrawnText ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
        else None
    let playerRow (playerId, player) =
        let (PlayerName playerName), playerTypeText = player.PlayerName, player.PlayerType |> playerTypeText
        let score = 0 // TEMP-NMB...
        tr false [
            td [ [ str playerName ] |> para theme paraDefaultSmallest ]
            td [ Rct.ofOption (editName playerId) ]
            td [ [ str playerTypeText ] |> para theme paraCentredSmallest ]
            td [ Rct.ofOption (changePosition playerId) ]
            td [ [ italic String.Empty ] |> para theme paraDefaultSmallest ]
            td [ [ scoreText score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
            td [ Rct.ofOption (withdraw playerId player) ] ]
    let sortedPlayers = playerDic |> List.ofSeq |> List.map (fun (KeyValue (playerId, player)) -> (playerId, player)) |> List.sortBy (fun (_, player) ->
        player.PlayerType |> playerTypeSortOrder, player.PlayerName)
    let playerRows = sortedPlayers |> List.map (fun (playerId, player) -> (playerId, player) |> playerRow)
    div divCentred [
        if playerDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [ 
                    tr false [
                        th [ [ bold "Player" ] |> para theme paraDefaultSmallest ]
                        th []
                        th [ [ bold "Position" ] |> para theme paraCentredSmallest ]
                        th []
                        th [ [ bold "Picked by" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                        th [] ] ]
                tbody [ yield! playerRows ] ]
        else yield [ str "Player details coming soon" ] |> para theme paraCentredSmaller ]    

let private addPlayers theme squadId squad authUser dispatch =
    let nonWithdrawnCount = squad |> Some |> nonWithdrawnCount
    let paraAddPlayers = { paraDefaultSmallest with ParaAlignment = RightAligned }
    match squad.Eliminated, authUser with
    | false, Some authUser ->
        match authUser.Permissions.SquadPermissions with
        | Some squadPermissions ->
            if squadPermissions.AddOrEditPlayerPermission then
                if nonWithdrawnCount < (squadId |> maxPlayers)  then
                    [ [ str "Add player/s" ] |> para theme paraAddPlayers ] |> link theme (ClickableLink (fun _ -> squadId |> ShowAddPlayersModal |> dispatch)) |> Some
                else
                    [ italic (squadId |> squadIsFullText) ] |> para theme paraAddPlayers |> Some
            else None
        | None -> None
    | _, _ -> None

let render (useDefaultTheme, state, authUser:AuthUser option, hasModal) dispatch =
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
            match hasModal, activeState.AddPlayersState with
            | false, Some addPlayersState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddPlayersModal (useDefaultTheme, squadDic, addPlayersState) (AddPlayersInput >> dispatch) ]
            | _ -> ()
            match hasModal, activeState.ChangePlayerNameState with
            | false, Some changePlayerNameState ->
                yield div divDefault [ lazyViewOrHMR2 renderChangePlayerNameModal (useDefaultTheme, squadDic, changePlayerNameState) (ChangePlayerNameInput >> dispatch) ]
            | _ -> ()
            match hasModal, activeState.ChangePlayerTypeState with
            | false, Some changePlayerTypeState ->
                yield div divDefault [ lazyViewOrHMR2 renderChangePlayerTypeModal (useDefaultTheme, squadDic, changePlayerTypeState) (ChangePlayerTypeInput >> dispatch) ]
            | _ -> ()
            match hasModal, activeState.WithdrawPlayerState with
            | false, Some withdrawPlayerState ->
                yield div divDefault [ lazyViewOrHMR2 renderWithdrawPlayerModal (useDefaultTheme, squadDic, withdrawPlayerState) (WithdrawPlayerInput >> dispatch) ]
            | _ -> ()           
            match hasModal, activeState.EliminateSquadState with
            | false, Some eliminateSquadState ->
                yield div divDefault [ lazyViewOrHMR2 renderEliminateSquadModal (useDefaultTheme, squadDic, eliminateSquadState) (EliminateSquadInput >> dispatch) ]
            | _ -> ()
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
                yield lazyViewOrHMR2 renderSquad (useDefaultTheme, currentSquadId, squad, authUser) dispatch
                yield lazyViewOrHMR2 renderPlayers (useDefaultTheme, squad.PlayerDic, currentSquadId, squad, authUser) dispatch
                yield Rct.ofOption (addPlayers theme currentSquadId squad authUser dispatch)
            | Some _ | None -> () ] // note: should never happen