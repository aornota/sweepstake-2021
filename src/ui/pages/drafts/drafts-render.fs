module Aornota.Sweepstake2018.UI.Pages.Drafts.Render

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.UI.Pages.Drafts.Common
open Aornota.Sweepstake2018.UI.Shared
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User

open System.Collections.Generic

module Rct = Fable.Helpers.React

type private UserDraftPickDic = Dictionary<UserDraftPick, int>

let private squadDescription (squadDic:SquadDic) squadId =
    if squadId |> squadDic.ContainsKey then       
        let squad = squadDic.[squadId]
        let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
        sprintf "%s (%s)" squadName coachName
    else UNKNOWN

let private playerDescriptionAndExtraAndWithdrawn (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let squad = squadDic.[squadId]
        let (SquadName squadName) = squad.SquadName
        let playerDic = squad.PlayerDic
        if playerId |> playerDic.ContainsKey then
            let player = playerDic.[playerId]
            let (PlayerName playerName) = player.PlayerName
            let withdrawn = match player.PlayerStatus with | Active -> false | Withdrawn _ -> true
            sprintf "%s (%s)" playerName squadName, player.PlayerType |> playerTypeText, withdrawn
        else UNKNOWN, UNKNOWN, false
    else UNKNOWN, UNKNOWN, false

let private renderOpenedDraft (useDefaultTheme, state, draftId, draft:Draft, userDraftPickDic:UserDraftPickDic, squadDic:SquadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let draftText = draft.DraftOrdinal |> draftTextLower
    let userDraftPickRow (userDraftPick, rank) =
        let count = userDraftPickDic.Count
        let increasePriorityButton =
            let interaction, highlight =
                if rank < 2 then None, false
                else
                    match state.ChangePriorityPending, state.RemovalPending with
                    | None, None ->
                        let highlight = match state.LastPriorityChanged with | Some (lastPick, Increase) when lastPick = userDraftPick -> true | Some _ | None -> false
                        Clickable ((fun _ -> (draftId, userDraftPick, Increase) |> ChangePriority |> dispatch), None) |> Some, highlight
                    | Some (pendingPick, Increase, _), _ when pendingPick = userDraftPick -> Loading |> Some, true
                    | _ -> NotEnabled None |> Some, false
            let buttonData = if highlight then buttonPrimarySmall else buttonLinkSmall
            match interaction with
            | Some interaction ->
                [ button theme { buttonData with Interaction = interaction ; IconLeft = iconAscendingSmall |> Some } [] ]
                |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
            | None -> None
        let decreasePriorityButton =
            let interaction, highlight =
                if rank > count - 1 then None, false
                else
                    match state.ChangePriorityPending, state.RemovalPending with
                    | None, None ->
                        let highlight = match state.LastPriorityChanged with | Some (lastPick, Decrease) when lastPick = userDraftPick -> true | Some _ | None -> false
                        Clickable ((fun _ -> (draftId, userDraftPick, Decrease) |> ChangePriority |> dispatch), None) |> Some, highlight
                    | Some (pendingPick, Decrease, _), _ when pendingPick = userDraftPick -> Loading |> Some, true
                    | _ -> NotEnabled None |> Some, false
            let buttonData = if highlight then buttonPrimarySmall else buttonLinkSmall
            match interaction with
            | Some interaction ->
                [ button theme { buttonData with Interaction = interaction ; IconLeft = iconDescendingSmall |> Some } [] ]
                |> para theme paraDefaultSmallest |> Some
            | None -> None
        let description, extra, withdrawn =
            match userDraftPick with
            | TeamPick squadId ->
                let description = squadId |> squadDescription squadDic
                [ str description ] |> para theme paraDefaultSmallest, None, None
            | PlayerPick (squadId, playerId) ->
                let (description, extra, withdrawn) = (squadId, playerId) |> playerDescriptionAndExtraAndWithdrawn squadDic
                let withdrawn =
                    if withdrawn then
                        [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
                    else None
                [ str description ] |> para theme paraDefaultSmallest, [ str extra ] |> para theme paraCentredSmallest |> Some, withdrawn
        let removeButton =
            let interaction =
                match state.RemovalPending, state.ChangePriorityPending with
                | None, None -> Clickable ((fun _ -> (draftId, userDraftPick) |> RemoveFromDraft |> dispatch), None)
                | Some (pendingPick, _), _ when pendingPick = userDraftPick -> Loading
                | _ -> NotEnabled None
            [ [ str (sprintf "Remove from %s" draftText) ] |> button theme { buttonDangerSmall with Interaction = interaction } ] |> para theme paraDefaultSmallest
        tr false [
            td [ Rct.ofOption increasePriorityButton ]
            td [ [ str (sprintf "#%i" rank) ] |> para theme paraCentredSmallest ]
            td [ Rct.ofOption decreasePriorityButton ]
            td [ description ]
            td [ Rct.ofOption extra ]
            td [ removeButton ]
            td [ Rct.ofOption withdrawn ] ]
    let userDraftPickRows =
        userDraftPickDic |> List.ofSeq |> List.sortBy (fun (KeyValue (_, rank)) -> rank) |> List.map (fun (KeyValue (userDraftPick, rank)) -> (userDraftPick, rank) |> userDraftPickRow)
    div divCentred [
        if userDraftPickDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true } [
                thead [ 
                    tr false [
                        th []
                        th [ [ bold "Rank" ] |> para theme paraCentredSmallest ]
                        th []
                        th []
                        th []
                        th []
                        th [] ] ]
                tbody [ yield! userDraftPickRows ] ]
        else yield [ bold (sprintf "You have not made any selections for the %s" draftText) ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger } ]

let render (useDefaultTheme, state, _authUser:AuthUser option, squadsProjection:Projection<_ * SquadDic>, currentDraft:(DraftId * Draft) option, currentUserDraftDto, _hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    let userDraftPickDic =
        match currentUserDraftDto with
        | Some currentUserDraftDto ->
            let userDraftPickDic = UserDraftPickDic ()
            currentUserDraftDto.UserDraftPickDtos |> List.iter (fun userDraftPickDto -> (userDraftPickDto.UserDraftPick, userDraftPickDto.Rank) |> userDraftPickDic.Add)
            userDraftPickDic
        | None -> UserDraftPickDic ()
    columnContent [
        yield [ bold "Drafts" ] |> para theme paraCentredSmall
        yield hr theme false
        match squadsProjection with
        | Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, squadDic) ->
            let openedDraft =
                match currentDraft with
                | Some (draftId, draft) ->
                    match draft.DraftStatus with | Opened _ -> (draftId, draft) |> Some | _ -> None
                | None -> None
            match openedDraft with
            | Some (draftId, draft) ->
                yield lazyViewOrHMR2 renderOpenedDraft (useDefaultTheme, state, draftId, draft, userDraftPickDic, squadDic) dispatch
            | None ->
                yield [ str "Coming soon" ] |> para theme paraCentredSmaller ]
