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

let private renderOpenedDraft (useDefaultTheme, draftId, draft:Draft, userDraftPickDic:UserDraftPickDic, pickOverrides:PickOverride list, squadDic:SquadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let draftText = draft.DraftOrdinal |> draftTextLower
    let userDraftPickRow (userDraftPick, rank) =
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
        let remove =
            let removeText = sprintf "Remove from %s" draftText
            if pickOverrides |> List.exists (fun pickOverride -> pickOverride.UserDraftPick = userDraftPick && pickOverride |> isRemoving) |> not then
                let onClick = (fun _ -> (draftId, userDraftPick) |> RemoveFromDraft |> dispatch)
                [ [ str removeText ] |> button theme { buttonDangerSmall with Interaction = Clickable (onClick, None) } ] |> para theme paraDefaultSmallest
            else [ [ str removeText ] |> button theme { buttonDangerSmall with Interaction = Loading } ] |> para theme paraDefaultSmallest
        tr false [
            td [ [ str (sprintf "#%i" rank) ] |> para theme paraDefaultSmallest ]
            td [ description ]
            td [ Rct.ofOption extra ]
            td [ remove ]
            td [ Rct.ofOption withdrawn ] ]
    let userDraftPickRows =
        userDraftPickDic |> List.ofSeq |> List.sortBy (fun (KeyValue (_, rank)) -> rank) |> List.map (fun (KeyValue (userDraftPick, rank)) -> (userDraftPick, rank) |> userDraftPickRow)
    div divCentred [
        if userDraftPickDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true } [
                thead [ 
                    tr false [
                        th [ [ bold "Rank" ] |> para theme paraDefaultSmallest ]
                        th []
                        th []
                        th []
                        th [] ] ]
                tbody [ yield! userDraftPickRows ] ]
        else yield [ bold (sprintf "You have not made any selections for the %s" draftText) ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger } ]

let render (useDefaultTheme, state, authUser:AuthUser option, squadsProjection:Projection<_ * SquadDic>, currentDraft:(DraftId * Draft) option, currentUserDraftDto, _hasModal) dispatch =
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
                let pickOverrides = state.PickOverridesState.PickOverrides
                yield lazyViewOrHMR2 renderOpenedDraft (useDefaultTheme, draftId, draft, userDraftPickDic, pickOverrides, squadDic) dispatch
            | None ->
                yield [ str "Coming soon" ] |> para theme paraCentredSmaller ]
