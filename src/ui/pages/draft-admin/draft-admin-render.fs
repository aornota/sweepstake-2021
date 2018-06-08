module Aornota.Sweepstake2018.UI.Pages.DraftAdmin.Render

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.UI.Pages.DraftAdmin.Common
open Aornota.Sweepstake2018.UI.Shared
open Aornota.Sweepstake2018.Common.Domain.Draft
open Aornota.Sweepstake2018.Common.Domain.User

module Rct = Fable.Helpers.React

let private renderDrafts (useDefaultTheme, draftDic:DraftDic, _authUser) _dispatch =
    let theme = getTheme useDefaultTheme
    let draftRow (_draftId, draft:Draft) =
        let (DraftOrdinal draftOrdinal) = draft.DraftOrdinal
        let draftOrdinal = [ str (sprintf "#%i" draftOrdinal) ] |> para theme paraDefaultSmallest
        let draftOrdinal, status, starts, ends =
            match draft.DraftStatus with
            | PendingOpen (starts, ends) ->
                let status = [ italic "Pending" ] |> para theme paraDefaultSmallest
                let starts, ends = starts.LocalDateTime |> dateAndTimeText, ends.LocalDateTime |> dateAndTimeText
                let starts = [ str starts ] |> para theme paraDefaultSmallest
                let ends = [ str ends ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, starts |> Some, ends |> Some
            | Opened ends ->
                let status = [ str "Open" ] |> para theme paraDefaultSmallest
                let ends = ends.LocalDateTime |> dateAndTimeText
                let ends = [ str ends ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, ends |> Some
            | PendingProcessing ->
                let status = [ italic "Pending processing" ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, None
            | FreeSelection ->
                let status = [ str "No more drafts" ] |> para theme paraDefaultSmallest
                None, status |> Some, None, None
            | _ -> None, None, None, None
        tr false [
            td [ Rct.ofOption draftOrdinal ]
            td [ Rct.ofOption status ]
            td [ Rct.ofOption starts ]
            td [ Rct.ofOption ends ] ]
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) -> draft.DraftOrdinal)
    let draftRows = drafts |> List.map draftRow
    div divCentred [
        if draftDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [ 
                    tr false [
                        th [ [ bold "Draft" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Status" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Starts" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Ends" ] |> para theme paraDefaultSmallest ] ] ]
                tbody [ yield! draftRows ] ]
        else yield [ str "There are no drafts" ] |> para theme paraCentredSmaller ] // note: should never happen

let private activeDraftSummary useDefaultTheme (userDraftProjection:Projection<_ * UserDraftSummaryDic>) (draftDic:DraftDic) (userDic:UserDic) =
    let theme = getTheme useDefaultTheme
    match userDraftProjection with
    | Pending ->
        [ div divCentred [ icon iconSpinnerPulseMedium ] ]
    | Failed -> // note: should never happen
        [ [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } ]
    | Ready (_, userDraftSummaryDic) ->
        match draftDic |> activeDraft with
        | Some (draftId, draft) ->
            let userDraftSummaryRow userDraftSummaryDto =
                let (UserName userName) = fst userDraftSummaryDto.UserDraftKey |> userName userDic
                tr false [
                    td [ [ str userName ] |> para theme paraDefaultSmallest ]
                    td [ [ str (sprintf "%i" userDraftSummaryDto.PickCount) ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
            let userDraftSummaries =
                userDraftSummaryDic |> List.ofSeq |> List.filter (fun (KeyValue (userDraftKey, _)) -> snd userDraftKey = draftId)
                |> List.sortBy (fun (KeyValue (userDraftKey, _)) -> fst userDraftKey |> userName userDic)
                |> List.map (fun (KeyValue (_, userDraftSummaryDto)) -> userDraftSummaryDto)
            let userDraftSummaryRows = userDraftSummaries |> List.map userDraftSummaryRow
            [
                yield [ bold (sprintf "Summary for %s" (draft.DraftOrdinal |> draftTextLower)) ] |> para theme paraCentredSmaller
                yield hr theme true
                if userDraftSummaries.Length > 0 then
                    yield div divCentred [
                        table theme false { tableDefault with IsNarrow = true } [
                            thead [ 
                                tr false [
                                    th [ [ bold "User" ] |> para theme paraDefaultSmallest ]
                                    th [ [ bold "Selections" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
                            tbody [ yield! userDraftSummaryRows ] ] ]
                else yield [ str "There are no user draft picks" ] |> para theme paraCentredSmallest
            ]
        | None -> []

let render (useDefaultTheme, state, authUser, draftProjection:Projection<_ * DraftDic * _>, userProjection:Projection<_ * UserDic>, _hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    let userDic = match userProjection with | Ready (_, userDic) -> userDic | Pending | Failed -> UserDic ()
    columnContent [
        yield [ bold "Draft administration" ] |> para theme paraCentredSmall
        yield hr theme false
        match draftProjection with
        | Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, draftDic, _) ->
            yield lazyViewOrHMR2 renderDrafts (useDefaultTheme, draftDic, authUser) dispatch
            match activeDraftSummary useDefaultTheme state.UserDraftSummaryProjection draftDic userDic with
            | h :: t ->
                yield br
                yield! h :: t
            | [] -> () ]
