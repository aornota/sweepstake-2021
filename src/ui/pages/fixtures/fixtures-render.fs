module Aornota.Sweepstake2018.UI.Pages.Fixtures.Render

open Aornota.Common.IfDebug
open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.TimestampHelper
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.Domain.Fixture
open Aornota.Sweepstake2018.UI.Pages.Fixtures.Common
open Aornota.Sweepstake2018.UI.Shared

open System

module Rct = Fable.Helpers.React

let private filterTabs currentFixtureFilter dispatch =
    let isActive filter =
        match filter with
        | AllFixtures -> currentFixtureFilter = AllFixtures
        | GroupFixtures _ -> match currentFixtureFilter with | GroupFixtures _ -> true | AllFixtures | KnockoutFixtures -> false
        | KnockoutFixtures -> currentFixtureFilter = KnockoutFixtures
    let filterText filter = match filter with | AllFixtures -> "All" | GroupFixtures _ -> "Group" | KnockoutFixtures -> "Knockout"
    let onClick filter =
        match filter with
        | AllFixtures -> (fun _ -> ShowAllFixtures |> dispatch )
        | GroupFixtures _ -> (fun _ -> None |> ShowGroupFixtures |> dispatch )
        | KnockoutFixtures -> (fun _ -> ShowKnockoutFixtures |> dispatch )
    let filters = [ AllFixtures ; GroupA |> GroupFixtures ; KnockoutFixtures ]
    filters |> List.map (fun filter -> { IsActive = filter |> isActive ; TabText = filter |> filterText ; TabLinkType = ClickableLink (filter |> onClick) } )

let private groupTabs currentFixtureFilter dispatch =
    let groupTab currentGroup dispatch group =
        { IsActive = group = currentGroup ; TabText = group |> groupText ; TabLinkType = ClickableLink (fun _ -> group |> Some |> ShowGroupFixtures |> dispatch ) }
    match currentFixtureFilter with
    | GroupFixtures currentGroup -> groups |> List.map (groupTab currentGroup dispatch)
    | AllFixtures | KnockoutFixtures -> []

// #region startsIn
let private startsIn (_timestamp:DateTime) : Fable.Import.React.ReactElement option * bool =
#if TICK
    let startsIn, imminent = _timestamp |> startsIn
    (if imminent then bold startsIn else str startsIn) |> Some, imminent
#else
    None, false
#endif
// #endregion

let private renderFixtures (useDefaultTheme, currentFixtureFilter, fixtureDic:FixtureDic, squadDic:SquadDic, authUser, _:int<tick>) dispatch = // TODO-SOON?: Enable ShowConfirmParticipantModal link in release builds...
    let theme = getTheme useDefaultTheme
    let matchesFilter fixture =
        match currentFixtureFilter with
        | AllFixtures -> true
        | GroupFixtures currentGroup -> match fixture.Stage with | Group group -> group = currentGroup | RoundOf16 _ | QuarterFinal _ | SemiFinal _ | ThirdPlacePlayOff | Final -> false
        | KnockoutFixtures -> match fixture.Stage with | RoundOf16 _ | QuarterFinal _ | SemiFinal _ | ThirdPlacePlayOff | Final -> true | Group _ -> false
    let canConfirmParticipant =
        match authUser with
        | Some authUser ->
            match authUser.Permissions.FixturePermissions with
            | Some fixturePermissions -> fixturePermissions.ConfirmFixturePermission
            | None -> false
        | None -> false
    let confirmParticipant role participant fixtureId =
        match participant with
        | Confirmed _ -> None
        | Unconfirmed _ ->
            if canConfirmParticipant then
                let paraConfirm = match role with | Home -> { paraDefaultSmallest with ParaAlignment = RightAligned } | Away -> paraDefaultSmallest
                let onClick = (fun _ -> (fixtureId, role) |> ShowConfirmParticipantModal |> dispatch)
                let confirmParticipant = [ [ str "Confirm participant" ] |> para theme paraConfirm ] |> link theme (ClickableLink onClick)
                ifDebug (confirmParticipant |> Some) None
            else None
    let stageText stage =
        let stageText =
            match stage with
            | Group group -> match currentFixtureFilter with | GroupFixtures _ -> None | AllFixtures | KnockoutFixtures -> group |> groupText |> Some
            | RoundOf16 matchNumber -> sprintf "Match %i" matchNumber |> Some
            | QuarterFinal quarterFinalOrdinal -> sprintf "Quarter-final %i" quarterFinalOrdinal |> Some
            | SemiFinal semiFinalOrdinal -> sprintf "Semi-final %i" semiFinalOrdinal |> Some
            | ThirdPlacePlayOff -> "Third/fourth place play-off" |> Some
            | Final -> "Final" |> Some
        match stageText with | Some stageText -> [ str stageText ] |> para theme paraDefaultSmallest |> Some | None -> None
    let details fixture =
        let unconfirmedText unconfirmed =
            match unconfirmed with
            | Winner (Group group) -> sprintf "%s winner" (group |> groupText)
            | Winner (RoundOf16 matchNumber) -> sprintf "Match %i winner" matchNumber
            | Winner (QuarterFinal quarterFinalOrdinal) -> sprintf "Quarter-final %i winner" quarterFinalOrdinal
            | Winner (SemiFinal semiFinalOrdinal) -> sprintf "Semi-final %i winner" semiFinalOrdinal
            | Winner (ThirdPlacePlayOff) | Winner (Final) -> SHOULD_NEVER_HAPPEN
            | RunnerUp group -> sprintf "%s runner-up" (group |> groupText)
            | Loser semiFinalOrdinal -> sprintf "Semi-final %i loser" semiFinalOrdinal
        match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
        | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
            let matchOutcome = matchResult.MatchOutcome
            let winnerSquadId, penaltyShootout =
                match matchOutcome.PenaltyShootoutOutcome with
                | Some penaltyShootoutOutcome ->
                    if penaltyShootoutOutcome.HomeScore > penaltyShootoutOutcome.AwayScore then
                        let (SquadName squadName) = homeSquadId |> squadName squadDic
                        let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.HomeScore penaltyShootoutOutcome.AwayScore
                        homeSquadId |> Some, [ str penaltyShootoutText ] |> para theme paraDefaultSmallest |> Some
                    else if penaltyShootoutOutcome.AwayScore > penaltyShootoutOutcome.HomeScore then
                        let (SquadName squadName) = awaySquadId |> squadName squadDic
                        let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.AwayScore penaltyShootoutOutcome.HomeScore                       
                        awaySquadId |> Some, [ str penaltyShootoutText ] |> para theme paraDefaultSmallest |> Some
                    else None, None // note: should never happen
                | None ->
                    if matchOutcome.HomeGoals > matchOutcome.AwayGoals then homeSquadId |> Some, None
                    else if matchOutcome.AwayGoals > matchOutcome.HomeGoals then awaySquadId |> Some, None
                    else None, None
            let home, homeGoals =
                let (SquadName squadName) = homeSquadId |> squadName squadDic
                if homeSquadId |> Some = winnerSquadId then bold squadName, [ bold (sprintf "%i" matchOutcome.HomeGoals) ] |> para theme paraDefaultSmallest |> Some
                else str squadName, [ str (sprintf "%i" matchOutcome.HomeGoals) ] |> para theme paraDefaultSmallest |> Some
            let away, awayGoals =
                let paraAway = { paraDefaultSmallest with ParaAlignment = RightAligned }
                let (SquadName squadName) = awaySquadId |> squadName squadDic
                if awaySquadId |> Some = winnerSquadId then bold squadName, [ bold (sprintf "%i" matchOutcome.AwayGoals) ] |> para theme paraAway |> Some
                else str squadName, [ str (sprintf "%i" matchOutcome.AwayGoals) ] |> para theme paraAway |> Some
            home, homeGoals, str "-", away, awayGoals, penaltyShootout
        | homeParticipant, awayParticipant, _ ->
            let home = 
                match homeParticipant with
                | Confirmed squadId ->
                    let (SquadName squadName) = squadId |> squadName squadDic
                    squadName
                | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
            let away = 
                match awayParticipant with
                | Confirmed squadId ->
                    let (SquadName squadName) = squadId |> squadName squadDic
                    squadName
                | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
            str home, None, str "vs.", str away, None, None
    let extra fixture =
        let local = fixture.KickOff.LocalDateTime
        let hasResult = match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with | Confirmed _ , Confirmed _, Some _ -> true | _ -> false
        let extra, imminent =
            if hasResult then
            
                // TODO-SOON...
            
                None, false
            else if local < DateTime.Now then italic "Result pending" |> Some, true
            else local |> startsIn
        let paraExtra = { paraDefaultSmallest with ParaAlignment = RightAligned ; ParaColour = GreyscalePara Grey }
        let paraExtra = if imminent then { paraExtra with ParaColour = GreyscalePara GreyDarker } else paraExtra
        match extra with | Some extra -> [ extra ] |> para theme paraExtra |> Some | None -> None
    let fixtureRow (fixtureId, fixture) =
        let home, homeGoals, vs, away, awayGoals, penaltyShootout = fixture |> details
        tr false [
            td [ [ str (fixture.KickOff.LocalDateTime |> dateText) ] |> para theme paraDefaultSmallest ]
            td [ [ str (fixture.KickOff.LocalDateTime.ToString ("HH:mm")) ] |> para theme paraDefaultSmallest ]
            td [ Rct.ofOption (stageText fixture.Stage) ]
            td [ Rct.ofOption (confirmParticipant Home fixture.HomeParticipant fixtureId) ]
            td [ [ home ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
            td [ Rct.ofOption homeGoals ]
            td [ [ vs ] |> para theme paraCentredSmallest ]
            td [ Rct.ofOption awayGoals ]
            td [ [ away ] |> para theme paraDefaultSmallest ]
            td [ Rct.ofOption (confirmParticipant Away fixture.AwayParticipant fixtureId) ]
            td [ Rct.ofOption penaltyShootout ]
            td [ Rct.ofOption (fixture |> extra) ] ]   
    let fixtures =
        fixtureDic
        |> List.ofSeq
        |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture))
        |> List.filter (fun (_, fixture) -> fixture |> matchesFilter)
        |> List.sortBy (fun (_, fixture) -> fixture.KickOff)
    let fixtureRows = fixtures |> List.map (fun (fixtureId, fixture) -> (fixtureId, fixture) |> fixtureRow)
    div divCentred [
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [ 
                tr false [
                    th [ [ bold "Date" ] |> para theme paraDefaultSmallest ]
                    th [ [ bold "Time" ] |> para theme paraDefaultSmallest ]
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th [] ] ]
            tbody [ yield! fixtureRows ] ] ]    

let render (useDefaultTheme, state, authUser:AuthUser option, fixturesProjection:Projection<_ * FixtureDic>, squadsProjection:Projection<_ * SquadDic>, _hasModal, ticks:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "Fixtures / Results" ] |> para theme paraCentredSmall
        yield hr theme false
        match squadsProjection, fixturesProjection with
        | Pending, _ | _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _ | _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, squadDic), Ready (_, fixtureDic) ->
            let currentFixtureFilter = state.CurrentFixtureFilter
            let filterTabs = filterTabs currentFixtureFilter dispatch
            let groupTabs = groupTabs currentFixtureFilter dispatch
            yield div divCentred [ tabs theme { tabsDefault with TabsSize = Normal ; Tabs = filterTabs } ]
            match groupTabs with
            | _ :: _ ->
                yield div divCentred [ tabs theme { tabsDefault with Tabs = groupTabs } ]
            | [] -> ()
            yield br
            yield lazyViewOrHMR2 renderFixtures (useDefaultTheme, currentFixtureFilter, fixtureDic, squadDic, authUser, ticks) dispatch ]
