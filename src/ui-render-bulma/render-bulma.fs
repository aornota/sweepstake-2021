module Aornota.UI.Render.Bulma

open Aornota.UI.Render.Common

module Rct = Fable.Helpers.React
open Fable.Helpers.React.Props

open Fulma
open Fulma.Components
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Layouts

type ContainerWidth = | Fluid | Widescreen | FullHD

type Size = | Large | Medium | Normal | Small

type Icon = | SpinnerPulse | Theme | Checked | Unchecked | ExpandDown | CollapseUp | Forward | Back | Ascending | Descending | Branch | Database | Notes | File | Find | User

type IconData = {
    IconSize : Size
    IconAlignment : Alignment option
    Icon : Icon }

type FixedSize = | Square16 | Square24 | Square32 | Square48 | Square64 | Square96 | Square128

type Ratio = | Square | FourByThree | ThreeByTwo | SixteenByNine | TwoByOne

type ImageSize =
    | FixedSize of fixedSize : FixedSize
    | Ratio of ratio: Ratio

let private iconDefault = { IconSize = Normal ; IconAlignment = None ; Icon = Theme }

let private columns isMobile children = Columns.columns [ if isMobile then yield Columns.CustomClass "is-mobile" ] children

let private columnEmpty = Column.column [] []

// TODO-NMB: Make this configurable (i.e. rather than hard-coding responsive sizes)?...
let columnContent children =
    columns true [
        columnEmpty
        Column.column [ Column.CustomClass "is-four-fifths-mobile is-four-fifths-tablet is-three-quarters-desktop is-three-fifths-widescreen is-half-fullhd" ] children
        columnEmpty ]

let container width children =
    let width =
        match width with | Some Fluid -> Some Container.IsFluid | Some Widescreen -> Some Container.IsWideScreen | Some FullHD -> Some Container.IsFullHD | None -> None
    Container.container [ match width with | Some width -> yield width | None -> () ] children

let control children = Control.div [] children

let icon iconData =
    let size, faSize =
        match iconData.IconSize with
        | Large -> Some (Icon.Size IsLarge), "fa-3x" | Medium -> Some (Icon.Size IsMedium), "fa-2x" | Normal -> None, "fa-lg" | Small -> Some (Icon.Size IsSmall), ""
    let alignment = match iconData.IconAlignment with | Some LeftAligned -> Some Icon.IsLeft | Some RightAligned -> Some Icon.IsRight | _ -> None
    let iconClass =
        match iconData.Icon with
        | SpinnerPulse -> "fa-spinner fa-pulse"
        | Theme -> "fa-square"
        | Checked -> "fa-check-square" | Unchecked -> "fa-square-o"
        | ExpandDown -> "fa-caret-down" | CollapseUp -> "fa-caret-up"
        | Forward -> "fa-chevron-right" | Back -> "fa-chevron-left"
        | Ascending -> "fa-angle-up" (* or "fa-long-arrow-up"? *) | Descending -> "fa-angle-down" (* or "fa-long-arrow-down"? *)
        | Branch -> "fa-code-fork"
        | Database -> "fa-database"
        | Notes -> "fa-pencil"
        | File -> "fa-file-o"
        | Find -> "fa-search"
        | User -> "fa-user"
    Icon.icon [
        match size with Some size -> yield size | None -> ()
        match alignment with Some alignment -> yield alignment | None -> ()
    ] [ Rct.i [ ClassName (sprintf "fa %s %s" iconClass faSize) ] [] ]

let image source size =
    let option =
        match size with
        | Some (FixedSize fixedSize) ->
            match fixedSize with
            | Square16 -> Some Image.Is16x16 | Square24 -> Some Image.Is24x24 | Square32 -> Some Image.Is32x32 | Square48 -> Some Image.Is48x48
            | Square64 -> Some Image.Is64x64 | Square96 -> Some Image.Is96x96 | Square128 -> Some Image.Is128x128
        | Some (Ratio ratio) ->
            match ratio with
            | Square -> Some Image.Is1by1 | FourByThree -> Some Image.Is4by3 | ThreeByTwo -> Some Image.Is3by2 | SixteenByNine -> Some Image.Is16by9
            | TwoByOne -> Some Image.Is2by1
        | None -> None
    Image.image [
        yield Image.Props [ Key source ]
        match option with | Some option -> yield option | None -> () ] [ Rct.img [ Src source] ]

let level hasContinuation children = Level.level [ if hasContinuation then yield Level.Level.CustomClass "hasContinuation" ] children

let levelLeft children = Level.left [] children
let levelRight children = Level.right [] children
let levelItem children = Level.item [] children

let navbarBrand children = Navbar.Brand.div [] children
let navbarBurger onClick isActive =
    Navbar.burger
        [
            if isActive then yield Fulma.Common.CustomClass "is-active"
            yield Fulma.Common.Props [ OnClick onClick ]
        ]
        [ for _ in 1..3 do yield Rct.span [] [] ]
let navbarItem children = Navbar.Item.div [] children
let navbarStart children = Navbar.Start.div [] children
let navbarEnd children = Navbar.End.div [] children

let thead children = Rct.thead [] children
let tbody children = Rct.tbody [] children
let tr isSelected children = Rct.tr [ if isSelected then yield ClassName "is-selected" :> IHTMLProp ] children
let td children = Rct.td [] children

let divTags children = div { divDefault with DivCustomClass = Some "tags" } children

let iconSpinnerPulse = { iconDefault with Icon = SpinnerPulse }
let iconSpinnerPulseSmall = { iconSpinnerPulse with IconSize = Small }
let iconSpinnerPulseMedium = { iconSpinnerPulse with IconSize = Medium }
let iconSpinnerPulseLarge = { iconSpinnerPulse with IconSize = Large }
let iconTheme = { iconDefault with Icon = Theme }
let iconThemeSmall = { iconTheme with IconSize = Small }
let iconChecked = { iconDefault with Icon = Checked }
let iconCheckedSmall = { iconChecked with IconSize = Small }
let iconUnchecked = { iconDefault with Icon = Unchecked }
let iconUncheckedSmall = { iconUnchecked with IconSize = Small }
let iconExpandDown = { iconDefault with Icon = ExpandDown }
let iconExpandDownSmall = { iconExpandDown with IconSize = Small }
let iconCollapseUp = { iconDefault with Icon = CollapseUp }
let iconCollapseUpSmall = { iconCollapseUp with IconSize = Small }
let iconForward = { iconDefault with Icon = Forward }
let iconForwardSmall = { iconForward with IconSize = Small }
let iconBack = { iconDefault with Icon = Back }
let iconBackSmall = { iconBack with IconSize = Small }
let iconAscending = { iconDefault with Icon = Ascending }
let iconAscendingSmall = { iconAscending with IconSize = Small }
let iconDescending = { iconDefault with Icon = Descending }
let iconDescendingSmall = { iconDescending with IconSize = Small }
let iconBranch = { iconDefault with Icon = Branch }
let iconBranchSmall = { iconBranch with IconSize = Small }
let iconDatabase = { iconDefault with Icon = Database }
let iconDatabaseSmall = { iconDatabase with IconSize = Small }
let iconNotes = { iconDefault with Icon = Notes }
let iconNotesSmall = { iconNotes with IconSize = Small }
let iconFile = { iconDefault with Icon = File }
let iconFileSmall = { iconFile with IconSize = Small }
let iconFind = { iconDefault with Icon = Find }
let iconFindSmall = { iconFind with IconSize = Small }
let iconUser = { iconDefault with Icon = User }
let iconUserSmall = { iconUser with IconSize = Small }

