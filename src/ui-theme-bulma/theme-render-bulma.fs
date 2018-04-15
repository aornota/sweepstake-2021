module Aornota.UI.Theme.Render.Bulma

open System

open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common

open Fable.Core.JsInterop
module Rct = Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React

open Fulma
open Fulma.Components
open Fulma.Elements
open Fulma.Elements.Form
open Fulma.Extensions
open Fulma.Layouts

type FieldData = {
    AddOns : Alignment option
    Grouped : Alignment option
    TooltipData : TooltipData option }

type LinkType =
    | SameWindow of url : string
    | NewWindow of url : string
    | DownloadFile of url : string * fileName : string
    | ClickableLink of onClick : (MouseEvent -> unit)

let [<Literal>] private IS_LINK = "is-link"

let private delete onClick = Delete.delete [ Delete.OnClick onClick ] []

let private semanticText semantic =
    match semantic with
    | Primary -> "primary" | Info -> "info" | Link -> "link" | Success -> "success" | Warning -> "warning" | Danger -> "danger"
    | Dark -> "dark" | Light -> "light" | Black -> "black" | White -> "white"

let private getTooltipCustomClass tooltipData =
    let semanticText = match tooltipData.TooltipSemantic with | Some semantic -> Some (semanticText semantic) | None -> None
    let position =
        match tooltipData.Position with
        | TooltipTop -> Tooltip.IsTooltipTop | TooltipRight -> Tooltip.IsTooltipRight | TooltipBottom -> Tooltip.IsTooltipBottom | TooltipLeft -> Tooltip.IsTooltipLeft
    let customClasses = [
        yield Tooltip.ClassName
        match semanticText with | Some semanticText -> yield sprintf "is-tooltip-%s" semanticText | _ -> ()
        yield position
        if tooltipData.IsMultiLine then yield "is-tooltip-multiline" ]
    match customClasses with | _ :: _ -> Some (String.concat SPACE customClasses) | _ -> None

let private getTooltipProps tooltipData = Tooltip.dataTooltip tooltipData.TooltipText

let private getClassName theme useAlternativeClass = if useAlternativeClass then getAlternativeClass theme.AlternativeClass else getThemeClass theme.ThemeClass

let fieldDefault = { AddOns = None ; Grouped = None ; TooltipData = None }

let box theme useAlternativeClass children =
    let className = getClassName theme useAlternativeClass
    Box.box' [ CustomClass className ] children

let button theme buttonData children =
    let buttonData = theme.TransformButtonData buttonData
    let tooltipData = match buttonData.Interaction with | Clickable (_, Some tooltipData) -> Some tooltipData | NotEnabled (Some tooltipData) -> Some tooltipData | _ -> None
    let tooltipData = match tooltipData with | Some tooltipData -> Some (theme.TransformTooltipData tooltipData) | None -> None
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match buttonData.ButtonSemantic with
        | Some Primary -> Some (Button.Color IsPrimary) | Some Info -> Some (Button.Color IsInfo) | Some Link -> None (* Some (Button.Color IsLink) *)
        | Some Success -> Some (Button.Color IsSuccess) | Some Warning -> Some (Button.Color IsWarning) | Some Danger -> Some (Button.Color IsDanger)
        | Some Dark -> Some (Button.Color IsDark) | Some Light -> Some (Button.Color IsLight) | Some Black -> Some (Button.Color IsBlack) | Some White -> Some (Button.Color IsWhite)
        | None -> None
    let size = match buttonData.ButtonSize with | Large -> Some (Button.Size IsLarge) | Medium -> Some (Button.Size IsMedium) | Normal -> None | Small -> Some (Button.Size IsSmall)
    let customClasses = [
        match buttonData.ButtonSemantic with | Some Link -> yield IS_LINK | _ -> ()
        match tooltipData with 
        | Some tooltipData -> match getTooltipCustomClass tooltipData with | Some tooltipCustomClass -> yield tooltipCustomClass | None -> ()
        | None -> () ]
    let customClass = match customClasses with | _ :: _ -> Some (Button.CustomClass (String.concat SPACE customClasses)) | _ -> None
    Button.button [
        match customClass with | Some customClass -> yield customClass | None -> ()
        match semantic with | Some semantic -> yield semantic | None -> ()
        match size with | Some size -> yield size | None -> ()
        if buttonData.IsText then yield Button.IsText
        if buttonData.IsOutlined then yield Button.IsOutlined
        if buttonData.IsInverted then yield Button.IsInverted
        match buttonData.Interaction with
        | Clickable (onClick, _) -> yield Button.OnClick onClick
        | Loading -> yield Button.IsLoading true
        | Static -> yield Button.IsStatic true
        | _ -> ()
        yield Button.Props [
            match tooltipData with | Some tooltipData -> yield getTooltipProps tooltipData | None -> ()
            yield Disabled (match buttonData.Interaction with | NotEnabled _ -> true | _ -> false) :> IHTMLProp ]
    ] [
        match buttonData.IconLeft with | Some iconDataLeft -> yield icon { iconDataLeft with IconAlignment = Some LeftAligned } | None -> ()
        yield! children
        match buttonData.IconRight with | Some iconDataRight -> yield icon { iconDataRight with IconAlignment = Some RightAligned } | None -> () ]

let field theme fieldData children =
    let tooltipData = match fieldData.TooltipData with | Some tooltipData -> Some (theme.TransformTooltipData tooltipData) | None -> None
    Field.div [
        match fieldData.AddOns with
        | Some Centred -> yield Field.HasAddonsCentered
        | Some LeftAligned -> yield Field.HasAddons
        | Some RightAligned -> yield Field.HasAddonsRight
        | Some FullWidth -> yield Field.HasAddonsFullWidth
        | _ -> ()
        match fieldData.Grouped with 
        | Some Centred -> yield Field.IsGroupedCentered
        | Some LeftAligned -> yield Field.IsGrouped
        | Some RightAligned -> yield Field.IsGroupedRight
        | _ -> ()
        match tooltipData with
        | Some tooltipData ->
            match getTooltipCustomClass tooltipData with | Some tooltipCustomClass -> yield Field.CustomClass tooltipCustomClass | None -> ()
            yield Field.Props [ getTooltipProps tooltipData ]
        | None -> ()
    ] children

let footer theme useAlternativeClass children =
    let className = getClassName theme useAlternativeClass
    Footer.footer [ CustomClass className ] children

let hr theme useAlternativeClass =
    let className = getClassName theme useAlternativeClass
    Rct.hr [ ClassName className ] 

let link theme linkType children =
    let (ThemeClass className) = theme.ThemeClass
    Rct.a [
        yield ClassName className :> IHTMLProp
        match linkType with
        | NewWindow url ->
            yield Href url :> IHTMLProp
            yield Target "_blank" :> IHTMLProp
        | SameWindow url ->
            yield Href url :> IHTMLProp
        | DownloadFile (url, fileName) ->
            yield Href url :> IHTMLProp
            yield Download fileName :> IHTMLProp
        | ClickableLink onClick ->
            yield OnClick onClick :> IHTMLProp
    ] children

let media theme left content right =
    let className = getClassName theme false
    // Note: Media.CustomClass does not work, so handled via Media.Props [ ClassName ... ] ].
    Media.media [ Media.Props [ ClassName (sprintf "media %s" className) ] ] [
        Media.left [] left
        Media.content [] content
        Media.right [] right ]

let message theme messageData headerChildren bodyChildren =
    let messageData = theme.TransformMessageData messageData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match messageData.MessageSemantic with
        | Some Primary -> Some (Message.Color IsPrimary) | Some Info -> Some (Message.Color IsInfo) | Some Link -> None (* Some (Message.Color IsLink) *)
        | Some Success -> Some (Message.Color IsSuccess) | Some Warning -> Some (Message.Color IsWarning) | Some Danger -> Some (Message.Color IsDanger)
        | Some Dark -> Some (Message.Color IsDark) | Some Light -> Some (Message.Color IsLight) | Some Black -> Some (Message.Color IsBlack) | Some White -> Some (Message.Color IsWhite)
        | None -> None
    let size = match messageData.MessageSize with | Large -> Some (Message.Size IsLarge) | Medium -> Some (Message.Size IsMedium) | Normal -> None | Small -> Some (Message.Size IsSmall)
    Message.message [ 
        match semantic with | Some semantic -> yield semantic | None -> ()
        match messageData.MessageSemantic with | Some Link -> yield Message.CustomClass IS_LINK | _ -> ()
        match size with | Some size -> yield size | None -> ()
    ] [
        Message.header [] [
            yield! headerChildren
            match messageData.OnDismissMessage with | Some onDismissMessage -> yield delete onDismissMessage | None -> () ]
        Message.body [] bodyChildren ]

let navbar theme navbarData children =
    let navbarData = theme.TransformNavbarData navbarData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match navbarData.NavbarSemantic with
        | Some Primary -> Some (Navbar.Color IsPrimary) | Some Info -> Some (Navbar.Color IsInfo) | Some Link -> None (* Some (Navbar.Color IsLink) *)
        | Some Success -> Some (Navbar.Color IsSuccess) | Some Warning -> Some (Navbar.Color IsWarning) | Some Danger -> Some (Navbar.Color IsDanger)
        | Some Dark -> Some (Navbar.Color IsDark) | Some Light -> Some (Navbar.Color IsLight) | Some Black -> Some (Navbar.Color IsBlack) | Some White -> Some (Navbar.Color IsWhite)
        | None -> None
    let customClasses = [
        match navbarData.NavbarFixed with | Some FixedTop -> yield "is-fixed-top" | Some FixedBottom -> yield "is-fixed-bottom" | None -> () ]
    let customClass = match customClasses with | _ :: _ -> Some (Navbar.CustomClass (String.concat SPACE customClasses)) | _ -> None
    Navbar.navbar [
        match semantic with | Some semantic -> yield semantic | None -> ()
        match customClass with | Some customClass -> yield customClass | None -> ()
    ] children

let navbarDropDown theme element children =
    let className = getClassName theme false
    Navbar.Item.div [ Navbar.Item.HasDropdown ; Navbar.Item.IsHoverable ] [
        // Note: Navbar.Link.CustomClass | Navbar.Dropdown.CustomClass do not work, so handle manually.
        Rct.div [ ClassName (sprintf "navbar-link %s" className) ] [ element ]
        Rct.div [ ClassName (sprintf "navbar-dropdown %s" className) ] children ]

let navbarDropDownItem theme isActive children =
    let className = getClassName theme false
    Navbar.Item.div [ yield Navbar.Item.CustomClass className ; yield Navbar.Item.IsActive isActive ] children

let navbarMenu theme navbarData isActive children =
    let navbarData = theme.TransformNavbarData navbarData
    let semantic = match navbarData.NavbarSemantic with | Some semantic -> Some (semanticText semantic) | None -> None
    Navbar.menu
        [
            match semantic with | Some semantic -> yield Navbar.Menu.CustomClass semantic | None -> ()
            yield Navbar.Menu.IsActive isActive
        ] children

let notification theme notificationData children =
    let notificationData = theme.TransformNotificationData notificationData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match notificationData.NotificationSemantic with
        | Some Primary -> Some (Notification.Color IsPrimary) | Some Info -> Some (Notification.Color IsInfo) | Some Link -> None (* Some (Notification.Color IsLink) *)
        | Some Success -> Some (Notification.Color IsSuccess) | Some Warning -> Some (Notification.Color IsWarning) | Some Danger -> Some (Notification.Color IsDanger)
        | Some Dark -> Some (Notification.Color IsDark) | Some Light -> Some (Notification.Color IsLight) | Some Black -> Some (Notification.Color IsBlack)
        | Some White -> Some (Notification.Color IsWhite)
        | None -> None
    Notification.notification [ 
        match semantic with | Some semantic -> yield semantic | None -> ()
        match notificationData.NotificationSemantic with | Some Link -> yield Notification.CustomClass IS_LINK | _ -> ()
    ] [
        match notificationData.OnDismissNotification with | Some onDismissNotification -> yield delete onDismissNotification | None -> ()
        yield! children ]

let pageLoader theme pageLoaderData =
    let pageLoaderData = theme.TransformPageLoaderData pageLoaderData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match pageLoaderData.PageLoaderSemantic with
        | Primary -> Some (PageLoader.Color IsPrimary) | Info -> Some (PageLoader.Color IsInfo) | Link -> None (* Some (PageLoader.Color IsLink) *)
        | Success -> Some (PageLoader.Color IsSuccess) | Warning -> Some (PageLoader.Color IsWarning) | Danger -> Some (PageLoader.Color IsDanger)
        | Dark -> Some (PageLoader.Color IsDark) | Light -> Some (PageLoader.Color IsLight) | Black -> Some (PageLoader.Color IsBlack) | White -> Some (PageLoader.Color IsWhite)
    PageLoader.pageLoader [
        match semantic with | Some semantic -> yield semantic | None -> ()
        match pageLoaderData.PageLoaderSemantic with | Link -> yield PageLoader.CustomClass IS_LINK | _ -> ()
        yield PageLoader.IsActive true
    ] []

let para theme paraData children =
    let paraData = theme.TransformParaData paraData
    let alignment = 
        match paraData.ParaAlignment with
        | Centred -> Some CENTRED | LeftAligned -> Some "left" | RightAligned -> Some "right" | Justified -> Some "justified"
        | _ -> None
    let colour =
        let greyscaleText greyscale =
            match greyscale with
            | BlackBis -> "black-bis" | BlackTer -> "black-ter" | GreyDarker -> "grey-darker" | GreyDark -> "grey-dark" | Grey -> "grey"
            | GreyLight -> "grey-light" | GreyLighter -> "grey-lighter" | WhiteTer -> "white-ter" | WhiteBis -> "white-bis"
        match paraData.ParaColour with
        | DefaultPara -> None
        | SemanticPara semantic -> Some (semanticText semantic)
        | GreyscalePara greyscale -> Some (greyscaleText greyscale)
    let size = match paraData.ParaSize with | LargestText -> 1 | LargerText -> 2 | LargeText -> 3 | MediumText -> 4 | SmallText -> 5 | SmallerText -> 6 | SmallestText -> 7
    let weight = match paraData.Weight with | LightWeight -> "light" | NormalWeight -> "normal" | SemiBold -> "semibold" | Bold -> "bold"
    let customClasses = [
        match alignment with | Some alignment -> yield sprintf "has-text-%s" alignment | None -> ()
        match colour with | Some colour -> yield sprintf "has-text-%s" colour | None -> ()
        yield sprintf "is-size-%i" size
        yield sprintf "has-text-weight-%s" weight ]
    let customClass = match customClasses with | _ :: _ -> Some (ClassName (String.concat SPACE customClasses)) | _ -> None
    Rct.p [ match customClass with | Some customClass -> yield customClass :> IHTMLProp | None -> () ] children

let progress theme useAlternativeClass progressData =
    let className = getClassName theme useAlternativeClass
    let progressData = theme.TransformProgressData progressData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match progressData.ProgressSemantic with
        | Some Primary -> Some (Progress.Color IsPrimary) | Some Info -> Some (Progress.Color IsInfo) | Some Link -> None (* Some (Progress.Color IsLink) *)
        | Some Success -> Some (Progress.Color IsSuccess) | Some Warning -> Some (Progress.Color IsWarning) | Some Danger -> Some (Progress.Color IsDanger)
        | Some Dark -> Some (Progress.Color IsDark) | Some Light -> Some (Progress.Color IsLight) | Some Black -> Some (Progress.Color IsBlack) | Some White -> Some (Progress.Color IsWhite)
        | None -> None
    let size = match progressData.ProgressSize with | Large -> Some (Progress.Size IsLarge) | Medium -> Some (Progress.Size IsMedium) | Normal -> None | Small -> Some (Progress.Size IsSmall)
    let customClasses = [
        yield className
        match progressData.ProgressSemantic with | Some Link -> yield IS_LINK | _ -> () ]
    let customClass = match customClasses with | _ :: _ -> Some (Progress.CustomClass (String.concat SPACE customClasses)) | _ -> None
    Progress.progress [
        match customClass with | Some customClass -> yield customClass | None -> ()
        match semantic with | Some semantic -> yield semantic | None -> ()
        match size with | Some size -> yield size | None -> ()
        yield Progress.Value progressData.Value
        yield Progress.Max progressData.MaxValue
    ] []

let span theme spanData children =
    let spanData = theme.TransformSpanData spanData
    let customClasses = [
        match spanData.SpanClass with | Some Healthy -> yield "healthy" | Some Unhealthy -> yield "unhealthy" | None -> () ]
    let customClass = match customClasses with | _ :: _ -> Some (ClassName (String.concat SPACE customClasses)) | _ -> None
    Rct.span [ match customClass with | Some customClass -> yield customClass :> IHTMLProp | None -> () ] children

let tabs theme tabsData =
    let className = getClassName theme false
    let tabsData = theme.TransformTabsData tabsData
    // Note: Tabs.CustomClass does not work, so handle manually.
    let customClasses = [
        yield "tabs"
        yield className
        if tabsData.IsBoxed then yield "is-boxed"
        if tabsData.IsToggle then yield "is-toggle"
        match tabsData.TabsSize with | Large -> yield "is-large" | Medium -> yield "is-medium" | Small -> yield "is-small" | _ -> ()
        match tabsData.TabsAlignment with | Centred -> yield "is-centered" | RightAligned -> yield "is-right" | FullWidth -> yield "is-fullwidth" | _ -> ()
    ]
    let customClass = match customClasses with | _ :: _ -> Some (ClassName (String.concat SPACE customClasses)) | _ -> None
    Rct.div [ match customClass with | Some customClass -> yield customClass :> IHTMLProp | None -> () ]
        [ Rct.ul [] [
            for tab in tabsData.Tabs do
                yield Tabs.tab [ Tabs.Tab.IsActive tab.IsActive ] [ Rct.a [ Href tab.TabLink ] [ str tab.TabText ] ]
        ] ]

let table theme useAlternativeClass tableData children =
    let className = getClassName theme useAlternativeClass
    let tableData = theme.TransformTableData tableData
    Table.table [
        yield Table.CustomClass className
        if tableData.IsBordered then yield Table.IsBordered
        if tableData.IsNarrow then yield Table.IsNarrow
        if tableData.IsStriped then yield Table.IsStriped
        if tableData.IsFullWidth then yield Table.IsFullwidth
    ] children

let tag theme tagData children =
    let tagData = theme.TransformTagData tagData
    // TODO-NMB-LOW: Rework Link hack once supported by Fulma...
    let semantic =
        match tagData.TagSemantic with
        | Some Primary -> Some (Tag.Color IsPrimary) | Some Info -> Some (Tag.Color IsInfo) | Some Link -> None (* Some (Tag.Color IsLink) *)
        | Some Success -> Some (Tag.Color IsSuccess) | Some Warning -> Some (Tag.Color IsWarning) | Some Danger -> Some (Tag.Color IsDanger)
        | Some Dark -> Some (Tag.Color IsDark) | Some Light -> Some (Tag.Color IsLight) | Some Black -> Some (Tag.Color IsBlack) | Some White -> Some (Tag.Color IsWhite)
        | None -> None
    let size = match tagData.TagSize with | Large -> Some (Tag.Size IsLarge) | Medium -> Some (Tag.Size IsMedium) | Normal | Small -> None
    let customClasses = [
        match tagData.TagSemantic with | Some Link -> yield IS_LINK | _ -> ()
        if tagData.IsRounded then yield "is-rounded" ]
    let customClass = match customClasses with | _ :: _ -> Some (Tag.CustomClass (String.concat SPACE customClasses)) | _ -> None
    Tag.tag [
        match semantic with | Some semantic -> yield semantic | None -> ()
        match customClass with | Some customClass -> yield customClass | None -> ()
        match size with | Some size -> yield size | None -> ()
    ] [
        yield! children
        match tagData.OnDismiss with | Some onDismiss -> yield delete onDismiss | None -> () ]

// TODO-NMB-MEDIUM: "Genericize"?...
let textArea theme (key:Guid) text errorText autoFocus disabled (onChange:string -> unit) =
    let className = getClassName theme false
    Control.div [ Control.HasIconLeft ] [
        yield Textarea.textarea [
            match errorText with | Some _ -> yield Textarea.Color IsDanger | None -> ()
            yield Textarea.CustomClass className
            yield Textarea.Size IsSmall
            yield Textarea.DefaultValue text
            yield Textarea.Props [
                Key (key.ToString ())
                Disabled disabled
                AutoFocus autoFocus
                OnChange (fun ev -> !!ev.target?value |> onChange) ] ] []
        match errorText with | Some errorText -> yield Help.help [ Help.Color IsDanger ] [ str errorText ] | None -> () ]

// TODO-NMB-MEDIUM: "Genericize"?...
let textBox theme (key:Guid) text iconData isPassword errorText autoFocus disabled (onChange:string -> unit) onEnter =
    let className = getClassName theme false
    Control.div [ Control.HasIconLeft ] [
        yield Input.text [
            match errorText with | Some _ -> yield Input.Color IsDanger | None -> ()
            yield Input.CustomClass className
            yield Input.Size IsSmall
            yield Input.DefaultValue text
            if isPassword then yield Input.Type Input.Password
            yield Input.Props [
                Key (key.ToString ())
                Disabled disabled
                AutoFocus autoFocus
                OnChange (fun ev -> !!ev.target?value |> onChange)
                onEnterPressed onEnter ] ]
        match iconData with | Some iconData -> yield icon { iconData with IconAlignment = Some LeftAligned } | None -> ()
        match errorText with | Some errorText -> yield Help.help [ Help.Color IsDanger ] [ str errorText ] | None -> () ]
