module Aornota.UI.Render.Common

module Rct = Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.PowerPack

type Alignment = | Centred | LeftAligned | RightAligned | Justified | FullWidth

type DivData = {
    DivCustomClass : string option
    IsCentred : bool
    PadV : int option
    PadH : int option }

let [<Literal>] CENTRED_CLASS = "centered" (* sic *)
let [<Literal>] SPACE = " "

let private padStyle padV padH =
    let padding =
        match padV, padH with
        | Some padV, Some padH -> sprintf "%ipx %ipx" padV padH
        | Some padV, None -> sprintf "%ipx 0" padV
        | None, Some padH -> sprintf "0 %ipx" padH
        | None, None -> "0 0"
    Style [ Padding padding ]

let str text = Rct.str text

let bold text = Rct.b [] [ str text ]

let italic text = Rct.i [] [ str text ]

let br = Rct.br []

let div divData children =
    let customClasses = [
        match divData.DivCustomClass with | Some divCustomClass -> yield divCustomClass | None -> ()
        if divData.IsCentred then yield CENTRED_CLASS ]
    let customClass = match customClasses with | _ :: _ -> Some (ClassName (String.concat SPACE customClasses)) | _ -> None
    Rct.div [
        match customClass with | Some customClass -> yield customClass :> IHTMLProp | None -> ()
        yield padStyle divData.PadV divData.PadH :> IHTMLProp
    ] children

let divDefault = { DivCustomClass = None ; IsCentred = false ; PadV = None ; PadH = None }
let divCentred = { divDefault with IsCentred = true }

let divVerticalSpace height = div { divDefault with PadV = Some (height / 2) } [ str SPACE ]

let divEmpty = div divDefault []

let onEnterPressed onEnter =
    OnKeyDown (fun (ev:KeyboardEvent) ->
        match ev with
        | _ when ev.keyCode = Keyboard.Codes.enter ->
            ev.preventDefault ()
            onEnter ()
        | _ -> ())

