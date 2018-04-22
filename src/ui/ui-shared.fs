module Aornota.Sweepstake2018.UI.Shared

open Elmish
open Elmish.Toastr

let [<Literal>] private DEFAULT_TOAST_TIMEOUT = 3000

let private toastCmd toCmd toastText : Cmd<_> =
    Toastr.message toastText |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> toCmd

let infoToastCmd toastText = toastCmd Toastr.info toastText
let successToastCmd toastText = toastCmd Toastr.success toastText
let warningToastCmd toastText = toastCmd Toastr.warning toastText
let errorToastCmd toastText = toastCmd Toastr.error toastText

(* Note: lazyView[n] functions do not play well with HMR - e.g. changes to render functions not apparent because state has not changed (so not re-rendered) - therefore suppress "laziness"
when HMR (see webpack.config.js) is defined. *)
let lazyViewOrHMR render state =
#if HMR
    render state
#else
    Elmish.React.Common.lazyView render state
#endif
let lazyViewOrHMR2 render state dispatch =
#if HMR
    render state dispatch
#else
    Elmish.React.Common.lazyView2 render state dispatch
#endif
