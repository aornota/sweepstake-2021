module Aornota.Sweepstake2018.UI.Shared

open Aornota.Sweepstake2018.Shared.Domain
open Aornota.Sweepstake2018.Shared.Ws.Server
open Aornota.Sweepstake2018.Shared.Ws.Ui

open Elmish
open Elmish.Toastr

type SharedInput =
    | AddDebugMessage of debugMessage : string
    | SendUnauthenticatedWsApi of uiUnauthenticatedWsApi : UiUnauthenticatedWsApi
    | SendAuthenticatedWsApi of authenticatedUser : AuthenticatedUser * uiAuthenticatedWsApi : UiAuthenticatedWsApi
    | ReceiveServerWsApi of serverWsApi : ServerWsApi

let [<Literal>] private DEFAULT_TOAST_TIMEOUT = 3000

let private toastCmd toCmd toastText : Cmd<_> =
    Toastr.message toastText |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> toCmd

let infoToastCmd toastText = toastCmd Toastr.info toastText
let successToastCmd toastText = toastCmd Toastr.success toastText
let warningToastCmd toastText = toastCmd Toastr.warning toastText
let errorToastCmd toastText = toastCmd Toastr.error toastText
