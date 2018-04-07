module Aornota.Sweepstake2018.Shared.Ws.Ui

open Aornota.Sweepstake2018.Shared.Domain

// TODO-NMB: Distinguish between "commands" and "queries"?...

type UiUnauthenticatedWsApi =
    | SignInWs of sessionId : SessionId * userName : string * password : string
    | AutoSignInWs of jwt : Jwt

type UiAuthenticatedWsApi =
    | SignOutWs
    | SendChatMessageWs of chatMessage : ChatMessage

type UiWsApi =
    | UiUnauthenticatedWsApi of uiUnauthenticatedWsApi : UiUnauthenticatedWsApi
    | UiAuthenticatedWsApi of jwt : Jwt * uiAuthenticatedWsApi : UiAuthenticatedWsApi
