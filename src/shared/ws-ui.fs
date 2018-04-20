module Aornota.Sweepstake2018.Shared.Ws.Ui

open Aornota.Sweepstake2018.Shared.Domain

// TODO-NMB-MEDIUM: Distinguish between "commands" and "queries"?...

type UiUnauthWsApi =
    | SignInWs of sessionId : SessionId * userName : string * password : string
    | AutoSignInWs of jwt : Jwt

type UiAuthWsApi =
    | SignOutWs
    | SendChatMessageWs of chatMessage : ChatMessage

type UiWsApi =
    | UiUnauthWsApi of uiUnauthWsApi : UiUnauthWsApi
    | UiAuthWsApi of jwt : Jwt * uiAuthWsApi : UiAuthWsApi
