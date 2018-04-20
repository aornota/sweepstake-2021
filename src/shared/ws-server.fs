module Aornota.Sweepstake2018.Shared.Ws.Server

open Aornota.Sweepstake2018.Shared.Domain

// TODO-NMB: Distinguish between "results" and "projections"?...

type ServerWsError =
    | ReceiveError of errorText : string
    | DeserializeUiWsApiError of errorText : string

type ServerAppWsApi =
    | ServerWsErrorWs of serverWsError : ServerWsError
    | ConnectedWs of otherConnections : int * signedIn : int
    | SignInResultWs of result : Result<AuthUser, string>
    | AutoSignInResultWs of result : Result<AuthUser, string>
    | SignOutResultWs of result : Result<SessionId, string> // TODO-NMB-HIGH: If no need for SessionId, use unit?...
    | AutoSignOutWs of sessionId : SessionId // TODO-NMB-HIGH: If no need for SessionId, use unit - or, e.g., AutoSignOutReason?...
    | OtherUserSignedIn of userName : string // TODO-NMB-MEDIUM: Should this be a ServerChatWs case, e.g. as a ChatUsers "projection"?...
    | OtherUserSignedOut of userName : string // TODO-NMB-MEDIUM: Should this be a ServerChatWs case, e.g. as a ChatUsers "projection"?...

type ServerChatWsApi =
    | SendChatMessageResultWs of result : Result<ChatMessage, ChatMessageId * string>
    | OtherUserChatMessageWs of chatMessage : ChatMessage

type ServerWsApi =
    | ServerAppWsApi of serverAppWsApi : ServerAppWsApi
    | ServerChatWsApi of serverChatWsApi : ServerChatWsApi
