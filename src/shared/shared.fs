module Aornota.Sweepstake2018.Shared.TODO

open System

type ConnectionId =
    | ConnectionId of id : Guid
    with static member Create () = Guid.NewGuid () |> ConnectionId

type Connection = {
    ConnectionId : ConnectionId
    Nickname : string }

type MessageId =
    | MessageId of id : Guid
    with static member Create () = Guid.NewGuid () |> MessageId

type Message = {
    MessageId : MessageId
    FromNickname : string
    Contents : string }

type UiWs =
    | ConnectWs of connection : Connection
    | SendMessageWs of connectionId : ConnectionId * message : Message
    | DisconnectWs of connection : Connection

type ServerWs =
    | OnReceiveErrorWs of errorText : string
    | ConnectResultWs of result : Result<Connection, string>
    | SendMessageResultWs of result : Result<Message, MessageId * string>
    | DisconnectResultWs of result : Result<Connection, string>    
    | UserConnectedOtherWs of nickname : string
    | SendMessageOtherWs of message : Message
    | UserDisconnectedOtherWs of nickname : string

type Ws =
    | UiWs of uiWs : UiWs
    | ServerWs of serverWs : ServerWs
