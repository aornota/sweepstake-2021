module Aornota.Sweepstake2018.Shared

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
    | ConnectResultWs of result : Result<Connection, exn>
    | UserConnectedOtherWs of nickname : string
    | SendMessageResultWs of Result<Message, MessageId * exn>
    | SendMessageOtherWs of message : Message
    | DisconnectResultWs of result : Result<Connection, exn>    
    | UserDisconnectedOtherWs of nickname : string

type Ws =
    | UiWs of uiWs : UiWs
    | ServerWs of serverWs : ServerWs
