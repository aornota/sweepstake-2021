module Aornota.Sweepstake2018.UI.State

open System

open Aornota.Sweepstake2018.Shared
open Aornota.Sweepstake2018.UI.Common

open Aornota.UI.Common.DebugMessages
open Aornota.UI.Common.LocalStorage
open Aornota.UI.Theme.Common

open Elmish
open Elmish.Toastr

open Fable.Core.JsInterop
open Fable.Import
module Brw = Fable.Import.Browser

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2018-ui-app-preferences"
let [<Literal>] private DEFAULT_TOAST_TIMEOUT = 3000

let private setBodyClass useDefaultTheme = Browser.document.body.className <- getThemeClass (getTheme useDefaultTheme).ThemeClass

let private infoToastCmd text : Cmd<_> =
    Toastr.message text |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> Toastr.info
let private successToastCmd text : Cmd<_> =
    Toastr.message text |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> Toastr.success
let private warningToastCmd text : Cmd<_> =
    Toastr.message text |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> Toastr.warning
let private errorToastCmd text : Cmd<_> =
    Toastr.message text |> Toastr.position TopRight |> Toastr.timeout DEFAULT_TOAST_TIMEOUT |> Toastr.hideEasing Easing.Swing |> Toastr.showCloseButton |> Toastr.error

let private readPreferencesCmd =
    let readPreferences () = async { return Option.map ofJson<Preferences> (readJson APP_PREFERENCES_KEY) }
    Cmd.ofAsync readPreferences () (Ok >> ReadPreferencesResult) (Error >> ReadPreferencesResult)

let private writePreferencesCmd useDefaultTheme =
    let writePreferences useDefaultTheme = async { do writeJson APP_PREFERENCES_KEY (toJson { UseDefaultTheme = useDefaultTheme }) }
    Cmd.ofAsync writePreferences useDefaultTheme (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private initializeWsSub dispatch =
    let sendUiWsCmd (ws:Brw.WebSocket) uiWs =
        if ws.readyState <> ws.OPEN then Cmd.ofMsg (OnSendUiWsNotOpen uiWs)
        else
            match uiWs with
            | ConnectWs connection -> ws.send (toJson (ConnectWs connection))
            | SendMessageWs (connectionId, message) -> ws.send (toJson (SendMessageWs (connectionId, message)))
            | DisconnectWs connection -> ws.send (toJson (DisconnectWs connection))
            Cmd.none
    let onWsMessage (wsMessage:Brw.MessageEvent) : unit =
        try
            // Note: Expect wsMessage.data to be deserializable to ServerWs.
            let serverWs = ofJson<ServerWs> <| unbox wsMessage.data
            match serverWs with
            | ConnectResultWs result -> dispatch (ConnectResult result)
            | UserConnectedOtherWs nickname -> dispatch (UserConnectedOther nickname)
            | SendMessageResultWs result -> dispatch (SendMessageResult result)
            | SendMessageOtherWs message -> dispatch (SendMessageOther message)
            | DisconnectResultWs result -> dispatch (DisconnectResult result)
            | UserDisconnectedOtherWs nickname -> dispatch (UserDisconnectedOther nickname)
            | OnReceiveErrorWs exn -> dispatch (OnServerReceiveWsError exn)
        with exn -> dispatch (OnWsMessageError exn)
#if DEBUG
    let url = "ws://localhost:8088/api/ws" // cf. port in ..\server\server.fs
#else
    // TODO-NMB: Confirm "production" url (e.g. Azure app service?)...
    let url = "wss://localhost:8088/api/ws"
#endif
    let ws = Brw.WebSocket.Create url
    ws.onopen <- (fun _ -> dispatch (OnWsOpen (sendUiWsCmd ws)))
    ws.onerror <- (fun _ -> dispatch OnWsError)
    ws.onmessage <- onWsMessage
    ()

let initialize () =
    let state = {
        DebugMessages = []
        UseDefaultTheme = true
        NavbarBurgerIsActive = false
        Status = ReadingPreferences
        SendUiWsCmd = (OnSendUiWsNotInitialized >> Cmd.ofMsg) }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let transition input state =
    let addDebugMessageIfSome debugMessage debugMessages = match debugMessage with | Some debugMessage -> debugMessage :: debugMessages | None -> debugMessages
    match input with
    | DismissDebugMessage debugId -> // note: silently ignore unknown debugId
        { state with DebugMessages = state.DebugMessages |> removeDebugMessage debugId }, Cmd.none
    | ToggleTheme ->
        let useDefaultTheme = not state.UseDefaultTheme
        setBodyClass useDefaultTheme
        { state with UseDefaultTheme = useDefaultTheme }, writePreferencesCmd useDefaultTheme
    | ToggleNavbarBurger ->
        { state with NavbarBurgerIsActive = not state.NavbarBurgerIsActive }, Cmd.none
    | ReadPreferencesResult (Ok (Some preferences)) ->
        let status, useDefaultTheme, cmd, debugMessage =
            match state.Status with
            | ReadingPreferences ->
                setBodyClass preferences.UseDefaultTheme
                // TEMP-NMB: Test using "uninitialized" SendUiWsCmd...
                //let cmd = state.SendUiWsCmd (ConnectWs { ConnectionId = ConnectionId.Create () ; Nickname = "uninitializedWs" })
                //InitializingWS, preferences.UseDefaultTheme, Cmd.batch [ cmd ; Cmd.ofSub initializeWsSub ], None
                // ...or not...
                InitializingWS, preferences.UseDefaultTheme, Cmd.ofSub initializeWsSub, None
                // ...NMB-TEMP
            | _ ->
                let debugMessage = Some (debugMessage (shouldNeverHappenText "ReadPreferencesResult (Ok (Some _)) input when Status is not ReadingPreferences"))
                state.Status, state.UseDefaultTheme, Cmd.none, debugMessage
        { state with Status = status ; UseDefaultTheme = useDefaultTheme ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | ReadPreferencesResult (Ok None) ->
        let status, cmd, debugMessage =
            match state.Status with
            | ReadingPreferences -> InitializingWS, Cmd.ofSub initializeWsSub, None
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "ReadPreferencesResult (Ok None) input when Status is not ReadingPreferences"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | ReadPreferencesResult (Error exn) ->
        let status, cmd, debugMessage =
            match state.Status with
            | ReadingPreferences -> state.Status, Cmd.ofMsg (ReadPreferencesResult (Ok None)), Some (debugMessage (errorText (sprintf "ReadPreferencesResult -> %s" exn.Message)))
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "ReadPreferencesResult (Error _) input when Status is not ReadingPreferences"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | WritePreferencesResult (Ok _) ->
        state, Cmd.none
    | WritePreferencesResult (Error exn) ->
        let debugMessage = Some (debugMessage (errorText (sprintf "WritePreferencesResult -> %s" exn.Message)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | OnWsOpen sendUiWsCmd ->
        let status, sendUiWsCmd, cmd, debugMessage =
            match state.Status with
            | InitializingWS ->
                // TEMP-NMB: Auto-connect...
                //NotConnected (ConnectionId.Create (), "neph", None, None), sendUiWsCmd, Cmd.ofMsg Connect, None
                // ...or not...
                NotConnected (ConnectionId.Create (), String.Empty, None, None), sendUiWsCmd, Cmd.none, None
                // ...NMB-TEMP
            | _ -> state.Status, state.SendUiWsCmd, Cmd.none, Some (debugMessage (shouldNeverHappenText "OnWsOpen input when Status is not InitializingWS"))
        { state with Status = status ; SendUiWsCmd = sendUiWsCmd ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | OnWsError ->
        let status, cmd, debugMessage =
            match state.Status with
            | InitializingWS -> ServiceUnavailable, errorToastCmd "Service is not available", Some (debugMessage (errorText "OnWsError input [Status is InitializingWS]"))
            | _ -> state.Status, Cmd.none, Some (debugMessage (errorText "OnWsError input [Status is not InitializingWS]"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | OnWsMessageError exn ->
        let debugMessage = Some (debugMessage (errorText (sprintf "OnWsMessageError -> %s" exn.Message)))
        // TODO-NMB: Improve errorToastCmd message?...
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, errorToastCmd "Unable to process web socket message from server"
    | OnServerReceiveWsError errorText' ->
        let debugMessage = Some (debugMessage (errorText (sprintf "OnServerReceiveWsError -> %s" errorText')))
        // TODO-NMB: Improve errorToastCmd message?...
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, errorToastCmd "Unable to process web socket message sent to server"
    | OnSendUiWsNotInitialized uiWs ->
        let debugMessage = Some (debugMessage (shouldNeverHappenText (sprintf "OnSendUiWsNotInitialized -> %A" uiWs)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | OnSendUiWsNotOpen uiWs ->
        let debugMessage = Some (debugMessage (errorText (sprintf "OnSendUiWsNotOpen -> %A" uiWs)))
        // TODO-NMB: Improve errorToastCmd message?...
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, errorToastCmd "The web socket is no longer open"
    | NicknameTextChanged nicknameText ->
        let status, debugMessage =
            match state.Status with
            | NotConnected (connectionId, _, _, _) -> NotConnected (connectionId, nicknameText, validateNicknameText nicknameText, None), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "NicknameTextChanged input when Status is not NotConnected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | Connect ->
        let status, cmd, debugMessage =
            match state.Status with
            | NotConnected (connectionId, nicknameText, _, _) ->
                match validateNicknameText nicknameText with
                | Some validationErrorText -> NotConnected (connectionId, nicknameText, Some validationErrorText, None), Cmd.none, None
                | None ->
                    let connection = { ConnectionId = connectionId ; Nickname = nicknameText }
                    Connecting connection, state.SendUiWsCmd (ConnectWs connection), None
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "Connect input when Status is not NotConnected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | ConnectResult (Ok connection) ->
        let status, cmd, debugMessage =
            match state.Status with
            | Connecting connection' when connection' = connection ->
                // TEMP-NMB: Test data...
                (*let messageUis = 
                    [
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "Hello?" } ; MessageType = Sent; Timestamp = DateTime.Now }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "Sorry\nReally" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddSeconds (-17.) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "I am w*nking as I write this..." } ; MessageType = SendFailed "Message 'I am w*nking as I write this...' is inappropriate" ; Timestamp = DateTime.Now.AddSeconds (-33.) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = "bubbles" ; Contents = "Lurking" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddSeconds (-45.) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "Where's bubbles?" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddMinutes (-2.2) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = "buttercup" ; Contents = "Hi blossom!" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddMinutes (-3.) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "Yo!" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddMinutes (-5.8) }
                        { Message = { MessageId = MessageId.Create () ; FromNickname = "blossom" ; Contents = "Hiya!" } ; MessageType = Confirmed; Timestamp = DateTime.Now.AddMinutes (-8.) }
                    ]
                Connected (connection, messageUis, MessageId.Create (), String.Empty), successToastCmd "You have connected", None*)
                // ...or not...
                Connected (connection, [], MessageId.Create (), String.Empty), successToastCmd "You have connected", None
                // ...NMB-TEMP
            | Connecting connection' ->
                state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText (sprintf "Connecting %A but ConnectResult (Ok _) is %A" connection' connection)))
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "ConnectResult (Ok _) input when Status is not Connecting"))     
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | ConnectResult (Error errorText) ->
        let status, debugMessage =
            match state.Status with
            | Connecting connection -> NotConnected (ConnectionId.Create (), connection.Nickname, None, Some errorText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "ConnectResult (Error _) input when Status is not Connecting"))        
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | DismissMessage messageId -> // note: silently ignore unknown messageId
        let status, debugMessage =
            match state.Status with
            | Connected (connection, messageUis, messageId', messageText) ->
                let messageUis = messageUis |> List.filter (fun messageUi -> messageUi.Message.MessageId <> messageId)
                Connected (connection, messageUis, messageId', messageText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "DismissMessage input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | MessageTextChanged messageText ->
        let status, debugMessage =
            match state.Status with
            | Connected (connection, messages, messageId, _) -> Connected (connection, messages, messageId, messageText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "MessageTextChanged input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | SendMessage ->
        let status, cmd, debugMessage =
            match state.Status with
            | Connected (connection, messageUis, messageId, messageText) ->
                let message = { MessageId = messageId ; FromNickname = connection.Nickname ; Contents = messageText }
                let cmd = state.SendUiWsCmd (SendMessageWs (connection.ConnectionId, message))
                Connected (connection, { Message = message ; MessageType = Sent ; Timestamp = DateTime.Now } :: messageUis, MessageId.Create (), String.Empty), cmd, None
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "SendMessage input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | SendMessageResult (Ok message) ->
        let status, debugMessage =
            match state.Status with
            | Connected (connection, messageUis, messageId, messageText) ->
                let messageUis =
                    messageUis
                    |> List.map (fun messageUi ->
                        match messageUi.MessageType with
                        | Sent when messageUi.Message.MessageId = message.MessageId -> { messageUi with MessageType = Confirmed }
                        | _ -> messageUi)
                Connected (connection, messageUis, messageId, messageText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "SendMessageResult (Ok _) input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | SendMessageResult (Error (messageId, errorText)) ->
        let status, debugMessage, cmd =
            match state.Status with
            | Connected (connection, messageUis, messageId', messageText) ->
                let messageUis =
                    messageUis
                    |> List.map (fun messageUi ->
                        match messageUi.MessageType with
                        | Sent when messageUi.Message.MessageId = messageId -> { messageUi with MessageType = SendFailed errorText }
                        | _ -> messageUi)
                Connected (connection, messageUis, messageId', messageText), None, errorToastCmd "Unable to send message"
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "SendMessageResult (Error _) input when Status is not Connected")), Cmd.none
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | Disconnect ->
        let status, cmd, debugMessage =
            match state.Status with
            | Connected (connection, _, _, _) ->
                Disconnecting connection, state.SendUiWsCmd (DisconnectWs connection), None
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "Disconnect input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | DisconnectResult (Ok connection) ->
        let status, cmd, debugMessage =
            match state.Status with
            | Disconnecting connection' when connection' = connection ->
                NotConnected (ConnectionId.Create (), connection'.Nickname, None, None), warningToastCmd "You have disconnected", None
            | Disconnecting connection' ->
                state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText (sprintf "Disconnecting %A but DisconnectResult (Ok _) is %A" connection' connection)))
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "DisconnectResult (Ok _) input when Status is not Disconnecting"))     
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | DisconnectResult (Error errorText') ->
        let status, cmd, debugMessage =
            match state.Status with
            | Disconnecting connection ->
                let debugMessage = Some (debugMessage (errorText (sprintf "DisconnectResult -> Disconnecting %A -> %s" connection errorText')))
                NotConnected (ConnectionId.Create (), connection.Nickname, None, None), errorToastCmd "An error occurred during disconnection", debugMessage
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "DisconnectResult (Error _) input when Status is not Disconnecting"))        
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | SendMessageOther message ->
        let status, debugMessage =
            match state.Status with
            | Connected (connection, messageUis, messageId, messageText) ->
                Connected (connection, { Message = message ; MessageType = Confirmed ; Timestamp = DateTime.Now } :: messageUis, messageId, messageText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "SendMessageOther input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | UserConnectedOther nickname ->
        let cmd, debugMessage =
            match state.Status with
            | Connected _ -> infoToastCmd (sprintf "%s has connected" nickname), None
            | _ -> Cmd.none, Some (debugMessage (shouldNeverHappenText "UserConnectedOther input when Status is not Connected"))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | UserDisconnectedOther nickname ->
        let cmd, debugMessage =
            match state.Status with
            | Connected _ -> infoToastCmd (sprintf "%s has disconnected" nickname), None
            | _ -> Cmd.none, Some (debugMessage (shouldNeverHappenText "UserDisconnectedOther input when Status is not Connected"))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    (* TEMP-NMB: Prevent warning for unmatched Input cases...
    | _ -> state, Cmd.none
    ...NMB-TEMP *)
