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

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2018-ui-app-preferences"
let [<Literal>] private DEFAULT_TOAST_TIMEOUT = 3000

let private random = Random ()

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

// Note: Causes mysterious JavaScript errors if defined within initializeWsCmd (below).
let private sendWsCmdAsync ws uiWs = async {
    // TEMP-NMB: Snooze - and fake error...
    do! Async.Sleep (random.Next (250, 1250))
    if (random.NextDouble () < 0.1) then failwith "Fake sendWsCmdAsync error"
    // ...NMB-TEMP
    let input =
        match uiWs with
        | ConnectWs connection ->
            // TEMP-NMB: Fake error...
            if connection.Nickname = "satan" then failwith "'satan' is a reserved nickname"
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws...
            ConnectResult (Ok connection)
        | SendMessageWs (connectionId, message) ->
            // TEMP-NMB: Fake error...
            if (random.NextDouble () < 0.1) then failwith (sprintf "Message '%s' is inappropriate" message.Contents)
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws [and using connectionId]...
            SendMessageResult (Ok message)
        | DisconnectWs connection ->
            // TEMP-NMB: Fake error...
            if connection.Nickname = "god" then failwith "'god' cannot disconnect"
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws...
            DisconnectResult (Ok connection)
    return input }

let private initializeWsCmd =
    let initializeWs () = async {
        // TEMP-NMB: Snooze - and fake error...
        do! Async.Sleep (random.Next (100, 500))
        if (random.NextDouble () < 0.1) then failwith "Fake initializeWs error"
        // ...NMB-TEMP
        // TODO-NMB: Implement ws...
        let ws = ()
        return sendWsCmdAsync ws }
    Cmd.ofAsync initializeWs () (Ok >> InitializeWsResult) (Error >> InitializeWsResult)

let initialize () =
    let state = {
        DebugMessages = []
        UseDefaultTheme = true
        NavbarBurgerIsActive = false
        Status = ReadingPreferences
        SendWsCmdAsync = (fun uiWs -> async { return WsNotInitialized uiWs }) }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let transition input state =
    let addDebugMessageIfSome debugMessage debugMessages = match debugMessage with | Some debugMessage -> debugMessage :: debugMessages | None -> debugMessages
    let sendWsCmd sendWsCmdAsync uiWs fError : Cmd<Input> = Cmd.ofAsync sendWsCmdAsync uiWs id fError
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
        setBodyClass preferences.UseDefaultTheme
        // TEMP-NMB: Test using "uninitialized" SendWsCmdAsync...
        //let cmd = sendWsCmd state.SendWsCmdAsync (ConnectWs { ConnectionId = ConnectionId.Create () ; Nickname = "uninitializedWs" }) (Error >> ConnectResult)
        //{ state with UseDefaultTheme = preferences.UseDefaultTheme ; Status = InitializingWS }, Cmd.batch [ cmd ; initializeWsCmd ]
        // ...or not...
        { state with UseDefaultTheme = preferences.UseDefaultTheme ; Status = InitializingWS }, initializeWsCmd
        // ...NMB-TEMP
    | ReadPreferencesResult (Ok None) ->
        { state with Status = InitializingWS }, initializeWsCmd
    | ReadPreferencesResult (Error exn) ->
        let debugMessage = Some (debugMessage (errorText (sprintf "ReadPreferencesResult -> %s" exn.Message)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.ofMsg (ReadPreferencesResult (Ok None))
    | WritePreferencesResult (Ok _) ->
        state, Cmd.none
    | WritePreferencesResult (Error exn) ->
        let debugMessage = Some (debugMessage (errorText (sprintf "WritePreferencesResult -> %s" exn.Message)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | InitializeWsResult (Ok sendWsCmdAsync) ->
        // TEMP-NMB: Auto-connect...
        //{ state with Status = NotConnected (ConnectionId.Create (), "neph", None, None) ; SendWsCmdAsync = sendWsCmdAsync }, Cmd.ofMsg Connect
        // ...or not...
        { state with Status = NotConnected (ConnectionId.Create (), String.Empty, None, None) ; SendWsCmdAsync = sendWsCmdAsync }, Cmd.none
        // ...NMB-TEMP
    | InitializeWsResult (Error exn) ->
        let debugMessage = Some (debugMessage (errorText (sprintf "InitializeWsResult -> %s" exn.Message)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage ; Status = ServiceUnavailable }, errorToastCmd "Service is not available"
    | WsNotInitialized uiWs ->
        let debugMessage = Some (debugMessage (shouldNeverHappenText (sprintf "WsNotInitialized -> %A" uiWs)))
        { state with DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
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
                    Connecting connection, sendWsCmd state.SendWsCmdAsync (ConnectWs connection) (Error >> ConnectResult), None
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
                        { Message = { MessageId = MessageId.Create () ; FromNickname = connection.Nickname ; Contents = "I am w*nking as I write this..." } ; MessageType = SendFailed (exn "Message 'I am w*nking as I write this...' is inappropriate") ; Timestamp = DateTime.Now.AddSeconds (-33.) }
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
    | ConnectResult (Error exn) ->
        let status, debugMessage =
            match state.Status with
            | Connecting connection -> NotConnected (ConnectionId.Create (), connection.Nickname, None, Some exn.Message), None
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
                let cmd = sendWsCmd state.SendWsCmdAsync (SendMessageWs (connection.ConnectionId, message)) (fun exn -> SendMessageResult (Error (messageId, exn)))
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
    | SendMessageResult (Error (messageId, exn)) ->
        let status, debugMessage, cmd =
            match state.Status with
            | Connected (connection, messageUis, messageId', messageText) ->
                let messageUis =
                    messageUis
                    |> List.map (fun messageUi ->
                        match messageUi.MessageType with
                        | Sent when messageUi.Message.MessageId = messageId -> { messageUi with MessageType = SendFailed exn }
                        | _ -> messageUi)
                Connected (connection, messageUis, messageId', messageText), None, errorToastCmd "Unable to send message"
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "SendMessageResult (Error _) input when Status is not Connected")), Cmd.none
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    | Disconnect ->
        let status, cmd, debugMessage =
            match state.Status with
            | Connected (connection, _, _, _) ->
                Disconnecting connection, sendWsCmd state.SendWsCmdAsync (DisconnectWs connection) (Error >> DisconnectResult), None
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
    | DisconnectResult (Error exn) ->
        let status, cmd, debugMessage =
            match state.Status with
            | Disconnecting connection ->
                let debugMessage = Some (debugMessage (errorText (sprintf "DisconnectResult -> Disconnecting %A -> %s" connection exn.Message)))
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
