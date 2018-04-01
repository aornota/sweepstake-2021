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

let private sendWsCmdAsync ws uiWs = async {
    // TEMP-NMB: Snooze - and fake error...
    do! Async.Sleep (random.Next (250, 1250))
    if (random.NextDouble () < 0.1) then failwith "Fake sendWsCmdAsync error! Even sadder"
    // ...NMB-TEMP
    let input =
        match uiWs with
        | ConnectWs connection ->
            // TEMP-NMB: Fake error...
            if connection.Nickname = "satan" then failwith "satan is a reserved nickname"
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws...
            ConnectResult (Ok connection)
        | SendMessageWs (connectionId, message) ->
            // TEMP-NMB: Fake error...
            if (random.NextDouble () < 0.1) then failwith (sprintf "Message '%s' is inappropriate" message.Contents)
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws...
            SendMessageResult (Ok message)
        | DisconnectWs connection ->
            // TEMP-NMB: Fake error...
            if connection.Nickname = "god" then failwith "god cannot disconnect"
            // ...NMB-TEMP
            // TODO-NMB: Implement via ws...
            DisconnectResult (Ok connection)
    return input }

let private initializeWsCmd =
    let initializeWs () = async {
        // TEMP-NMB: Snooze - and fake error...
        do! Async.Sleep (random.Next (100, 500))
        if (random.NextDouble () < 0.1) then failwith "Fake initializeWs error! Sad"
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
        { state with Status = NotConnected (ConnectionId.Create (), "neph", None, None) ; SendWsCmdAsync = sendWsCmdAsync }, Cmd.ofMsg Connect
        // ...or not...
        //{ state with Status = NotConnected (ConnectionId.Create (), String.Empty, None, None) ; SendWsCmdAsync = sendWsCmdAsync }, Cmd.none
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
                let messages = 
                    [
                        Sent ({ MessageId = MessageId.Create () ; Nickname = connection.Nickname ; Contents = "Test self-message #5" }, DateTime.Now)
                        SendFailed ({ MessageId = MessageId.Create () ; Nickname = connection.Nickname ; Contents = "Test self-message #4" }, DateTime.Now.AddSeconds (-17.))
                        Received ({ MessageId = MessageId.Create () ; Nickname = connection.Nickname ; Contents = "Test self-message #3" }, DateTime.Now.AddSeconds (-33.))
                        Received ({ MessageId = MessageId.Create () ; Nickname = "bubbles" ; Contents = "Test message #3" }, DateTime.Now.AddSeconds (-45.))
                        Received ({ MessageId = MessageId.Create () ; Nickname = connection.Nickname ; Contents = "Test self-message #2" }, DateTime.Now.AddMinutes (-2.2))
                        Received ({ MessageId = MessageId.Create () ; Nickname = "buttercup" ; Contents = "Test message #2" }, DateTime.Now.AddMinutes (-3.))
                        Received ({ MessageId = MessageId.Create () ; Nickname = connection.Nickname ; Contents = "Test self-message #1" }, DateTime.Now.AddMinutes (-5.8))
                        Received ({ MessageId = MessageId.Create () ; Nickname = "blossom" ; Contents = "Test message #1" }, DateTime.Now.AddMinutes (-8.))
                    ]
                Connected (connection, messages, MessageId.Create (), String.Empty), successToastCmd "You have connected", None
                // ...or not...
                //Connected (connection, [], String.Empty), successToastCmd "You have connected", None
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
    //| DismissMessage messageId -> // TODO-NMB: Silently ignore unknown messageId?...
    | MessageTextChanged messageText ->
        let status, debugMessage =
            match state.Status with
            | Connected (connection, messages, messageId, _) -> Connected (connection, messages, messageId, messageText), None
            | _ -> state.Status, Some (debugMessage (shouldNeverHappenText "MessageTextChanged input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, Cmd.none
    | SendMessage ->
        let status, cmd, debugMessage =
            match state.Status with
            | Connected (connection, messages, messageId, messageText) ->
                let message = { MessageId = messageId ; Nickname = connection.Nickname ; Contents = messageText }
                let cmd = sendWsCmd state.SendWsCmdAsync (SendMessageWs (connection.ConnectionId, message)) (fun exn -> SendMessageResult (Error (messageId, exn)))
                Connected (connection, Sent (message, DateTime.Now) :: messages, MessageId.Create (), String.Empty), cmd, None
            | _ -> state.Status, Cmd.none, Some (debugMessage (shouldNeverHappenText "SendMessage input when Status is not Connected"))
        { state with Status = status ; DebugMessages = state.DebugMessages |> addDebugMessageIfSome debugMessage }, cmd
    //| SendMessageResult (Ok message) -> // TODO-NMB: Change from Sent to Received...
    //| SendMessageResult (Error (messageId, exn)) -> // TODO-NMB: Change from Sent to SendFailed...
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
    //| SendMessageOther message -> // TODO-NMB: Add to state.Messages...
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
    // TEMP-NMB: Prevent warning for unmatched Input cases...
    | _ -> state, Cmd.none
    // ...NMB-TEMP
