module Aornota.Sweepstake2018.Server.Agents.Entities.Users

open Aornota.Common.IfDebug
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Text

type private User = { Rvn : Rvn ; UserName : UserName ; PasswordSalt : Salt ; PasswordHash : Hash ; UserType : UserType }

type private UsersInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | OnUsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | HandleSignInCmd of sessionId : SessionId * userName : UserName * password : Password * reply : AsyncReplyChannel<Result<AuthUser, SignInCmdError>>
    | HandleChangePasswordCmd of token : ChangePasswordToken * auditUserId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleCreateUserCmd of token : CreateUserToken * auditUserId : UserId * userId : UserId * userName : UserName * password : Password * userType : UserType * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleResetPasswordCmd of token : ResetPasswordToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleChangeUserTypeCmd of token : ChangeUserTypeToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * userType : UserType * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>

let private log category = consoleLogger.Log (Entity Entity.Users, category)

let private logResult resultSource successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        log (Info (sprintf "%s Ok%s" resultSource successText))
    | Error error -> log (Danger (sprintf "%s Error -> %A" resultSource error))

let private rng = RandomNumberGenerator.Create ()
let private sha512 = SHA512.Create ()
let private encoding = Encoding.UTF8

let private salt () =
    let bytes : byte [] = Array.zeroCreate 32
    rng.GetBytes bytes
    Salt (Convert.ToBase64String bytes)

let private hash (Password password) (Salt salt) =
    let bytes = encoding.GetBytes (sprintf "%s|%s" password salt) |> sha512.ComputeHash // note: password is therefore case-sensitive
    Hash (Convert.ToBase64String bytes)

let private applyUserEvent debugSource idAndUserResult (nextRvn, userEvent:UserEvent) =
    let otherError errorText = otherError (sprintf "%s#applyUserEvent" debugSource) errorText
    match idAndUserResult, userEvent with
    | Ok (userId, _), _ when userId <> userEvent.UserId -> // note: should never happen
        otherError (ifDebug (sprintf "UserId mismatch for %A -> %A" userId userEvent) UNEXPECTED_ERROR)
    | Ok (userId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        otherError (ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" userId nextRvn userEvent) UNEXPECTED_ERROR)
    | Ok (userId, Some user), _ when validateNextRvn (Some user.Rvn) nextRvn |> not -> // note: should never happen
        otherError (ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" userId user.Rvn nextRvn userEvent) UNEXPECTED_ERROR)
    | Ok (userId, None), UserCreated (_, userName, passwordSalt, passwordHash, userType) ->
        Ok (userId, Some { Rvn = nextRvn ; UserName = userName ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; UserType = userType })
    | Ok (userId, None), _ -> // note: should never happen
        otherError (ifDebug (sprintf "Invalid initial UserEvent for %A -> %A" userId userEvent) UNEXPECTED_ERROR)
    | Ok (userId, Some user), UserCreated _ -> // note: should never happen
        otherError (ifDebug (sprintf "Invalid non-initial UserEvent for %A (%A) -> %A" userId user userEvent) UNEXPECTED_ERROR)
    | Ok (userId, Some user), PasswordChanged (_, passwordSalt, passwordHash) ->
        Ok (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash })
    | Ok (userId, Some user), PasswordReset (_, passwordSalt, passwordHash) ->
        Ok (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash })
    | Ok (userId, Some user), UserTypeChanged (_, userType) ->
        Ok (userId, Some { user with Rvn = nextRvn ; UserType = userType })
    | Error error, _ -> Error error

let private initializeUsers debugSource (usersEvents:(UserId * (Rvn * UserEvent) list) list) =
    let debugSource = sprintf "%s#initializeUsers" debugSource
    let users = new Dictionary<UserId, User> ()
    let results =
        usersEvents
        |> List.map (fun (userId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndUserResult (rvn, userEvent) -> applyUserEvent debugSource idAndUserResult (rvn, userEvent)) (Ok (userId, None))
            | [] -> otherError debugSource (ifDebug (sprintf "No UserEvents for %A" userId) UNEXPECTED_ERROR)) // note: should never happen
    results
    |> List.choose (fun idAndUserResult -> match idAndUserResult with | Ok (userId, Some user) -> Some (userId, user) | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (userId, user) -> users.Add (userId, user))
    let errors =
        results
        |> List.choose (fun idAndUserResult ->
            match idAndUserResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> Some (OtherError (ifDebug (sprintf "%s: applyUserEvent returned Ok (_, None)" debugSource) UNEXPECTED_ERROR)) // note: should never happen
            | Error error -> Some error)
    users, errors

let private updateUser userId user (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then users.[userId] <- user
    users

let private tryFindUser debugSource userId (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then Ok (userId, users.Item userId) else otherCmdError debugSource (ifDebug (sprintf "%A does not exist" userId) UNEXPECTED_ERROR)

let private tryApplyUserEvent debugSource userId user nextRvn userEvent =
    match applyUserEvent debugSource (Ok (userId, user)) (nextRvn, userEvent) with
    | Ok (_, Some user) -> Ok (user, nextRvn, userEvent)
    | Ok (_, None) -> otherCmdError debugSource (ifDebug "applyUserEvent returned Ok (_, None)" UNEXPECTED_ERROR) // note: should never happen
    | Error otherError -> Error (OtherCmdError otherError)

let private tryWriteUserEventAsync auditUserId rvn userEvent (user:User) = async {
    let! result = (auditUserId, rvn, userEvent) |> persistence.WriteUserEventAsync
    return match result with | Ok _ -> Ok (userEvent.UserId, user) | Error persistenceError -> Error (CmdPersistenceError persistenceError) }

// TODO-NMB-HIGH: Handle auto-sign in (i.e. via Jwt) - if not handled by Connections agent?...

type Users () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                log (Info "Start when awaitingStart -> pendingOnUsersEventsRead")
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | Reset _ -> log (Agent (IgnoredInput "Reset when awaitingStart")) ; return! awaitingStart ()
            | OnUsersEventsRead _ -> log (Agent (IgnoredInput "OnUsersEventsRead when awaitingStart")) ; return! awaitingStart ()
            | HandleSignInCmd _ -> log (Agent (IgnoredInput "HandleSignInCmd when awaitingStart")) ; return! awaitingStart ()
            | HandleChangePasswordCmd _ -> log (Agent (IgnoredInput "HandleChangePasswordCmd when awaitingStart")) ; return! awaitingStart ()
            | HandleCreateUserCmd _ -> log (Agent (IgnoredInput "HandleCreateUserCmd when awaitingStart")) ; return! awaitingStart ()
            | HandleResetPasswordCmd _ -> log (Agent (IgnoredInput "HandleResetPasswordCmd when awaitingStart")) ; return! awaitingStart ()
            | HandleChangeUserTypeCmd _ -> log (Agent (IgnoredInput "HandleChangeUserTypeCmd when awaitingStart")) ; return! awaitingStart () }
        and pendingOnUsersEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingOnUsersEventsRead ()
            | Start _ -> log (Agent (IgnoredInput "Start when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | Reset _ -> log (Agent (IgnoredInput "Reset when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead usersEvents ->
                let users, errors = initializeUsers "Users.pendingOnUsersEventsRead.OnUsersEventsRead" usersEvents
                errors |> List.iter (fun (OtherError error) -> log (Danger error))
                log (Info (sprintf "OnUsersEventsRead when pendingOnUsersEventsRead -> managingUsers (%i user/s)" users.Count))
                UsersRead (users |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> userId, user.UserName, user.UserType)) |> broadcaster.Broadcast       
                return! managingUsers users
            | HandleSignInCmd _ -> log (Agent (IgnoredInput "HandleSignInCmd when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | HandleChangePasswordCmd _ -> log (Agent (IgnoredInput "HandleChangePasswordCmd when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | HandleCreateUserCmd _ -> log (Agent (IgnoredInput "HandleCreateUserCmd when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | HandleResetPasswordCmd _ -> log (Agent (IgnoredInput "HandleResetPasswordCmd when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead ()
            | HandleChangeUserTypeCmd _ -> log (Agent (IgnoredInput "HandleChangeUserTypeCmd when pendingOnUsersEventsRead")) ; return! pendingOnUsersEventsRead () }
        and managingUsers users = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingUsers users
            | Start _ -> log (Agent (IgnoredInput (sprintf "Start when managingUsers (%i user/s)" users.Count))) ; return! managingUsers users
            | Reset reply ->
                log (Info (sprintf "Reset when managingUsers (%i user/s) -> pendingOnUsersEventsRead" users.Count))
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead _ -> log (Agent (IgnoredInput (sprintf "OnUsersEventsRead when managingUsers (%i user/s)" users.Count))) ; return! managingUsers users
            | HandleSignInCmd (sessionId, userName, password, reply) -> // TODO-NMB-HIGH: Is sessionId necessary?...
                let invalidCredentials errorText = Error (InvalidCredentials errorText)
                log (Verbose (sprintf "HandleSignInCmd for %A (%A) when managingUsers (%i user/s)" userName sessionId users.Count))
                let result =
                    match validateUserName [] userName with | None -> Ok () | Some errorText -> invalidCredentials (Some errorText)
                    |> Result.bind (fun _ -> match validatePassword password with | None -> Ok () | Some errorText -> invalidCredentials (Some errorText))
                    |> Result.bind (fun _ ->
                        let matches = users |> List.ofSeq |> List.choose (fun (KeyValue (userId, user)) -> if user.UserName = userName then Some (userId, user) else None)
                        match matches with
                        | [ userId, user ] ->
                            if hash password user.PasswordSalt <> user.PasswordHash then invalidCredentials None
                            else
                                if user.UserType = PersonaNotGrata then invalidCredentials (Some "You are not permitted to sign in to this system")
                                else
                                    let (UserName userName) = user.UserName
                                    Ok { UserId = userId ; SessionId = sessionId ; UserName = userName }
                        | _ :: _ -> invalidCredentials None // note: multiple matches for userName [should never happen]
                        | [] -> invalidCredentials None)
                let successText = (fun (authUser:AuthUser) -> Some (sprintf "%A %A (%A)" (UserName authUser.UserName) authUser.UserId authUser.SessionId))
                logResult "HandleSignInCmd" successText result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! managingUsers users
            | HandleChangePasswordCmd (ChangePasswordToken onlyUserId, auditUserId, currentRvn, password, reply) ->
                let debugSource = "Users.managingUsers.HandleChangePasswordCmd"
                log (Verbose (sprintf "HandleChangePasswordCmd for %A (%A) when managingUsers (%i user/s)" auditUserId currentRvn users.Count))
                let result =
                    if onlyUserId = auditUserId then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ -> users |> tryFindUser debugSource auditUserId)
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> Ok (userId, user) | Some errorText -> otherCmdError debugSource errorText)
                    |> Result.bind (fun (userId, user) ->
                        if hash password user.PasswordSalt <> user.PasswordHash then Ok (userId, user)
                        else otherCmdError debugSource "New password must not be the same as your current password")
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordChanged (auditUserId, salt, hash password salt)
                        tryApplyUserEvent debugSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                logResult "HandleChangePasswordCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                let users = match result with | Ok (userId, user) -> users |> updateUser userId user | Error _ -> users
                return! managingUsers users
            | HandleCreateUserCmd (CreateUserToken onlyUserTypes, auditUserId, userId, userName, password, userType, reply) ->
                let debugSource = "Users.managingUsers.HandleCreateUserCmd"
                log (Verbose (sprintf "HandleCreateUserCmd for %A (%A %A) when managingUsers (%i user/s)" userId userName userType users.Count))
                let result =
                    if onlyUserTypes |> List.contains userType then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ ->
                        if users.ContainsKey userId |> not then Ok ()
                        else otherCmdError debugSource (ifDebug (sprintf "%A already exists" userId) UNEXPECTED_ERROR))
                    |> Result.bind (fun _ ->
                        let userNames = users |> List.ofSeq |> List.map (fun (KeyValue (_, user)) -> user.UserName)
                        match validateUserName userNames userName with | None -> Ok () | Some errorText -> otherCmdError debugSource errorText)
                    |> Result.bind (fun _ -> match validatePassword password with | None -> Ok () | Some errorText -> otherCmdError debugSource errorText)
                    |> Result.bind (fun _ ->
                        let salt = salt ()
                        let userEvent = UserCreated (userId, userName, salt, hash password salt, userType)
                        tryApplyUserEvent debugSource userId None (Rvn 1) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                let successText = (fun user -> Some (sprintf "Audit%A %A" auditUserId user))
                logResult "HandleCreateUserCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> tupleError userId |> reply.Reply
                match result with | Ok (userId, user) -> users.Add (userId, user) | Error _ -> ()
                return! managingUsers users
            | HandleResetPasswordCmd (ResetPasswordToken, auditUserId, userId, currentRvn, password, reply) ->
                let debugSource = "Users.managingUsers.HandleResetPasswordCmd"
                log (Verbose (sprintf "HandleResetPasswordCmd for %A (%A) when managingUsers (%i user/s)" userId currentRvn users.Count))
                // TODO-NMB-HIGH: Check ResetPasswordToken data (once it exists)...
                let result =
                    users |> tryFindUser debugSource userId
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> Ok (userId, user) | Some errorText -> otherCmdError debugSource errorText)
                    // Note: Do not check if password is the same as the current password (cf. HandleChangePasswordCmd) as this would be a leak of security information.
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordReset (userId, salt, hash password salt)
                        tryApplyUserEvent debugSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                logResult "HandleResetPasswordCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> tupleError userId |> reply.Reply
                let users = match result with | Ok (userId, user) -> users |> updateUser userId user | Error _ -> users
                return! managingUsers users
            | HandleChangeUserTypeCmd (ChangeUserTypeToken, auditUserId, userId, currentRvn, userType, reply) ->
                let debugSource = "Users.managingUsers.HandleChangeUserTypeCmd"
                log (Verbose (sprintf "HandleChangeUserTypeCmd %A for %A (%A) when managingUsers (%i user/s)" userType userId currentRvn users.Count))
                // TODO-NMB-HIGH: Check ChangeUserTypeToken data (once it exists)...
                let result =
                    users |> tryFindUser debugSource userId
                    |> Result.bind (fun (userId, user) ->
                        if userType <> user.UserType then Ok (userId, user)
                        else otherCmdError debugSource (ifDebug (sprintf "New UserType must not be the same as current UserType (%A)" user.UserType) UNEXPECTED_ERROR))
                    |> Result.bind (fun (userId, user) ->
                        let userEvent = UserTypeChanged (userId, userType)
                        tryApplyUserEvent debugSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                logResult "HandleChangeUserTypeCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> tupleError userId |> reply.Reply
                let users = match result with | Ok (userId, user) -> users |> updateUser userId user | Error _ -> users
                return! managingUsers users }
        log (Info "agent instantiated -> awaitingStart")
        awaitingStart ())
    do agent.Error.Add (logAgentException (Entity Entity.Users)) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in UserEventWritten events (since Users agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event -> match event with | UsersEventsRead usersEvents -> usersEvents |> self.OnUsersEventsRead | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            log (Info (sprintf "agent subscribed to UsersEventsRead broadcasts -> %A" subscriptionId))
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            log (Info "agent has already been started")
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.OnUsersEventsRead usersEvents = OnUsersEventsRead usersEvents |> agent.Post
    member __.HandleSignInCmdAsync (sessionId, userName, password) = (fun reply -> HandleSignInCmd (sessionId, userName, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleCreateUserCmdAsync (token, auditUserId, userId, userName, password, userType) = (fun reply -> HandleCreateUserCmd (token, auditUserId, userId, userName, password, userType, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangePasswordCmdAsync (token, auditUserId, currentRvn, password) = (fun reply -> HandleChangePasswordCmd (token, auditUserId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleResetPasswordCmdAsync (token, auditUserId, userId, currentRvn, password) = (fun reply -> HandleResetPasswordCmd (token, auditUserId, userId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangeUserTypeCmdAsync (token, auditUserId, userId, currentRvn, userType) = (fun reply -> HandleChangeUserTypeCmd (token, auditUserId, userId, currentRvn, userType, reply)) |> agent.PostAndAsyncReply

let users = Users ()
