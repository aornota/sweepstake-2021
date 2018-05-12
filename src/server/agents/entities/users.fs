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
open Aornota.Sweepstake2018.Server.Jwt

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
    | HandleSignInCmd of sessionId : SessionId * userName : UserName * password : Password * reply : AsyncReplyChannel<Result<AuthUser, SignInCmdError<string>>>
    | HandleChangePasswordCmd of token : ChangePasswordToken option * auditUserId : UserId * currentRvn : Rvn * password : Password
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleCreateUserCmd of token : CreateUserToken option * auditUserId : UserId * userId : UserId * userName : UserName * password : Password * userType : UserType
        * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleResetPasswordCmd of token : ResetPasswordToken option * auditUserId : UserId * userId : UserId * currentRvn : Rvn * password : Password
        * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleChangeUserTypeCmd of token : ChangeUserTypeToken option * auditUserId : UserId * userId : UserId * currentRvn : Rvn * userType : UserType
        * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>

let private log category = (Entity Entity.Users, category) |> consoleLogger.Log

let private logResult resultSource successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" resultSource successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" resultSource error |> Danger |> log

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
        ifDebug (sprintf "UserId mismatch for %A -> %A" userId userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" userId nextRvn userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), _ when validateNextRvn (Some user.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" userId user.Rvn nextRvn userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, None), UserCreated (_, userName, passwordSalt, passwordHash, userType) ->
        (userId, Some { Rvn = nextRvn ; UserName = userName ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; UserType = userType }) |> Ok
    | Ok (userId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial UserEvent for %A -> %A" userId userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), UserCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial UserEvent for %A (%A) -> %A" userId user userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), PasswordChanged (_, passwordSalt, passwordHash) ->
        (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash }) |> Ok
    | Ok (userId, Some user), PasswordReset (_, passwordSalt, passwordHash) ->
        (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash }) |> Ok
    | Ok (userId, Some user), UserTypeChanged (_, userType) ->
        (userId, Some { user with Rvn = nextRvn ; UserType = userType }) |> Ok
    | Error error, _ -> error |> Error

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
    |> List.choose (fun idAndUserResult -> match idAndUserResult with | Ok (userId, Some user) -> (userId, user) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (userId, user) -> users.Add (userId, user))
    let errors =
        results
        |> List.choose (fun idAndUserResult ->
            match idAndUserResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyUserEvent returned Ok (_, None)" debugSource) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    users, errors

let private updateUser userId user (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then users.[userId] <- user
    users

let private tryFindUser debugSource userId (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then (userId, users.Item userId) |> Ok else ifDebug (sprintf "%A does not exist" userId) UNEXPECTED_ERROR |> otherCmdError debugSource

let private tryApplyUserEvent debugSource userId user nextRvn userEvent =
    match applyUserEvent debugSource (Ok (userId, user)) (nextRvn, userEvent) with
    | Ok (_, Some user) -> (user, nextRvn, userEvent) |> Ok
    | Ok (_, None) -> ifDebug "applyUserEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError debugSource // note: should never happen
    | Error otherError -> otherError |> OtherCmdError |> Error

let private tryWriteUserEventAsync auditUserId rvn userEvent (user:User) = async {
    let! result = (auditUserId, rvn, userEvent) |> persistence.WriteUserEventAsync
    return match result with | Ok _ -> (userEvent.UserId, user) |> Ok | Error persistenceError -> persistenceError |> CmdPersistenceError |> Error }

type Users () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersEventsRead" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUsersEventsRead _ -> "OnUsersEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleSignInCmd _ -> "HandleSignInCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePasswordCmd _ -> "HandleChangePasswordCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateUserCmd _ -> "HandleCreateUserCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleResetPasswordCmd _ -> "HandleResetPasswordCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangeUserTypeCmd _ -> "HandleChangeUserTypeCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingOnUsersEventsRead ()
            | Start _ -> "Start when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | Reset _ -> "Reset when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead usersEvents ->
                let users, errors = initializeUsers "Users.pendingOnUsersEventsRead.OnUsersEventsRead" usersEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "OnUsersEventsRead when pendingOnUsersEventsRead -> managingUsers (%i user/s)" users.Count |> Info |> log
                UsersRead (users |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> userId, user.Rvn, user.UserName, user.UserType)) |> broadcaster.Broadcast       
                return! managingUsers users
            | HandleSignInCmd _ -> "HandleSignInCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleChangePasswordCmd _ -> "HandleChangePasswordCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleCreateUserCmd _ -> "HandleCreateUserCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleResetPasswordCmd _ -> "HandleResetPasswordCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleChangeUserTypeCmd _ -> "HandleChangeUserTypeCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead () }
        and managingUsers users = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingUsers users
            | Start _ -> sprintf "Start when managingUsers (%i user/s)" users.Count |> IgnoredInput |> Agent |> log ; return! managingUsers users
            | Reset reply ->
                sprintf "Reset when managingUsers (%i user/s) -> pendingOnUsersEventsRead" users.Count |> Info |>log
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead _ -> sprintf "OnUsersEventsRead when managingUsers (%i user/s)" users.Count |> IgnoredInput |> Agent |> log ; return! managingUsers users
            | HandleSignInCmd (sessionId, userName, password, reply) ->
                let invalidCredentialsError errorText = errorText |> InvalidCredentials |> Error
                sprintf "HandleSignInCmd for %A when managingUsers (%i user/s)" userName users.Count |> Verbose |> log
                let result =
                    match validateUserName [] userName with | None -> () |> Ok | Some errorText -> errorText |> Some |> invalidCredentialsError
                    |> Result.bind (fun _ -> match validatePassword password with | None -> () |> Ok | Some errorText -> errorText |> Some |> invalidCredentialsError)
                    |> Result.bind (fun _ ->
                        let matches = users |> List.ofSeq |> List.choose (fun (KeyValue (userId, user)) -> if user.UserName = userName then (userId, user) |> Some else None)
                        match matches with
                        | [ userId, user ] ->
                            if hash password user.PasswordSalt <> user.PasswordHash then ifDebug ("Incorrect password" |> Some) None |> invalidCredentialsError
                            else
                                if user.UserType = PersonaNonGrata then "You are not permitted to sign in to this system" |> Some |> invalidCredentialsError
                                else
                                    let permissions = permissions userId user.UserType
                                    match (sessionId, userId, user.UserName, user.UserType, permissions, permissions |> UserTokens) |> toAuthUser with
                                    | Ok ok -> ok |> Ok
                                    | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> SignInCmdJwtError |> Error
                        | _ :: _ -> ifDebug (sprintf "Multiple matches for %A" userName |> Some) None |> invalidCredentialsError
                        | [] -> ifDebug (sprintf "No matches for %A" userName |> Some) None |> invalidCredentialsError)
                let successText = (fun (authUser:AuthUser) -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some)
                result |> logResult "HandleSignInCmd" successText // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! managingUsers users
            | HandleChangePasswordCmd (changePasswordToken, auditUserId, currentRvn, password, reply) ->
                let debugSource = "Users.managingUsers.HandleChangePasswordCmd"
                sprintf "HandleChangePasswordCmd for %A (%A) when managingUsers (%i user/s)" auditUserId currentRvn users.Count |> Verbose |> log

                // TODO-NMB: Error (&c.) piping...

                let result =
                    match changePasswordToken with
                    | Some changePasswordToken ->
                        if changePasswordToken.UserId = auditUserId then Ok () else Error (CmdAuthznError NotAuthorized)
                        |> Result.bind (fun _ -> users |> tryFindUser debugSource auditUserId)
                        |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> Ok (userId, user) | Some errorText -> otherCmdError debugSource errorText)
                        |> Result.bind (fun (userId, user) ->
                            if hash password user.PasswordSalt <> user.PasswordHash then Ok (userId, user)
                            else otherCmdError debugSource "New password must not be the same as your current password")
                        |> Result.bind (fun (userId, user) ->
                            let salt = salt ()
                            let userEvent = PasswordChanged (auditUserId, salt, hash password salt)
                            tryApplyUserEvent debugSource userId (Some user) (incrementRvn currentRvn) userEvent)
                    | None -> Error (CmdAuthznError NotAuthorized)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                logResult "HandleChangePasswordCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                let users = match result with | Ok (userId, user) -> users |> updateUser userId user | Error _ -> users
                return! managingUsers users
            | HandleCreateUserCmd (createUserToken, auditUserId, userId, userName, password, userType, reply) ->
                let debugSource = "Users.managingUsers.HandleCreateUserCmd"
                sprintf "HandleCreateUserCmd for %A (%A %A) when managingUsers (%i user/s)" userId userName userType users.Count |> Verbose |> log

                // TODO-NMB: Error (&c.) piping...

                let result =
                    match createUserToken with
                    | Some createUserToken ->
                        if createUserToken.UserTypes |> List.contains userType then Ok () else Error (CmdAuthznError NotAuthorized)
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
                    | None -> Error (CmdAuthznError NotAuthorized)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                let successText = (fun user -> Some (sprintf "Audit%A %A" auditUserId user))
                logResult "HandleCreateUserCmd" (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> tupleError userId |> reply.Reply
                match result with | Ok (userId, user) -> users.Add (userId, user) | Error _ -> ()
                return! managingUsers users
            | HandleResetPasswordCmd (_resetPasswordToken, auditUserId, userId, currentRvn, password, reply) ->
                let debugSource = "Users.managingUsers.HandleResetPasswordCmd"
                sprintf "HandleResetPasswordCmd for %A (%A) when managingUsers (%i user/s)" userId currentRvn users.Count |> Verbose |> log

                // TODO-NMB: Error (&c.) piping...
                // TODO-NMB-HIGH: Check ResetPasswordToken "data"...

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
            | HandleChangeUserTypeCmd (_changeUserTypeToken, auditUserId, userId, currentRvn, userType, reply) ->
                let debugSource = "Users.managingUsers.HandleChangeUserTypeCmd"
                sprintf "HandleChangeUserTypeCmd %A for %A (%A) when managingUsers (%i user/s)" userType userId currentRvn users.Count |> Verbose |> log

                // TODO-NMB: Error (&c.) piping...
                // TODO-NMB-HIGH: Check ChangeUserTypeToken "data"...

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
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Users |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in UserEventWritten events (since Users agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event -> match event with | UsersEventsRead usersEvents -> usersEvents |> self.OnUsersEventsRead | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to UsersEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.OnUsersEventsRead usersEvents = OnUsersEventsRead usersEvents |> agent.Post
    member __.HandleSignInCmdAsync (sessionId, userName, password) = (fun reply -> HandleSignInCmd (sessionId, userName, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleCreateUserCmdAsync (token, auditUserId, userId, userName, password, userType) =
        (fun reply -> HandleCreateUserCmd (token, auditUserId, userId, userName, password, userType, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangePasswordCmdAsync (token, auditUserId, currentRvn, password) =
        (fun reply -> HandleChangePasswordCmd (token, auditUserId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleResetPasswordCmdAsync (token, auditUserId, userId, currentRvn, password) =
        (fun reply -> HandleResetPasswordCmd (token, auditUserId, userId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangeUserTypeCmdAsync (token, auditUserId, userId, currentRvn, userType) =
        (fun reply -> HandleChangeUserTypeCmd (token, auditUserId, userId, currentRvn, userType, reply)) |> agent.PostAndAsyncReply

let users = Users ()
