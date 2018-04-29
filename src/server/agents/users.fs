module Aornota.Sweepstake2018.Server.Agents.Users

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
    | OnUsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | HandleSignInCmd of sessionId : SessionId * userName : UserName * password : Password * reply : AsyncReplyChannel<Result<AuthUser, SignInCmdError>>
    | HandleChangePasswordCmd of token : ChangePasswordToken * auditUserId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleCreateUserCmd of token : CreateUserToken * auditUserId : UserId * userId : UserId * userName : UserName * password : Password * userType : UserType * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleResetPasswordCmd of token : ResetPasswordToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>
    | HandleChangeUserTypeCmd of token : ChangeUserTypeToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * userType : UserType * reply : AsyncReplyChannel<Result<unit, UserId * AuthCmdError<string>>>

let private log category = consoleLogger.Log (Source.Users, category)

let private rng = RandomNumberGenerator.Create ()
let private sha512 = SHA512.Create ()
let private encoding = Encoding.UTF8

let private salt () =
    let bytes : byte [] = Array.zeroCreate 32
    rng.GetBytes bytes
    Salt (Convert.ToBase64String bytes)

let private hash (Password password) (Salt salt) =
    let bytes = encoding.GetBytes (sprintf "%s|%s" password salt) |> sha512.ComputeHash
    Hash (Convert.ToBase64String bytes)

let private applyUserEvent idAndUserResult (nextRvn, userEvent:UserEvent) =
    let otherError errorText = otherError "applyUserEvent" errorText
    match idAndUserResult, userEvent with
    | Ok (userId, _), _ when userId <> userEvent.UserId -> otherError (sprintf "UserId mismatch for %A -> %A" userId userEvent)
    | Ok (userId, None), _ when validateNextRvn None nextRvn |> not -> otherError (sprintf "Invalid initial Rvn for %A -> %A (%A)" userId nextRvn userEvent)
    | Ok (userId, Some user), _ when validateNextRvn (Some user.Rvn) nextRvn |> not -> otherError (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" userId user.Rvn nextRvn userEvent)
    | Ok (userId, None), UserCreated (_, userName, passwordSalt, passwordHash, userType) ->
        Ok (userId, Some { Rvn = nextRvn ; UserName = userName ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; UserType = userType })
    | Ok (userId, None), _ -> otherError (sprintf "Invalid initial UserEvent for %A -> %A" userId userEvent)
    | Ok (userId, Some user), UserCreated _ -> otherError (sprintf "Invalid non-initial UserEvent for %A (%A) -> %A" userId user userEvent)
    | Ok (userId, Some user), PasswordChanged (_, passwordSalt, passwordHash) ->
        Ok (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash })
    | Ok (userId, Some user), PasswordReset (_, passwordSalt, passwordHash) ->
        Ok (userId, Some { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash })
    | Ok (userId, Some user), UserTypeChanged (_, userType) ->
        Ok (userId, Some { user with Rvn = nextRvn ; UserType = userType })
    | Error error, _ -> Error error

let private initializeUsers (usersEvents:(UserId * (Rvn * UserEvent) list) list) =
    let users = new Dictionary<UserId, User>()
    usersEvents
    |> List.map (fun (userId, events) -> events |> List.fold (fun idAndUserResult (rvn, userEvent) -> applyUserEvent idAndUserResult (rvn, userEvent)) (Ok (userId, None)))
    |> List.choose (fun idAndUser -> match idAndUser with | Ok (userId, Some user) -> Some (userId, user) | Ok (_, None) | Error _ -> None) // note: silently discard failures
    |> List.iter (fun (userId, user) -> users.Add (userId, user))
    users

let private updateUser userId user (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then users.[userId] <- user
    users

let private tryFindUser errorSource userId (users:Dictionary<UserId, User>) =
    if users.ContainsKey userId then Ok (userId, users.Item userId) else otherCmdError errorSource (sprintf "userId %A does not exist" userId)

let private tryApplyUserEvent errorSource userId user nextRvn userEvent =
    match applyUserEvent (Ok (userId, user)) (nextRvn, userEvent) with
    | Ok (_, Some user) -> Ok (user, nextRvn, userEvent)
    | Ok (_, None) -> otherCmdError errorSource "applyUserEvent returned Ok (_, None)"
    | Error otherError -> Error (OtherCmdError otherError)

let private tryWriteUserEventAsync auditUserId rvn userEvent (user:User) = async {
    let! persistenceResult = persistence.WriteUserEventAsync (auditUserId, rvn, userEvent)
    return match persistenceResult with | Ok _ -> Ok (user, rvn, userEvent) | Error persistenceError -> Error (CmdPersistenceError persistenceError) }

// TODO-NMB-HIGH: Handle auto-sign in (i.e. via Jwt) - if not handled by Connections agent?...

type Users () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec pendingOnUsersEventsRead () = async {
            return! inbox.Scan (fun input ->
                match input with
                | OnUsersEventsRead usersEvents ->
                    let users = initializeUsers usersEvents
                    log (Info (sprintf "OnUsersEventsRead when pendingOnUsersEventsRead -> running -> %i user/s" users.Count))
                    UsersRead (users |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> userId, user.UserName, user.UserType)) |> broadcaster.Broadcast       
                    Some (running users)
                | HandleSignInCmd _ | HandleChangePasswordCmd _ | HandleCreateUserCmd _ | HandleResetPasswordCmd _ | HandleChangeUserTypeCmd _ ->
                    log (SkippedInput (sprintf "%A when pendingOnUsersEventsRead -> pendingOnUsersEventsRead" input))
                    None) }
        and running (users:Dictionary<UserId, User>) = async {
            let! input = inbox.Receive ()
            match input with
            | OnUsersEventsRead _ -> return! running users // note: silently ignore OnUsersEventsRead once running
            | HandleSignInCmd (sessionId, userName, password, reply) -> // TODO-NMB-HIGH: Do we really need sessionId?...
                let invalidCredentials errorText = Error (InvalidCredentials errorText)
                let result =
                    match validateUserName [] userName with | None -> Ok () | Some errorText -> invalidCredentials (Some errorText)
                    |> Result.bind (fun _ -> match validatePassword password with | None -> Ok () | Some errorText -> invalidCredentials (Some errorText))
                    |> Result.bind (fun _ ->
                        let matches = users |> List.ofSeq |> List.choose (fun (KeyValue (userId, user)) -> if user.UserName = userName then Some (userId, user) else None)
                        match matches with
                        | [ userId, user ] ->
                            if user.UserType = PersonaNotGrata then invalidCredentials (Some "Your access has been revoked")
                            else
                                if hash password user.PasswordSalt <> user.PasswordHash then invalidCredentials None
                                else
                                    let (UserName userName) = user.UserName
                                    Ok { UserId = userId ; SessionId = sessionId ; UserName = userName }
                        | _ :: _ -> invalidCredentials None // note: multiple matches for userName [should never happen]
                        | [] -> invalidCredentials None)
                result |> reply.Reply
                return! running users
            | HandleChangePasswordCmd (ChangePasswordToken onlyUserId, auditUserId, currentRvn, password, reply) ->
                let errorSource = "HandleChangePasswordCmd"
                let result =
                    if onlyUserId = auditUserId then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ -> users |> tryFindUser errorSource auditUserId)
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> Ok (userId, user) | Some errorText -> otherCmdError errorSource errorText)
                    |> Result.bind (fun (userId, user) ->
                        if hash password user.PasswordSalt <> user.PasswordHash then Ok (userId, user)
                        else otherCmdError errorSource "New password must not be the same as your current password")
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordChanged (auditUserId, salt, hash password salt)
                        tryApplyUserEvent errorSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> Result.map ignore |> reply.Reply
                let users = match result with | Ok (user, _, _) -> users |> updateUser auditUserId user | Error _ -> users
                return! running users
            | HandleCreateUserCmd (CreateUserToken onlyUserTypes, auditUserId, userId, userName, password, userType, reply) ->
                let errorSource = "HandleCreateUserCmd"
                let result =
                    if onlyUserTypes |> List.contains userType then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ -> if users.ContainsKey userId |> not then Ok () else otherCmdError errorSource (sprintf "userId %A already exists" userId))
                    |> Result.bind (fun _ ->
                        let userNames = users |> List.ofSeq |> List.map (fun (KeyValue (_, user)) -> user.UserName)
                        match validateUserName userNames userName with | None -> Ok () | Some errorText -> otherCmdError errorSource errorText)
                    |> Result.bind (fun _ -> match validatePassword password with | None -> Ok () | Some errorText -> otherCmdError errorSource errorText)
                    |> Result.bind (fun _ ->
                        let salt = salt ()
                        let userEvent = UserCreated (userId, userName, salt, hash password salt, userType)
                        tryApplyUserEvent errorSource userId None (Rvn 1) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> discardOk |> tupleError userId |> reply.Reply
                match result with | Ok (user, _, _) -> users.Add (userId, user) | Error _ -> ()
                return! running users
            | HandleResetPasswordCmd (ResetPasswordToken, auditUserId, userId, currentRvn, password, reply) ->
                let errorSource = "HandleResetPasswordCmd"
                // TODO-NMB-HIGH: Check ResetPasswordToken data (once it exists)...
                let result =
                    users |> tryFindUser errorSource userId
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> Ok (userId, user) | Some errorText -> otherCmdError errorSource errorText)
                    // Note: Do not check if password is the same as the current password (cf. HandleChangePasswordCmd) as this would be a leak of security information.
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordReset (userId, salt, hash password salt)
                        tryApplyUserEvent errorSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> discardOk |> tupleError userId |> reply.Reply
                let users = match result with | Ok (user, _, _) -> users |> updateUser auditUserId user | Error _ -> users
                return! running users
            | HandleChangeUserTypeCmd (ChangeUserTypeToken, auditUserId, userId, currentRvn, userType, reply) ->
                let errorSource = "HandleChangeUserTypeCmd"
                // TODO-NMB-HIGH: Check ChangeUserTypeToken data (once it exists)...
                let result =
                    users |> tryFindUser errorSource userId
                    |> Result.bind (fun (userId, user) ->
                        let userEvent = UserTypeChanged (userId, userType)
                        tryApplyUserEvent errorSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> discardOk |> tupleError userId |> reply.Reply
                let users = match result with | Ok (user, _, _) -> users |> updateUser auditUserId user | Error _ -> users
                return! running users }
        log (Info "agent instantiated -> pendingOnUsersEventsRead")
        pendingOnUsersEventsRead ())
    do agent.Error.Add (logAgentExn Source.Users) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.OnUsersEventsRead usersEvents = OnUsersEventsRead usersEvents |> agent.Post
    member __.HandleSignInCmdAsync (sessionId, userName, password) = (fun reply -> HandleSignInCmd (sessionId, userName, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleCreateUserCmdAsync (token, auditUserId, userId, userName, password, userType) = (fun reply -> HandleCreateUserCmd (token, auditUserId, userId, userName, password, userType, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangePasswordCmdAsync (token, auditUserId, currentRvn, password) = (fun reply -> HandleChangePasswordCmd (token, auditUserId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleResetPasswordCmdAsync (token, auditUserId, userId, currentRvn, password) = (fun reply -> HandleResetPasswordCmd (token, auditUserId, userId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangeUserTypeCmdAsync (token, auditUserId, userId, currentRvn, userType) = (fun reply -> HandleChangeUserTypeCmd (token, auditUserId, userId, currentRvn, userType, reply)) |> agent.PostAndAsyncReply

let users = Users ()

let subscriberId = broadcaster.Subscribe (fun event ->
    match event with // note: not interested in UserEventWritten (since Users agent causes these in the first place - and will already have updated its state accordingly)
    | UsersEventsRead usersEvents -> users.OnUsersEventsRead usersEvents
    | _ -> ())
log (Info (sprintf "agent subscribed to UsersEventsRead broadcasts -> %A" subscriberId))

let ensureInstantiated () = () // note: Users agent [users] is static - so will only be instantiated when Users module (effectively a static class) is first referenced
