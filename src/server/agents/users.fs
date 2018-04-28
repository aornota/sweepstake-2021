module Aornota.Sweepstake2018.Server.Agents.Users

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.Persistence
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

open System.Collections.Generic

type private User = { Rvn : Rvn ; UserName : UserName ; PasswordSalt : Salt ; PasswordHash : Hash ; UserType : UserType }

// TEMP-NMB: Define authorization tokens elsewhere...
type ChangePasswordToken = private | ChangePasswordToken of onlyUserId : UserId
type CreateUserToken = private | CreateUserToken of onlyUserTypes : UserType list
type ResetPasswordToken = private | ResetPasswordToken // TODO-NMB-HIGH: of "targets" [not self; UserType list]...
type ChangeUserTypeToken = private | ChangeUserTypeToken // TODO-NMB-HIGH: of "targets" [not self; UserType list] * UserType list...
// ...NMB-TEMP

type private UsersInput =
    | OnUsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | HandleSignInCmd of sessionId : SessionId * userName : UserName * password : Password * reply : AsyncReplyChannel<Result<AuthUser, SignInCmdError>>
    | HandleChangePasswordCmd of token : ChangePasswordToken * auditUserId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleCreateUserCmd of token : CreateUserToken * auditUserId : UserId * userId : UserId * userName : UserName * password : Password * userType : UserType * reply : AsyncReplyChannel<Result<UserId, UserId * AuthCmdError<string>>>
    | HandleResetPasswordCmd of token : ResetPasswordToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * password : Password * reply : AsyncReplyChannel<Result<UserId, UserId * AuthCmdError<string>>>
    | HandleChangeUserTypeCmd of token : ChangeUserTypeToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * userType : UserType * reply : AsyncReplyChannel<Result<UserId, UserId * AuthCmdError<string>>>

let private salt () = Salt "Fake Salt"

let private hash (Password password) (Salt salt) = Hash (sprintf "Fake Hash | %s | %s" password salt)

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

// TODO-NMB-HIGH: Handle auto-sign in (i.e. via Jwt) - if not handled by connections?...

type Users () =
    let updateUser userId user (users:Dictionary<UserId, User>) =
        if users.ContainsKey userId then users.[userId] <- user
        users
    let tryFindUser errorSource userId (users:Dictionary<UserId, User>) =
        if users.ContainsKey userId then Ok (userId, users.Item userId) else otherCmdError errorSource (sprintf "userId %A does not exist" userId)
    let tryApplyUserEvent errorSource userId user nextRvn userEvent =
        match applyUserEvent (Ok (userId, user)) (nextRvn, userEvent) with
        | Ok (_, Some user) -> Ok (user, nextRvn, userEvent)
        | Ok (_, None) -> otherCmdError errorSource "applyUserEvent returned Ok (_, None)"
        | Error otherError -> Error (OtherCmdError otherError)
    let tryWriteUserEventAsync auditUserId rvn userEvent (user:User) = async {
        let! persistenceResult = persistence.WriteUserEventAsync (auditUserId, rvn, userEvent)
        return match persistenceResult with | Ok _ -> Ok (user, rvn, userEvent) | Error persistenceError -> Error (CmdPersistenceError persistenceError) }
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec running (users:Dictionary<UserId, User>) = async {
            let! message = inbox.Receive ()
            match message with
            | OnUsersEventsRead usersEvents ->
                let users = if users.Count = 0 then initializeUsers usersEvents else users // note: silently ignore OnUsersEventsRead if already "initialized" [should never happen]
                UsersRead (users |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> userId, user.UserName, user.UserType)) |> broadcaster.Broadcast       
                return! running users
            | HandleSignInCmd (sessionId, userName, _password, reply) -> // TODO-NMB-HIGH: Do we really need sessionId?...
                let matches = users |> List.ofSeq |> List.choose (fun (KeyValue (userId, user)) -> if user.UserName = userName then Some (userId, user) else None)
                let result =
                    match matches with
                    | [ userId, user ] ->
                        if user.UserType = PersonaNotGrata then Error InvalidCredentials
                        else
                            // TODO-NMB-HIGH: Actually check password!...
                            let (UserName userName) = user.UserName
                            Ok { UserId = userId ; SessionId = sessionId ; UserName = userName }
                    | _ :: _ -> Error InvalidCredentials // note: multiple matches for userName [should never happen]
                    | [] -> Error InvalidCredentials
                result |> reply.Reply
                return! running users
            | HandleChangePasswordCmd (ChangePasswordToken onlyUserId, auditUserId, currentRvn, password, reply) -> // TODO-NMB: Validate password?...
                let errorSource = "HandleChangePasswordCmd"
                let result =
                    if onlyUserId = auditUserId then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ -> users |> tryFindUser errorSource auditUserId)
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordChanged (auditUserId, salt, hash password salt)
                        tryApplyUserEvent errorSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> Result.map ignore |> reply.Reply
                let users = match result with | Ok (user, _, _) -> users |> updateUser auditUserId user | Error _ -> users
                return! running users
            | HandleCreateUserCmd (CreateUserToken onlyUserTypes, auditUserId, userId, userName, password, userType, reply) -> // TODO-NMB: Validate userName? password?...
                let errorSource = "HandleCreateUserCmd"
                let result =
                    if onlyUserTypes |> List.contains userType then Ok () else Error (CmdAuthznError NotAuthorized)
                    |> Result.bind (fun _ -> if users.ContainsKey userId |> not then Ok () else otherCmdError errorSource (sprintf "userId %A already exists" userId))
                    |> Result.bind (fun _ ->
                        let salt = salt ()
                        let userEvent = UserCreated (userId, userName, salt, hash password salt, userType)
                        tryApplyUserEvent errorSource userId None (Rvn 1) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> Result.map (fun _ -> userId) |> tupleError userId |> reply.Reply
                match result with | Ok (user, _, _) -> users.Add (userId, user) | Error _ -> ()
                return! running users
            | HandleResetPasswordCmd (ResetPasswordToken, auditUserId, userId, currentRvn, password, reply) ->
                let errorSource = "HandleResetPasswordCmd"
                // TODO-NMB-HIGH: Check ResetPasswordToken data (once it exists)...
                let result =
                    users |> tryFindUser errorSource userId
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        let userEvent = PasswordReset (userId, salt, hash password salt)
                        tryApplyUserEvent errorSource userId (Some user) (incrementRvn currentRvn) userEvent)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> thingAsync (Error error)
                result |> Result.map (fun _ -> userId) |> tupleError userId |> reply.Reply
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
                result |> Result.map (fun _ -> userId) |> tupleError userId |> reply.Reply
                let users = match result with | Ok (user, _, _) -> users |> updateUser auditUserId user | Error _ -> users
                return! running users }
        running (new Dictionary<UserId, User>()))
    member __.OnUsersEventsRead usersEvents = OnUsersEventsRead usersEvents |> agent.Post
    member __.HandleSignInCmdAsync (sessionId, userName, password) = (fun reply -> HandleSignInCmd (sessionId, userName, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleCreateUserCmdAsync (token, auditUserId, userId, userName, password, userType) = (fun reply -> HandleCreateUserCmd (token, auditUserId, userId, userName, password, userType, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangePasswordCmdAsync (token, auditUserId, currentRvn, password) = (fun reply -> HandleChangePasswordCmd (token, auditUserId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleResetPasswordCmdAsync (token, auditUserId, userId, currentRvn, password) = (fun reply -> HandleResetPasswordCmd (token, auditUserId, userId, currentRvn, password, reply)) |> agent.PostAndAsyncReply
    member __.HandleChangeUserTypeCmdAsync (token, auditUserId, userId, currentRvn, userType) = (fun reply -> HandleChangeUserTypeCmd (token, auditUserId, userId, currentRvn, userType, reply)) |> agent.PostAndAsyncReply

let users = Users ()

broadcaster.Subscribe (fun event -> // note: not interested in UserEventWritten (since Users agent causes these in the first place - and will already have updated its state accordingly)
    match event with
    | UsersEventsRead usersEvents -> users.OnUsersEventsRead usersEvents
    | _ -> ()) |> ignore

//#if DEBUG
let changePasswordToken userId = ChangePasswordToken userId
let createUserAnyToken = CreateUserToken [ SuperUser ; Administrator ; Pleb ; PersonaNotGrata ]
let resetPasswordToken = ResetPasswordToken
let changeUserTypeToken = ChangeUserTypeToken
//#endif

let ensureInstantiated () = () // note: Users agent [users] is static - so will only be instantiated when Users module (effectively a static class) is first referenced
