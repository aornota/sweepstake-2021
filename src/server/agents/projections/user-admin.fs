module Aornota.Sweepstake2018.Server.Agents.Projections.UserAdmin

(* Broadcasts: SendMsg
   Subscribes: UsersRead
               UserEventWritten (UserCreated | PasswordChanged | PasswordReset | UserTypeChanged)
               UserSignedIn | UserActivity | UserSignedOut
               ConnectionsSignedOut | Disconnected *)

open Aornota.Common.Revision
open Aornota.Common.UnitsOfMeasure

open Aornota.Server.Common.DeltaHelper

open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.Domain.UserAdmin
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Authorization
open Aornota.Sweepstake2018.Server.Connection
open Aornota.Sweepstake2018.Server.Events.UserEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic

type private UserAdminInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnUsersRead of usersRead : UserRead list
    | OnUserCreated of userId : UserId * rvn : Rvn * userName : UserName * userType : UserType
    | OnUserTypeChanged of userId : UserId * rvn : Rvn * userType : UserType
    | OnOtherUserEvent of userId : UserId * rvn : Rvn
    | OnUserSignedInOrOut of userId : UserId * signedIn : bool
    | OnUserActivity of userId : UserId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeUserAdminProjectionQry of token : UserAdminProjectionQryToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<UserAdminProjectionDto, AuthQryError<string>>>

type private User4Admin = { Rvn : Rvn ; UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }
type private User4AdminDic = Dictionary<UserId, User4Admin>

type private Projectee = { LastRvn : Rvn }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { User4AdminDic : User4AdminDic }

type private StateChangeType =
    | Initialization of user4AdminDic : User4AdminDic
    | User4AdminChange of user4AdminDic : User4AdminDic * state : State
  
type private User4AdminDtoDic = Dictionary<UserId, User4AdminDto>

let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>

let private log category = (Projection UserAdmin, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private user4AdminDto (userId, user4Admin:User4Admin) =
    { UserId = userId ; Rvn = user4Admin.Rvn ; UserName = user4Admin.UserName ; UserType = user4Admin.UserType ; LastActivity = user4Admin.LastActivity }

let private user4AdminDtoDic (user4AdminDic:User4AdminDic) =
    let user4AdminDtoDic = User4AdminDtoDic ()
    user4AdminDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, user4Admin)) -> 
        let user4AdminDto = (userId, user4Admin) |> user4AdminDto
        (user4AdminDto.UserId, user4AdminDto) |> user4AdminDtoDic.Add)
    user4AdminDtoDic

let private userAdminProjectionDto state = { User4AdminDtos = state.User4AdminDic |> List.ofSeq |> List.map (fun (KeyValue (userId, user4Admin)) -> (userId, user4Admin) |> user4AdminDto) }

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendUser4AdminDtoDelta (projecteeDic:ProjecteeDic) user4AdminDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendUser4AdminDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, user4AdminDtoDelta) |> Users4AdminDeltaMsg |> UserAdminProjectionMsg |> ServerUserAdminMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization user4AdminDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { User4AdminDic = User4AdminDic user4AdminDic }
        | User4AdminChange (user4AdminDic, state) ->
            let previousUser4AdminDtoDic = state.User4AdminDic |> user4AdminDtoDic
            let user4AdminDtoDic = user4AdminDic |> user4AdminDtoDic
            let user4AdminDtoDelta = user4AdminDtoDic |> delta previousUser4AdminDtoDic
            if user4AdminDtoDelta |> isEmpty |> not then
                sprintf "%s -> User4AdminDto delta %A -> %i projectee/s" source user4AdminDtoDelta projecteeDic.Count |> Info |> log
                user4AdminDtoDelta |> sendUser4AdminDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with User4AdminDic = User4AdminDic user4AdminDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type UserAdmin () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersRead (0 chat users) (0 chat messages) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersRead ()
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnOtherUserEvent _ -> "OnOtherUserEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserActivity _ -> "OnUserActivity when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeUserAdminProjectionQry _ -> "HandleInitializeUserAdminProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUsersRead usersRead ->
                let source = "OnUsersRead"
                sprintf "%s (%i user/s) when pendingOnUsersRead" source usersRead.Length |> Info |> log
                let user4AdminDic = User4AdminDic ()
                usersRead |> List.iter (fun userRead -> (userRead.UserId, { Rvn = userRead.Rvn ; UserName = userRead.UserName ; UserType = userRead.UserType ; LastActivity = None }) |> user4AdminDic.Add)           
                let projecteeDic = ProjecteeDic ()
                let state = user4AdminDic |> Initialization |> updateState source projecteeDic
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUserCreated _ -> "OnUserCreated when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnOtherUserEvent _ -> "OnOtherUserEvent when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserActivity _ -> "OnUserActivity when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleInitializeUserAdminProjectionQry _ -> "HandleInitializeUserAdminProjectionQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead () }
        and projectingUserAdmins state user4AdminDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingUserAdmins" |> IgnoredInput |> Agent |> log ; return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUsersRead _ -> "OnUsersRead when projectingUserAdmins" |> IgnoredInput |> Agent |> log ; return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUserCreated (userId, rvn, userName, userType) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A %A) when projectingUserAdmins (%i user/s) (%i projectee/s)" source userId userName userType user4AdminDic.Count projecteeDic.Count |> Info |> log
                if userId |> user4AdminDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, { UserName = userName ; Rvn = rvn ; UserType = userType ; LastActivity = None }) |> user4AdminDic.Add
                sprintf "%s when projectingUserAdmins -> %i user/s)" source user4AdminDic.Count |> Info |> log
                let state = (user4AdminDic, state) |> User4AdminChange |> updateState source projecteeDic
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUserTypeChanged (userId, rvn, userType) ->
                let source = "OnUserTypeChanged"
                sprintf "%s (%A %A) when projectingUserAdmins (%i user/s) (%i projectee/s)" source userId userType user4AdminDic.Count projecteeDic.Count |> Info |> log
                if userId |> user4AdminDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user4Admin = user4AdminDic.[userId]
                    user4AdminDic.[userId] <- { user4Admin with Rvn = rvn ; UserType = userType }
                let state = (user4AdminDic, state) |> User4AdminChange |> updateState source projecteeDic
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnOtherUserEvent (userId, rvn) ->
                let source = "OnOtherUserEvent"
                sprintf "%s (%A) when projectingUserAdmins (%i user/s) (%i projectee/s)" source userId user4AdminDic.Count projecteeDic.Count |> Info |> log
                if userId |> user4AdminDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user4Admin = user4AdminDic.[userId]
                    user4AdminDic.[userId] <- { user4Admin with Rvn = rvn }
                let state = (user4AdminDic, state) |> User4AdminChange |> updateState source projecteeDic
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUserSignedInOrOut (userId, signedIn) ->
                let source = "OnUserSignedInOrOut"
                sprintf "%s (%A %b) when projectingUserAdmins (%i user/s) (%i projectee/s)" source userId signedIn user4AdminDic.Count projecteeDic.Count |> Info |> log
                if userId |> user4AdminDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user4Admin = user4AdminDic.[userId]
                    user4AdminDic.[userId] <- { user4Admin with LastActivity = match signedIn with | true -> DateTimeOffset.UtcNow |> Some | false -> None }
                let state = (user4AdminDic, state) |> User4AdminChange |> updateState source projecteeDic
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | OnUserActivity userId ->
                let source = "OnUserActivity"
                sprintf "%s (%A) when projectingUserAdmins (%i user/s) (%i projectee/s)" source userId user4AdminDic.Count projecteeDic.Count |> Info |> log
                let updated =
                    if userId |> user4AdminDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                        let user4Admin = user4AdminDic.[userId]
                        let now = DateTimeOffset.UtcNow
                        let throttled = match user4Admin.LastActivity with | Some lastActivity -> (now - lastActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE | None -> false
                        if throttled |> not then user4AdminDic.[userId] <- { user4Admin with LastActivity = now |> Some }
                        else sprintf "%s throttled for %A" source userId |> Info |> log
                        throttled |> not
                    else false
                let state = if updated then (user4AdminDic, state) |> User4AdminChange |> updateState source projecteeDic else state
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingUserAdmins (%i user/s) (%i projectee/s)" source connectionIds user4AdminDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds                
                sprintf "%s when projectingUserAdmins -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingUserAdmins state user4AdminDic projecteeDic
            | HandleInitializeUserAdminProjectionQry (_, connectionId, reply) ->
                let source = "HandleInitializeUserAdminProjectionQry"
                sprintf "%s for %A when projectingUserAdmins (%i user/s) (%i projectee/s)" source connectionId user4AdminDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add
                else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingUserAdmins -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> userAdminProjectionDto |> Ok
                result |> logResult source (fun userAdminProjectionDto -> sprintf "%i user/s" userAdminProjectionDto.User4AdminDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)                   
                result |> reply.Reply
                return! projectingUserAdmins state user4AdminDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.UserAdmin |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | UserEventWritten (rvn, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, userType) -> (userId, rvn, userName, userType) |> OnUserCreated |> agent.Post
                | PasswordChanged (userId, _, _) -> (userId, rvn) |> OnOtherUserEvent |> agent.Post
                | PasswordReset (userId, _, _) -> (userId, rvn) |> OnOtherUserEvent |> agent.Post
                | UserTypeChanged (userId, userType) -> (userId, rvn, userType) |> OnUserTypeChanged |> agent.Post
            | UserSignedIn userId -> (userId, true) |> OnUserSignedInOrOut |> agent.Post
            | UserSignedOut userId -> (userId, false) |> OnUserSignedInOrOut |> agent.Post
            | UserActivity userId -> userId |> OnUserActivity |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten (subset) | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeUserAdminProjectionQry (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleInitializeUserAdminProjectionQry) |> agent.PostAndAsyncReply

let userAdmin = UserAdmin ()
