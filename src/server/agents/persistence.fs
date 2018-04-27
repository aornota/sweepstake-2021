module Aornota.Sweepstake2018.Server.Agents.Persistence

open Aornota.Common.Json

open Aornota.Server.Common.Json

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

open System
open System.IO
open System.Text

type private PersistenceInput = 
    | AcquireReadLock of disposable : IDisposable * reply : AsyncReplyChannel<IDisposable>
    | ReleaseReadLock
    | ReadUserEvents of reply : AsyncReplyChannel<Result<unit, ServerError<unit>>>
    | WriteUserEvent of rvn : Rvn * userEvent : UserEvent * auditUserId : UserId * reply : AsyncReplyChannel<Result<unit, ServerError<unit>>>

type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId }

type private EntityType = // TODO-NMB-MEDIUM: More EntityTypes...
    | Users

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

// TEMP-NMB...
let private nephId = UserId.Create ()
let private nephCreated = UserCreated (nephId, UserName "neph", Salt "TODO-NMB-HIGH: neph Salt", Hash "TODO-NMB-HIGH: neph Hash", SuperUser)
let private rosieId = UserId.Create ()
let private rosieCreated = UserCreated (rosieId, UserName "rosie", Salt "TODO-NMB-HIGH: rosie Salt", Hash "TODO-NMB-HIGH: rosie Hash", Administrator)
let private hughId = UserId.Create ()
let private hughCreated = UserCreated (hughId, UserName "hugh", Salt "TODO-NMB-HIGH: hugh Salt", Hash "TODO-NMB-HIGH: hugh Hash", Administrator)
let private willId = UserId.Create ()
let private willCreated = UserCreated (willId, UserName "will", Salt "TODO-NMB-HIGH: will Salt", Hash "TODO-NMB-HIGH: will Hash", Pleb)
// ...NMB-TEMP

let private encoding = Encoding.UTF8

let private directory entityType =
    let entityTypeDir =
        match entityType with // TODO-NMB-MEDIUM: More EntityTypes...
        | Users -> "users"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private readEvents<'a> entityType =
    let eventsExtensionWithDot = sprintf ".%s" EVENTS_EXTENSION
    let readFile (fileName:string) = // note: silently ignore non-[{Guid}.EVENTS_EXTENSION] files
        match if fileName.EndsWith eventsExtensionWithDot then Some (fileName.Substring (0, fileName.Length - eventsExtensionWithDot.Length)) else None with
        | Some fileName ->
            match Guid.TryParse fileName with
            | true, id ->
                let events =
                    File.ReadAllLines (fileName, encoding)
                    |> List.ofArray
                    |> List.map (fun line ->
                        let persistedEvent = Json line |> ofJson<PersistedEvent>
                        persistedEvent.Rvn, persistedEvent.EventJson |> ofJson<'a>)
                Some (id, events)
            | false, _ -> None
        | None -> None
    try
        Directory.GetFiles (directory entityType, eventsExtensionWithDot)
        |> List.ofArray       
        |> List.choose readFile
        |> Ok
    with exn -> Error (PersistenceError exn.Message)

let private writeEvent entityType (entityId:Guid) rvn eventJson auditUserId =
    let dir = directory entityType
    let fileName = sprintf "%s/%s.%s" dir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.Now.ToUniversalTime () ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists dir |> not then Directory.CreateDirectory dir |> ignore
        if File.Exists fileName then
            File.AppendAllLines (fileName, [ json ], encoding)
            Ok ()
        else
            File.WriteAllLines (fileName, [ json ], encoding)
            Ok ()
    with exn -> Error (PersistenceError exn.Message)

type Persistence () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec running () = async {
            return! inbox.Scan (fun input ->
                match input with
                | AcquireReadLock (disposable, reply) ->
                    disposable |> reply.Reply
                    Some (reading ())
                | ReleaseReadLock -> None
                | ReadUserEvents _ -> None
                | WriteUserEvent (rvn, userEvent, auditUserId, reply) ->
                    let (UserId userId) = userEvent.UserId
                    let result =
                        match writeEvent Users userId rvn (toJson userEvent) auditUserId with
                        | Ok _ ->
                            UserEventWritten (rvn, userEvent) |> broadcaster.Broadcast
                            Ok ()
                        | Error error -> Error error
                    result |> reply.Reply
                    Some (running ()) ) }
        and reading () = async {
            return! inbox.Scan (fun input ->
                match input with
                | AcquireReadLock _ -> None
                | ReleaseReadLock -> Some (running ())
                | ReadUserEvents reply ->
                    // TEMP-NMB: Bypass persistent storage...
                    (*UsersEventsRead [ 
                        nephId, [ Rvn 1, nephCreated ]
                        rosieId, [ Rvn 1, rosieCreated ]
                        hughId, [ Rvn 1, hughCreated ]
                        willId, [ Rvn 1, willCreated ] ] |> broadcaster.Broadcast
                    let result = Ok ()*)
                    // ...or not...
                    let result =
                        match readEvents<UserEvent> Users with
                        | Ok usersEvents ->
                            // TODO-NMB-LOW: For each usersEvents, check fst [Guid] consistent with snd [UserEvent list]?...
                            UsersEventsRead (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> broadcaster.Broadcast
                            Ok ()
                        | Error serverError -> Error serverError
                    // ...NMB-TEMP
                    result |> reply.Reply
                    Some (reading ())
                | WriteUserEvent _ -> None) }
        running ())
    member __.AcquireReadLock () = (fun reply -> AcquireReadLock ({ new IDisposable with member __.Dispose () = ReleaseReadLock |> agent.Post }, reply)) |> agent.PostAndReply
    member __.ReadUserEvents () = ReadUserEvents |> agent.PostAndAsyncReply
    member __.WriteUserEvent (rvn, userEvent, auditUserId) = (fun reply -> WriteUserEvent (rvn, userEvent, auditUserId, reply)) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    // TEMP-NMB: Write "default" users...
    let writeDefaultUsers = async {
        if Directory.Exists (directory Users) |> not then // note: silently ignore WriteUserEvent (...) result/s [Result<unit<ServerError<unit>>]
            let! _ = persistence.WriteUserEvent (Rvn 1, nephCreated, nephId)
            let! _ = persistence.WriteUserEvent (Rvn 1, rosieCreated, nephId)
            let! _ = persistence.WriteUserEvent (Rvn 1, hughCreated, nephId)
            let! _ = persistence.WriteUserEvent (Rvn 1, willCreated, nephId)
            ()
        return () }
    writeDefaultUsers |> Async.Start
    // ...NMB-TEMP
    use _lock = persistence.AcquireReadLock ()
    persistence.ReadUserEvents () |> Async.RunSynchronously |> ignore // note: silently ignore ReadUserEvents () result [Result<unit<ServerError<unit>>]
