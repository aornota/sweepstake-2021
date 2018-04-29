module Aornota.Sweepstake2018.Server.Agents.Persistence

open Aornota.Common.Json

open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

open System
open System.IO
open System.Text

type EntityType = | Users // note: not private since used by #if DEBUG code in host.fs

// TODO-NMB-HIGH: Try making private with [<JsonConstructor>] attribute (open Newtonsoft.Json) and see if serialization works?...
type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId } // note: *not* private because this breaks serialization

type private PersistenceInput =
    | AcquireReadLock of disposable : IDisposable * reply : AsyncReplyChannel<IDisposable>
    | ReleaseReadLock
    | ReadUsersEvents of reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserEvent of auditUserId : UserId * rvn : Rvn * userEvent : UserEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

let private log category = consoleLogger.Log (Persistence, category)

let directory entityType = // note: not private since used by #if DEBUG code in host.fs
    let entityTypeDir = match entityType with | Users -> "users"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private encoding = Encoding.UTF8

let private readEvents<'a> entityType =
    let eventsExtensionWithDot = sprintf ".%s" EVENTS_EXTENSION
    let readFile (fileName:string) = // note: silently ignore non-{Guid}.EVENTS_EXTENSION files
        let fileInfo = FileInfo fileName
        match if fileInfo.Extension = eventsExtensionWithDot then Some (fileInfo.Name.Substring (0, fileInfo.Name.Length - eventsExtensionWithDot.Length)) else None with
        | Some possibleGuid ->
            match Guid.TryParse possibleGuid with
            | true, id ->
                let events =
                    File.ReadAllLines (fileInfo.FullName, encoding)
                    |> List.ofArray
                    |> List.map (fun line ->
                        let persistedEvent = Json line |> ofJson<PersistedEvent>
                        persistedEvent.Rvn, persistedEvent.EventJson |> ofJson<'a>)
                Some (id, events)
            | false, _ -> None
        | None -> None
    let entityTypeDir = directory entityType
    if Directory.Exists entityTypeDir then
        try
            Directory.GetFiles (entityTypeDir, sprintf "*%s" eventsExtensionWithDot)
            |> List.ofArray       
            |> List.choose readFile
            |> Ok
        with exn -> Error (PersistenceError exn.Message)
    else Ok []

let private writeEvent entityType (entityId:Guid) rvn eventJson auditUserId =
    let entityTypeDir = directory entityType
    let fileName = sprintf "%s/%s.%s" entityTypeDir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.Now.ToUniversalTime () ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists entityTypeDir |> not then Directory.CreateDirectory entityTypeDir |> ignore
        if File.Exists fileName then
            File.AppendAllLines (fileName, [ json ], encoding)
            Ok ()
        else
            File.WriteAllLines (fileName, [ json ], encoding)
            Ok ()
    with exn -> Error (PersistenceError exn.Message)

type Persistence () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec notLockedForReading () = async {
            // TODO-NMB-HIGH: Switch to Receive () since do want to consume "warning" events?...
            return! inbox.Scan (fun input -> // note: not Receive (...) in order to leave "skipped" inputs on the queue (rather than consuming them)
                match input with
                | AcquireReadLock (disposable, reply) ->
                    log (Info "AcquireReadLock when notLockedForReading -> lockedForReading -> 1 lock")
                    disposable |> reply.Reply
                    Some (lockedForReading 1u)
                | ReleaseReadLock ->
                    log (Warning "ReleaseReadLock when notLockedForReading") // note: should never happen
                    None
                | ReadUsersEvents _ ->
                    log (Warning "ReadUsersEvents when notLockedForReading") // note: should never happen [unless calling code forgets to acquire read lock]
                    None
                | WriteUserEvent (auditUserId, rvn, userEvent, reply) ->
                    // TODO-NMB-HIGH: log more?...
                    log (Info (sprintf "WriteUserEvent when notLockedForReading -> %A %A %A" auditUserId rvn userEvent))
                    let (UserId userId) = userEvent.UserId
                    let result =
                        match writeEvent Users userId rvn (toJson userEvent) auditUserId with
                        | Ok _ ->
                            UserEventWritten (rvn, userEvent) |> broadcaster.Broadcast
                            Ok ()
                        | Error error -> Error error
                    result |> reply.Reply
                    Some (notLockedForReading ())) }
        and lockedForReading lockCount = async {
            return! inbox.Scan (fun input -> // note: not Receive (...) in order to leave "skipped" inputs on the queue (rather than consuming them)
                match input with
                | AcquireReadLock (disposable, reply) ->
                    let lockCount = lockCount + 1u
                    log (Info (sprintf "AcquireReadLock when lockedForReading -> lockedForReading -> %i lock/s" lockCount))
                    disposable |> reply.Reply
                    Some (lockedForReading lockCount)
                | ReleaseReadLock ->
                    if lockCount = 0u then log (Warning "ReleaseReadLock when lockedForReading -> already 0 locks") // note: should never happen
                    let lockCount = if lockCount = 0u then lockCount else lockCount - 1u
                    if lockCount = 0u then
                        log (Info "ReleaseReadLock when lockedForReading -> 0 locks -> notLockedForReading")
                        Some (notLockedForReading ())
                    else
                        log (Info (sprintf "ReleaseReadLock when lockedForReading -> lockedForReading -> %i lock/s" lockCount))
                        Some (lockedForReading lockCount)
                | ReadUsersEvents reply ->
                    // TODO-NMB-HIGH: log more?...
                    log (Info (sprintf "ReadUsersEvents when lockedForReading -> %i lock/s" lockCount))
                    let result =
                        match readEvents<UserEvent> Users with
                        | Ok usersEvents ->
                            // TODO-NMB-LOW: Sanity-check/s, e.g. fst [Guid a.k.a. UserId] consistent with snd:snd/s [UserEvent/s.UserId]? snd:fst/s [Rvn/s] contiguous?...
                            UsersEventsRead (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> broadcaster.Broadcast
                            Ok ()
                        | Error error -> Error error
                    result |> reply.Reply
                    Some (lockedForReading lockCount)
                | WriteUserEvent _ ->
                    log (SkippedInput "WriteUserEvent when lockedForReading -> skipped")
                    None) }
        log (Info "agent instantiated -> notLockedForReading")
        notLockedForReading ())
    do agent.Error.Add (logAgentExn Source.Persistence) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.AcquireReadLockAsync () = (fun reply -> AcquireReadLock ({ new IDisposable with member __.Dispose () = ReleaseReadLock |> agent.Post }, reply)) |> agent.PostAndAsyncReply
    member __.ReadUsersEventsAsync () = ReadUsersEvents |> agent.PostAndAsyncReply
    member __.WriteUserEventAsync (auditUserId, rvn, userEvent) = (fun reply -> WriteUserEvent (auditUserId, rvn, userEvent, reply)) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    log (Info "reading persisted events")
    use _readLock = persistence.AcquireReadLockAsync () |> Async.RunSynchronously
    log (Info "reading persisted User/s events")
    match persistence.ReadUsersEventsAsync () |> Async.RunSynchronously with
    | Ok _ -> log (Info "ReadUsersEventsAsync succeeded")
    | Error error -> log (Danger (sprintf "ReadUsersEventsAsync failed -> %A" error))

// Note: No ensureInstantiated function since host.fs has explicit call to readPersistedEvents.
