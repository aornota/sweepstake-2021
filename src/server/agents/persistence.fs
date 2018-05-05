module Aornota.Sweepstake2018.Server.Agents.Persistence

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers
open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Core
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.Event
open Aornota.Sweepstake2018.Server.Events.User

open System
open System.Collections.Generic
open System.IO
open System.Text

type ReadLockId = private | ReadLockId of guid : Guid

type EntityType = // note: not private since used by #if DEBUG code elsewhere (e.g. to create default User/s events)
    | Users

(* TODO-NMB-LOW: Implement LogWriteEventFilter to control logging for Write[EntityType]Event input/s (cf. LogEventFilter for Broadcaster)?...
type EntityTypeFilter = EntityType -> bool
type LogWriteEventFilter = string * EntityTypeFilter *)

// TODO-NMB-MEDIUM: Try making private with [<JsonConstructor>] attribute (open Newtonsoft.Json) - perhaps using single-case union rather than record? - and see if deserialization works?...
type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId } // note: *not* private because this breaks deserialization

type private PersistenceInput =
    | Start of reply : AsyncReplyChannel<unit>
    | AcquireReadLock of readLockId : ReadLockId * acquiredBy : string * disposable : IDisposable * reply : AsyncReplyChannel<ReadLockId * IDisposable>
    | ReleaseReadLock of readLockId : ReadLockId
    | ReadUsersEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserEvent of auditUserId : UserId * rvn : Rvn * userEvent : UserEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

let private log category = consoleLogger.Log (Persistence, category)

let private logResult resultSource successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        log (Info (sprintf "%s Ok%s" resultSource successText))
    | Error error -> log (Danger (sprintf "%s Error -> %A" resultSource error))

let directory entityType = // note: not private since used by #if DEBUG code elsewhere (e.g. to create default User/s events)
    let entityTypeDir = match entityType with | Users -> "users"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private encoding = Encoding.UTF8

let private persistenceError debugSource errorText = Error (PersistenceError (ifDebugSource debugSource errorText))

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
        with exn -> persistenceError (sprintf "Persistence.readEvents<%s>" typeof<'a>.Name) (ifDebug exn.Message UNEXPECTED_ERROR)
    else Ok []

let private writeEvent entityType (entityId:Guid) rvn eventJson auditUserId =
    let debugSource = "Persistence.writeEvent"
    let entityTypeDir = directory entityType
    let fileName = sprintf "%s/%s.%s" entityTypeDir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.Now.ToUniversalTime () ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists entityTypeDir |> not then Directory.CreateDirectory entityTypeDir |> ignore
        if File.Exists fileName then
            let lineCount = (File.ReadAllLines fileName).Length
            if validateNextRvn (Some (Rvn lineCount)) rvn |> not then
                persistenceError debugSource (ifDebug (sprintf "File %s contains %i lines (Rvns) when writing %A (%A)" fileName lineCount rvn eventJson) UNEXPECTED_ERROR)
            else
                File.AppendAllLines (fileName, [ json ], encoding)
                Ok ()
        else
            if rvn <> Rvn 1 then
                persistenceError debugSource (ifDebug (sprintf "No existing file %s when writing %A (%A)" fileName rvn eventJson) UNEXPECTED_ERROR)
            else
                File.WriteAllLines (fileName, [ json ], encoding)
                Ok ()
    with exn -> persistenceError debugSource (ifDebug exn.Message UNEXPECTED_ERROR)

type Persistence () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                log (Info "Start when awaitingStart -> notLockedForReading")
                () |> reply.Reply
                return! notLockedForReading ()
            | AcquireReadLock _ -> log (Agent (IgnoredInput "AcquireReadLock when awaitingStart")) ; return! awaitingStart ()
            | ReleaseReadLock _ -> log (Agent (IgnoredInput "ReleaseReadLock when awaitingStart")) ; return! awaitingStart ()
            | ReadUsersEvents _ -> log (Agent (IgnoredInput "ReadUsersEvents when awaitingStart")) ; return! awaitingStart ()
            | WriteUserEvent _ -> log (Agent (IgnoredInput "WriteUserEvent when awaitingStart")) ; return! awaitingStart () }
        and notLockedForReading () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> log (Agent (IgnoredInput "Start when notLockedForReading")) ; return! notLockedForReading ()
            | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                log (Info (sprintf "AcquireReadLock %A for '%s' when notLockedForReading -> lockedForReading (1 lock)" readLockId acquiredBy))
                (readLockId, disposable) |> reply.Reply
                let readLocks = new Dictionary<ReadLockId, string> ()
                readLocks.Add (readLockId, acquiredBy)
                return! lockedForReading readLocks
            | ReleaseReadLock _ -> log (Danger (formatIgnoredInput "ReleaseReadLock when notLockedForReading")) ; return! notLockedForReading ()
            | ReadUsersEvents (readLockId, reply) ->
                let errorText = sprintf "ReadUsersEvents (%A) when notLockedForReading" readLockId
                log (Danger (formatIgnoredInput errorText))
                Error (PersistenceError errorText) |> reply.Reply
                return! notLockedForReading ()
            | WriteUserEvent (auditUserId, rvn, userEvent, reply) ->
                log (Verbose (sprintf "WriteUserEvent when notLockedForReading -> Audit%A %A %A" auditUserId rvn userEvent))
                let (UserId userId) = userEvent.UserId
                let result =
                    match writeEvent Users userId rvn (toJson userEvent) auditUserId with
                    | Ok _ ->
                        UserEventWritten (rvn, userEvent) |> broadcaster.Broadcast
                        Ok ()
                    | Error error -> Error error
                logResult "WriteUserEvent" (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn userEvent)) result // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading () }
        and lockedForReading readLocks = async {
            // Note: Scan (rather than Receive) in order to leave "skipped" inputs (e.g. WriteUserEvent) on the queue - though also ignore-but-consume some inputs (e.g. Start).
            return! inbox.Scan (fun input ->
                match input with
                | Start _ -> log (Agent (IgnoredInput (sprintf "Start when lockedForReading (%i lock/s)" readLocks.Count))) ; Some (lockedForReading readLocks)
                | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                    if readLocks.ContainsKey readLockId |> not then
                        let previousCount = readLocks.Count
                        readLocks.Add (readLockId, acquiredBy)
                        log (Info (sprintf "AcquireReadLock %A for '%s' when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" readLockId acquiredBy previousCount readLocks.Count))
                    else // note: should never happen
                        log (Danger (sprintf "AcquireReadLock for '%s' when lockedForReading (%i lock/s) -> %A is not valid (already in use)" acquiredBy readLocks.Count readLockId))
                    (readLockId, disposable) |> reply.Reply
                    Some (lockedForReading readLocks)
                | ReleaseReadLock readLockId ->
                    if readLocks.ContainsKey readLockId then
                        let previousCount = readLocks.Count
                        readLocks.Remove readLockId |> ignore
                        if readLocks.Count = 0 then
                            log (Info (sprintf "ReleaseReadLock %A when lockedForReading (%i lock/s) -> 0 locks -> notLockedForReading" readLockId previousCount))
                            Some (notLockedForReading ())
                        else
                            log (Info (sprintf "ReleaseReadLock %A when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" readLockId previousCount readLocks.Count))
                            Some (lockedForReading readLocks)
                    else // note: should never happen
                        log (Danger (sprintf "ReleaseReadLock when lockedForReading (%i lock/s) -> %A is not valid (not in use)" readLocks.Count readLockId))
                        Some (lockedForReading readLocks)
                | ReadUsersEvents (readLockId, reply) ->
                    log (Verbose (sprintf "ReadUsersEvents (%A) when lockedForReading (%i lock/s)" readLockId readLocks.Count))
                    let result =
                        if readLocks.ContainsKey readLockId |> not then // note: should never happen
                            let errorText = sprintf "ReadUsersEvents when lockedForReading (%i lock/s) -> %A is not valid (not in use)" readLocks.Count readLockId
                            Error (PersistenceError (ifDebug errorText UNEXPECTED_ERROR))
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Users with
                            | Ok usersEvents ->
                                UsersEventsRead (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> broadcaster.Broadcast
                                Ok usersEvents
                            | Error error -> Error error)
                    let successText = (fun usersEvents ->
                        let eventsCount = usersEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        Some (sprintf "%i UserEvent/s for %i UserId/s" eventsCount usersEvents.Length))
                    logResult "ReadUsersEvents" successText result // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    Some (lockedForReading readLocks)
                | WriteUserEvent _ -> log (Agent (SkippedInput (sprintf "WriteUserEvent when lockedForReading (%i lock/s)" readLocks.Count))) ; None) }
        log (Info "agent instantiated -> awaitingStart")
        awaitingStart ())
    do agent.Error.Add (logAgentException Source.Persistence) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    // TODO-NMB-MEDIUM: Subscribe to Tick [in Start] - then periodically "auto-backup" everything in PERSISTENCE_ROOT?...
    member __.Start () = Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AcquireReadLockAsync acquiredBy = (fun reply ->
        let readLockId = Guid.NewGuid () |> ReadLockId
        AcquireReadLock (readLockId, acquiredBy, { new IDisposable with member __.Dispose () = ReleaseReadLock readLockId |> agent.Post }, reply)) |> agent.PostAndAsyncReply
    member __.ReadUsersEventsAsync readLockId = (fun reply -> ReadUsersEvents (readLockId, reply)) |> agent.PostAndAsyncReply
    member __.WriteUserEventAsync (auditUserId, rvn, userEvent) = (fun reply -> WriteUserEvent (auditUserId, rvn, userEvent, reply)) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    let acquiredBy = "readPersistedEvents"
    let (readLockId, readLock) = acquiredBy |> persistence.AcquireReadLockAsync |> Async.RunSynchronously
    use _disposable = readLock
    (* TEMP-NMB: Try calling WriteUserEventAsync in read lock (should be "skipped" but processed later)...
    log (Info "calling WriteUserEventAsync in read lock")
    let userEvent = UserCreated (UserId (Guid "ffffffff-ffff-ffff-ffff-ffffffffffff"), UserName "skippy", Salt "salt", Hash "hash", Pleb)
    // Note: Need to call via Async.Start since WriteUserEventAsync will block (since input will be "skipped" when in read lock) until _disposable disposed (which will release the read lock).
    async {
        let! _ = (UserId Guid.Empty, Rvn 1, userEvent) |> persistence.WriteUserEventAsync
        return () } |> Async.Start*)
    readLockId |> persistence.ReadUsersEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
