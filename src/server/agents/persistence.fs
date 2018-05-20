module Aornota.Sweepstake2018.Server.Agents.Persistence

// Note: Persistence agent broadcasts UsersEventsRead | SquadsEventsRead | UserEventWritten | SquadEventWritten - and subscribes to nothing.

open Aornota.Common.IfDebug
open Aornota.Common.Json
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.Server.Common.Helpers
open Aornota.Server.Common.JsonConverter

open Aornota.Sweepstake2018.Common.Domain.Squad
open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Server.Agents.Broadcaster
open Aornota.Sweepstake2018.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2018.Server.Events.SquadEvents
open Aornota.Sweepstake2018.Server.Events.UserEvents
open Aornota.Sweepstake2018.Server.Signal

open System
open System.Collections.Generic
open System.IO
open System.Text

type ReadLockId = private | ReadLockId of guid : Guid

type EntityType = // note: not private since used by #if DEBUG code elsewhere (e.g. to create default User/s events &c.)
    | Users
    | Squads

(* TODO-NMB-LOW: Implement LogWriteEventFilter to control logging for Write[EntityType]Event input/s (cf. LogEventFilter for Broadcaster)?...
type EntityTypeFilter = EntityType -> bool
type LogWriteEventFilter = string * EntityTypeFilter *)

type private PersistenceInput =
    | Start of reply : AsyncReplyChannel<unit>
    | AcquireReadLock of readLockId : ReadLockId * acquiredBy : string * disposable : IDisposable * reply : AsyncReplyChannel<ReadLockId * IDisposable>
    | ReleaseReadLock of readLockId : ReadLockId
    | ReadUsersEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadSquadsEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserEvent of auditUserId : UserId * rvn : Rvn * userEvent : UserEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteSquadEvent of auditUserId : UserId * rvn : Rvn * squadEvent : SquadEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>

// TODO-NMB-MEDIUM: Try making private with [<JsonConstructor>] attribute (open Newtonsoft.Json) - perhaps using single-case union rather than record? - and see if deserialization works?...
type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId } // note: *not* private because this breaks deserialization

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

let private log category = (Persistence, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let directory entityType = // note: not private since used by #if DEBUG code elsewhere (e.g. to create default User/s events &c.)
    let entityTypeDir = match entityType with | Users -> "users" | Squads -> "squads"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private encoding = Encoding.UTF8

let private persistenceError source errorText = ifDebugSource source errorText |> PersistenceError |> Error

let private readEvents<'a> entityType =
    let eventsExtensionWithDot = sprintf ".%s" EVENTS_EXTENSION
    let readFile (fileName:string) = // note: silently ignore non-{Guid}.EVENTS_EXTENSION files
        let fileInfo = FileInfo fileName
        match if fileInfo.Extension = eventsExtensionWithDot then fileInfo.Name.Substring (0, fileInfo.Name.Length - eventsExtensionWithDot.Length) |> Some else None with
        | Some possibleGuid ->
            match Guid.TryParse possibleGuid with
            | true, id ->
                let events =
                    File.ReadAllLines (fileInfo.FullName, encoding)
                    |> List.ofArray
                    |> List.map (fun line ->
                        let persistedEvent = Json line |> ofJson<PersistedEvent>
                        persistedEvent.Rvn, persistedEvent.EventJson |> ofJson<'a>)
                (id, events) |> Some
            | false, _ -> None
        | None -> None
    let entityTypeDir = directory entityType
    if Directory.Exists entityTypeDir then
        try
            Directory.GetFiles (entityTypeDir, sprintf "*%s" eventsExtensionWithDot)
            |> List.ofArray       
            |> List.choose readFile
            |> Ok
        with exn -> ifDebug exn.Message UNEXPECTED_ERROR |> persistenceError (sprintf "Persistence.readEvents<%s>" typeof<'a>.Name)
    else [] |> Ok

let private writeEvent source entityType (entityId:Guid) rvn eventJson auditUserId =
    let source = sprintf "%s#writeEvent" source
    let entityTypeDir = directory entityType
    let fileName = sprintf "%s/%s.%s" entityTypeDir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.Now.ToUniversalTime () ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists entityTypeDir |> not then Directory.CreateDirectory entityTypeDir |> ignore
        if File.Exists fileName then
            let lineCount = (File.ReadAllLines fileName).Length
            if validateNextRvn ((Rvn lineCount) |> Some) rvn |> not then
                ifDebug (sprintf "File %s contains %i lines (Rvns) when writing %A (%A)" fileName lineCount rvn eventJson) UNEXPECTED_ERROR |> persistenceError source
            else
                File.AppendAllLines (fileName, [ json ], encoding)
                () |> Ok
        else
            if rvn <> Rvn 1 then
                ifDebug (sprintf "No existing file %s when writing %A (%A)" fileName rvn eventJson) UNEXPECTED_ERROR |> persistenceError source
            else
                File.WriteAllLines (fileName, [ json ], encoding)
                () |> Ok
    with exn -> ifDebug exn.Message UNEXPECTED_ERROR |> persistenceError source

type Persistence () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> notLockedForReading" |> Info |> log
                () |> reply.Reply
                return! notLockedForReading ()
            | AcquireReadLock _ -> "AcquireReadLock when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReleaseReadLock _ -> "ReleaseReadLock when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadUsersEvents _ -> "ReadUsersEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadSquadsEvents _ -> "ReadSquadsEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteUserEvent _ -> "WriteUserEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteSquadEvent _ -> "WriteSquadEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and notLockedForReading () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when notLockedForReading" |> IgnoredInput |> Agent |> log ; return! notLockedForReading ()
            | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                sprintf "AcquireReadLock %A for '%s' when notLockedForReading -> lockedForReading (1 lock)" readLockId acquiredBy |> Info |> log
                (readLockId, disposable) |> reply.Reply
                let readLocks = new Dictionary<ReadLockId, string> ()
                (readLockId, acquiredBy) |> readLocks.Add
                return! lockedForReading readLocks
            | ReleaseReadLock _ -> "ReleaseReadLock when notLockedForReading" |> formatIgnoredInput |> Danger |> log ; return! notLockedForReading ()
            | ReadUsersEvents (readLockId, reply) ->
                let errorText = sprintf "ReadUsersEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | ReadSquadsEvents (readLockId, reply) ->
                let errorText = sprintf "ReadSquadsEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteUserEvent (auditUserId, rvn, userEvent, reply) ->
                let source = "WriteUserEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn userEvent |> Verbose |> log
                let (UserId userId) = userEvent.UserId
                let result =
                    match writeEvent source Users userId rvn (toJson userEvent) auditUserId with
                    | Ok _ ->
                        (rvn, userEvent) |> UserEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn userEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | WriteSquadEvent (auditUserId, rvn, squadEvent, reply) ->
                let source = "WriteSquadEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn squadEvent |> Verbose |> log
                let (SquadId squadId) = squadEvent.SquadId
                let result =
                    match writeEvent source Squads squadId rvn (toJson squadEvent) auditUserId with
                    | Ok _ ->
                        (rvn, squadEvent) |> SquadEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn squadEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading () }
        and lockedForReading readLocks = async {
            // Note: Scan (rather than Receive) in order to leave "skipped" inputs (e.g. WriteUserEvent) on the queue - though also ignore-but-consume some inputs (e.g. Start).
            return! inbox.Scan (fun input ->
                match input with
                | Start _ -> sprintf "Start when lockedForReading (%i lock/s)" readLocks.Count |> IgnoredInput |> Agent |> log ; lockedForReading readLocks |> Some
                | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                    let source = "AcquireReadLock"
                    if readLockId |> readLocks.ContainsKey |> not then
                        let previousCount = readLocks.Count
                        (readLockId, acquiredBy) |> readLocks.Add
                        sprintf "%s %A for '%s' when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" source readLockId acquiredBy previousCount readLocks.Count |> Info |> log
                    else // note: should never happen
                        sprintf "%s for '%s' when lockedForReading (%i lock/s) -> %A is not valid (already in use)" source acquiredBy readLocks.Count readLockId |> Danger |> log
                    (readLockId, disposable) |> reply.Reply
                    lockedForReading readLocks |> Some
                | ReleaseReadLock readLockId ->
                    let source = "ReleaseReadLock"
                    if readLockId |> readLocks.ContainsKey then
                        let previousCount = readLocks.Count
                        readLockId |> readLocks.Remove |> ignore
                        if readLocks.Count = 0 then
                            sprintf "%s %A when lockedForReading (%i lock/s) -> 0 locks -> notLockedForReading" source readLockId previousCount |> Info |> log
                            notLockedForReading () |> Some
                        else
                            sprintf "%s %A when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" source readLockId previousCount readLocks.Count |> Info |> log
                            lockedForReading readLocks |> Some
                    else // note: should never happen
                        sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLocks.Count readLockId |> Danger |> log
                        lockedForReading readLocks |> Some
                | ReadUsersEvents (readLockId, reply) ->
                    let source = "ReadUsersEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLocks.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLocks.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLocks.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Users with
                            | Ok usersEvents ->
                                (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> UsersEventsRead |> broadcaster.Broadcast
                                usersEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun usersEvents ->
                        let eventsCount = usersEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i UserEvent/s for %i UserId/s" eventsCount usersEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLocks |> Some
                | ReadSquadsEvents (readLockId, reply) ->
                    let source = "ReadSquadsEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLocks.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLocks.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLocks.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Squads with
                            | Ok squadsEvents ->
                                (squadsEvents |> List.map (fun (id, squadsEvents) -> SquadId id, squadsEvents)) |> SquadsEventsRead |> broadcaster.Broadcast
                                squadsEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun squadsEvents ->
                        let eventsCount = squadsEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i SquadEvent/s for %i SquadId/s" eventsCount squadsEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLocks |> Some
                | WriteUserEvent _ -> sprintf "WriteUserEvent when lockedForReading (%i lock/s)" readLocks.Count |> SkippedInput |> Agent |> log ; None
                | WriteSquadEvent _ -> sprintf "WriteSquadEvent when lockedForReading (%i lock/s)" readLocks.Count |> SkippedInput |> Agent |> log ; None) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Persistence |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    // TODO-NMB-MEDIUM: Subscribe to Tick [in Start] - then periodically "auto-backup" everything in PERSISTENCE_ROOT?...
    member __.Start () = Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AcquireReadLockAsync acquiredBy = (fun reply ->
        let readLockId = Guid.NewGuid () |> ReadLockId
        (readLockId, acquiredBy, { new IDisposable with member __.Dispose () = ReleaseReadLock readLockId |> agent.Post }, reply) |> AcquireReadLock) |> agent.PostAndAsyncReply
    member __.ReadUsersEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadUsersEvents) |> agent.PostAndAsyncReply
    member __.ReadSquadsEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadSquadsEvents) |> agent.PostAndAsyncReply
    member __.WriteUserEventAsync (auditUserId, rvn, userEvent) = (fun reply -> (auditUserId, rvn, userEvent, reply) |> WriteUserEvent) |> agent.PostAndAsyncReply
    member __.WriteSquadEventAsync (auditUserId, rvn, squadEvent) = (fun reply -> (auditUserId, rvn, squadEvent, reply) |> WriteSquadEvent) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    let acquiredBy = "readPersistedEvents"
    let (readLockId, readLock) = acquiredBy |> persistence.AcquireReadLockAsync |> Async.RunSynchronously
    use _disposable = readLock
    (* TEMP-NMB: Try calling WriteUserEventAsync in read lock (should be "skipped" but processed later)...
    "calling WriteUserEventAsync in read lock" |> Info |> log
    let userEvent = (UserId (Guid "ffffffff-ffff-ffff-ffff-ffffffffffff"), UserName "skippy", Salt "salt", Hash "hash", Pleb) |> UserCreated
    // Note: Need to call via Async.Start since WriteUserEventAsync will block (since input will be "skipped" when in read lock) until _disposable disposed (which will release the read lock).
    async {
        let! _ = (UserId Guid.Empty, Rvn 1, userEvent) |> persistence.WriteUserEventAsync
        return () } |> Async.Start *)
    readLockId |> persistence.ReadUsersEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadSquadsEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
