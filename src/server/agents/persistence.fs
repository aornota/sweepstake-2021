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

type EntityType = // TODO-NMB-MEDIUM: More EntityTypes...
    | Users

type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId } // note: *not* private because this breaks serialization

type private PersistenceInput = 
    | AcquireReadLock of disposable : IDisposable * reply : AsyncReplyChannel<IDisposable>
    | ReleaseReadLock
    | ReadUsersEvents of reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserEvent of auditUserId : UserId * rvn : Rvn * userEvent : UserEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

let directory entityType =
    let entityTypeDir =
        match entityType with // TODO-NMB-MEDIUM: More EntityTypes...
        | Users -> "users"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private encoding = Encoding.UTF8

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
    let entityDir = directory entityType
    let fileName = sprintf "%s/%s.%s" entityDir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.Now.ToUniversalTime () ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists entityDir |> not then Directory.CreateDirectory entityDir |> ignore
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
                | ReadUsersEvents _ -> None
                | WriteUserEvent (auditUserId, rvn, userEvent, reply) ->
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
                | ReadUsersEvents reply ->
                    let result =
                        match readEvents<UserEvent> Users with
                        | Ok usersEvents ->
                            // TODO-NMB-LOW: Sanity-check/s, e.g. fst [Guid a.k.a. UserId] consistent with snd:snd/s [UserEvent/s.UserId]? snd:fst/s [Rvn/s] contiguous?...
                            UsersEventsRead (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> broadcaster.Broadcast
                            Ok ()
                        | Error serverError -> Error serverError
                    result |> reply.Reply
                    Some (reading ())
                | WriteUserEvent _ -> None) }
        running ())
    member __.AcquireReadLock () = (fun reply -> AcquireReadLock ({ new IDisposable with member __.Dispose () = ReleaseReadLock |> agent.Post }, reply)) |> agent.PostAndReply
    member __.ReadUsersEventsAsync () = ReadUsersEvents |> agent.PostAndAsyncReply
    member __.WriteUserEventAsync (auditUserId, rvn, userEvent) = (fun reply -> WriteUserEvent (auditUserId, rvn, userEvent, reply)) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    use _lock = persistence.AcquireReadLock ()
    persistence.ReadUsersEventsAsync () |> Async.RunSynchronously |> ignore // note: silently ignore ReadUsersEventsAsync result

// Note: No ensureInstantiated function since host.fs has explicit call to readPersistedEvents.
