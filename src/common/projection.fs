module Aornota.Common.Projection

open System
open System.Collections.Generic

type ProjectionRvn<'a> = | ProjectionRvn of rvn : int

type IItemId<'a> = abstract member ItemId : Guid

type Projection<'a> when 'a :> IItemId<'a> = {
    LastRvn : ProjectionRvn<'a>
    Items : Dictionary<Guid, 'a> }

type Delta<'a> when 'a :> IItemId<'a> = {
    DeltaRvn : ProjectionRvn<'a>
    AddedItems : 'a list
    ModifiedItems : 'a list
    RemovedItemIds : IItemId<'a> list }

type DeltaError<'a> when 'a :> IItemId<'a> =
    | MissedDelta of lastRvn : ProjectionRvn<'a> * deltaRvn : ProjectionRvn<'a>
    | AddedItemsAlreadyKnown of items : 'a list
    | ModifiedItemsUnknown of items : 'a list
    | RemovedItemIdsUnknown of itemIds : IItemId<'a> list

let private getItemId item = (item :> IItemId<'a>).ItemId

// TODO-NMB-HIGH: Extend applyDelta | makeDelta to handle "mapping", e.g. server -> Dto | Dto -> client (note that mapper could be "identity", i.e. if Dto is same type as server/client)?...

let applyDelta projection delta =
    let checkDeltaRvn lastRvn deltaRvn =
        let ProjectionRvn lastRvn', ProjectionRvn deltaRvn' = lastRvn, deltaRvn
        if deltaRvn' = lastRvn' + 1 then Ok () else Error (MissedDelta (lastRvn, deltaRvn))
    let addItems projection items =
        match items |> List.filter (getItemId >> projection.Items.ContainsKey) with
        | h :: t -> Error (AddedItemsAlreadyKnown (h :: t))
        | [] ->
            items |> List.iter (fun item -> projection.Items.Add (getItemId item, item))
            Ok projection
    let modifyItems projection items =
        match items |> List.filter (getItemId >> projection.Items.ContainsKey >> not) with
        | h :: t -> Error (ModifiedItemsUnknown (h :: t))
        | [] ->
            items |> List.iter (fun item -> projection.Items.Item (getItemId item) <- item)
            Ok projection
    let removeItemIds (projection:Projection<'a>) (itemIds:IItemId<'a> list) =
        match itemIds |> List.filter (fun itemId -> projection.Items.ContainsKey itemId.ItemId |> not) with
        | h :: t -> Error (RemovedItemIdsUnknown (h :: t))
        | [] ->
            itemIds |> List.iter (fun itemId -> projection.Items.Remove itemId.ItemId |> ignore)
            Ok projection
    checkDeltaRvn projection.LastRvn delta.DeltaRvn
    |> Result.bind (fun _ -> addItems projection delta.AddedItems)
    |> Result.bind (fun projection -> modifyItems projection delta.ModifiedItems)
    |> Result.bind (fun projection -> removeItemIds projection delta.RemovedItemIds)

// TODO-NMB-HIGH... let makeDelta previous current = ...
