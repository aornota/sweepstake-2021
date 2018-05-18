module Aornota.Sweepstake2018.Common.Domain.Core

(*open Aornota.Common.Projection
open Aornota.Common.UnitsOfMeasure*)

open System

type Markdown = | Markdown of markdown : string

type Rvn = | Rvn of rvn : int

type SessionId = | SessionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SessionId

type Group = | GroupA | GroupB | GroupC | GroupD | GroupE | GroupF | GroupG | GroupH

(*type SignedInStatusDto = // TODO-NMB-HIGH: Use UTC (and/or DateTimeOffset) to avoid sinceLastApi stuff?...
    | SignedIn of sinceLastApi : float<second>
    | NotSignedIn

type UserAdminDto =
    {
        UserId : UserId
        Rvn : Rvn
        UserName : UserName
        UserType : UserType
        SignedInStatusDto : SignedInStatusDto
    }
    interface IItemId<UserAdminDto> with
        member self.ItemId =
            let (UserId id) = self.UserId
            id *)

let incrementRvn (Rvn rvn) = Rvn (rvn + 1)

let validateNextRvn (currentRvn:Rvn option) (Rvn nextRvn) =
    match currentRvn, nextRvn with
    | None, nextRvn when nextRvn = 1 -> true
    | Some (Rvn currentRvn), nextRvn when currentRvn + 1 = nextRvn -> true
    | _ -> false
