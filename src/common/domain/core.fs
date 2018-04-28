module Aornota.Sweepstake2018.Common.Domain.Core

(*open Aornota.Common.Projection
open Aornota.Common.UnitsOfMeasure*)

open System

type ToDo = unit // TODO-NMB-HIGH: Remove once no longer used...

type Markdown = | Markdown of markdown : string

type Rvn = | Rvn of rvn : int

type UserId = | UserId of guid : Guid with
    static member Create () = Guid.NewGuid () |> UserId

type SessionId = | SessionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SessionId

type UserName = | UserName of userName : string
type Password = | Password of password : string

type UserType = | SuperUser | Administrator | Pleb | PersonaNotGrata // TODO-NMB-HIGH: Move below Jwt - and use more granular "permissions" for AuthUser

type AuthUser = { // TODO-NMB-HIGH: Permissions?...
    UserId : UserId
    SessionId : SessionId
    UserName : string // TODO-NMB-HIGH: Change to UserName (rather than string)...
    (*UserType : UserType*) }

type Jwt = | Jwt of jwt : AuthUser // TODO-NMB-MEDIUM: Change to string (rather than AuthUser) - once Jwt functionality implemented...

(*type SignedInStatusDto =
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
 
let validateNextRvn (currentRvn:Rvn option) (Rvn nextRvn) =
    match currentRvn, nextRvn with
    | None, nextRvn when nextRvn = 1 -> true
    | Some (Rvn currentRvn), nextRvn when currentRvn + 1 = nextRvn -> true
    | _ -> false

let incrementRvn (Rvn rvn) = Rvn (rvn + 1)

let validateUserName (UserName _userName) : string option = None // TODO-NMB-HIGH...
let validatePassword (Password _password) : string option = None // TODO-NMB-HIGH...

let thingAsync thing = async { return thing }

let tupleError thing result = match result with | Ok ok -> Ok ok | Error error -> Error (thing, error)
