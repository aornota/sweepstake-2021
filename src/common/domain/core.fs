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

let validateUserName (userNames:UserName list) (UserName userName) =
    if String.IsNullOrWhiteSpace userName then Some "Username must not be blank"
    else if (userName.Trim ()).Length < 4 then Some "Username must be at least 4 characters"
    else if userNames |> List.map (fun (UserName userName) -> userName.ToLower ()) |> List.contains (userName.ToLower ()) then Some "Username already in use"
    else None
let validatePassword (Password password) =
    if String.IsNullOrWhiteSpace password then Some "Password must not be blank"
    else if (password.Trim ()).Length < 6 then Some "Password must be at least 6 characters"
    // TODO-NMB-LOW?... else if password = "password" then Some "'password' is not an acceptable password!"
    else None
