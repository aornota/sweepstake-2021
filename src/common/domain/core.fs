module Aornota.Sweepstake2018.Common.Domain.Core

(*open Aornota.Common.Projection
open Aornota.Common.UnitsOfMeasure*)

open System

type ToDo = unit // TODO-NMB-HIGH: Remove once no longer used...

type Markdown = | Markdown of markdown : string

type Rvn = | Rvn of rvn : int

type SessionId = | SessionId of guid : Guid with
    static member Create () = Guid.NewGuid () |> SessionId

type UserId = | UserId of guid : Guid with
    static member Create () = Guid.NewGuid () |> UserId

type UserName = | UserName of userName : string
type Password = | Password of password : string

type UserType = | SuperUser | Administrator | Pleb | PersonaNonGrata

type UserTarget = | NotSelf of userTypes : UserType list

type UserAdministrationPermissions = {
    CreateUserPermission : UserType list
    ResetPasswordPermission : UserTarget option
    ChangeUserTypePermission : (UserTarget * UserType list) option }

type Permissions = {
    ChangePasswordPermission : UserId option
    UserAdministrationPermissions : UserAdministrationPermissions option }

type Jwt = | Jwt of jwt : string

type AuthUser = { // TODO-NMB-HIGH: Should this be a projection?...
    UserId : UserId
    UserName : UserName
    UserType : UserType
    Permissions : Permissions
    Jwt : Jwt }

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

let permissions userId userType =
    let changePasswordPermission = match userType with | SuperUser | Administrator | Pleb -> userId |> Some | PersonaNonGrata -> None
    let createUserPermission =
        match userType with
        | SuperUser -> [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]
        | Administrator -> [ Pleb ]
        | Pleb | PersonaNonGrata -> []
    let resetPasswordPermission =
        match userType with
        | SuperUser -> NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ] |> Some
        | Administrator -> NotSelf [ Pleb ] |> Some
        | Pleb | PersonaNonGrata -> None
    let changeUserTypePermission =
        match userType with
        | SuperUser -> (NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ], [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]) |> Some
        | Administrator | Pleb | PersonaNonGrata -> None
    let userAdministrationPermissions = {
        CreateUserPermission = createUserPermission
        ResetPasswordPermission = resetPasswordPermission
        ChangeUserTypePermission = changeUserTypePermission }
    { ChangePasswordPermission = changePasswordPermission ; UserAdministrationPermissions = userAdministrationPermissions |> Some }

let incrementRvn (Rvn rvn) = Rvn (rvn + 1)

let validateNextRvn (currentRvn:Rvn option) (Rvn nextRvn) =
    match currentRvn, nextRvn with
    | None, nextRvn when nextRvn = 1 -> true
    | Some (Rvn currentRvn), nextRvn when currentRvn + 1 = nextRvn -> true
    | _ -> false

let validateUserName (userNames:UserName list) (UserName userName) =
    if String.IsNullOrWhiteSpace userName then "Username must not be blank" |> Some
    else if (userName.Trim ()).Length < 4 then "Username must be at least 4 characters" |> Some
    else if userNames |> List.map (fun (UserName userName) -> userName.ToLower ()) |> List.contains (userName.ToLower ()) then "Username already in use" |> Some
    else None
let validatePassword (Password password) =
    if String.IsNullOrWhiteSpace password then "Password must not be blank" |> Some
    else if (password.Trim ()).Length < 6 then "Password must be at least 6 characters" |> Some
    else None
