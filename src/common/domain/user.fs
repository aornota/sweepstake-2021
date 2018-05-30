module Aornota.Sweepstake2018.Common.Domain.User

open Aornota.Common.Revision

open System

type UserId = | UserId of guid : Guid with
    static member Create () = Guid.NewGuid () |> UserId

type UserName = | UserName of userName : string
type Password = | Password of password : string

type UserType = | SuperUser | Administrator | Pleb | PersonaNonGrata

type UserTarget = | NotSelf of userTypes : UserType list

type UserAdminPermissions = {
    CreateUserPermission : UserType list
    ResetPasswordPermission : UserTarget option
    ChangeUserTypePermission : (UserTarget * UserType list) option }

type SquadPermissions = {
    SquadProjectionAuthQryPermission : bool
    CreateSquadPermission : bool
    AddOrEditPlayerPermission : bool
    WithdrawPlayerPermission : bool
    EliminateSquadPermission : bool }

type ChatPermissions = { SendChatMessagePermission : bool }

type Permissions = {
    ChangePasswordPermission : UserId option
    UserAdminPermissions : UserAdminPermissions option
    SquadPermissions : SquadPermissions option
    ChatPermissions : ChatPermissions option }

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordReset

type Jwt = | Jwt of jwt : string

type AuthUser = {
    UserId : UserId
    Rvn : Rvn
    UserName : UserName
    UserType : UserType
    Permissions : Permissions
    MustChangePasswordReason : MustChangePasswordReason option
    Jwt : Jwt }

let permissions userId userType =
    let changePasswordPermission = match userType with | SuperUser | Administrator | Pleb -> userId |> Some | PersonaNonGrata -> None
    let createUserPermission, resetPasswordPermission, changeUserTypePermission =
        match userType with
        | SuperUser ->
            let createUserPermission = [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]
            let resetPasswordPermission = NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ] |> Some
            let changeUserTypePermission = (NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ], [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]) |> Some
            createUserPermission, resetPasswordPermission, changeUserTypePermission
        | Administrator -> [ Pleb ], NotSelf [ Pleb ] |> Some, None
        | Pleb | PersonaNonGrata -> [], None, None
    let userAdminPermissions =
        match createUserPermission, resetPasswordPermission, changeUserTypePermission with
        | [], None, None -> None
        | _ -> { CreateUserPermission = createUserPermission ; ResetPasswordPermission = resetPasswordPermission ; ChangeUserTypePermission = changeUserTypePermission } |> Some
    let squadProjectionAuthQryPermission, createSquadPermission, addOrEditPlayerPermission, withdrawPlayerPermission, eliminateSquadPermission =
        match userType with
        | SuperUser -> true, true, true, true, true
        | Administrator -> true, false, true, true, true
        | Pleb -> true, false, false, false, false
        | PersonaNonGrata -> false, false, false, false, false
    let squadPermissions =
        match squadProjectionAuthQryPermission, createSquadPermission, addOrEditPlayerPermission, withdrawPlayerPermission, eliminateSquadPermission with
        | false, false, false, false, false -> None
        | _ -> { SquadProjectionAuthQryPermission = squadProjectionAuthQryPermission ; CreateSquadPermission = createSquadPermission ; AddOrEditPlayerPermission = addOrEditPlayerPermission
                 WithdrawPlayerPermission = withdrawPlayerPermission ; EliminateSquadPermission = eliminateSquadPermission } |> Some
    let chatPermissions = match userType with | SuperUser | Administrator | Pleb -> { SendChatMessagePermission = true } |> Some | PersonaNonGrata -> None
    {
        ChangePasswordPermission = changePasswordPermission
        UserAdminPermissions = userAdminPermissions
        SquadPermissions = squadPermissions
        ChatPermissions = chatPermissions
    }

let validateUserName (userNames:UserName list) (UserName userName) =
    if String.IsNullOrWhiteSpace userName then "User name must not be blank" |> Some
    else if (userName.Trim ()).Length < 3 then "User name must be at least 3 characters" |> Some
    else if userNames |> List.map (fun (UserName userName) -> (userName.ToLower ()).Trim ()) |> List.contains ((userName.ToLower ()).Trim ()) then "User name already in use" |> Some
    else None
let validatePassword (Password password) =
    if String.IsNullOrWhiteSpace password then "Password must not be blank" |> Some
    else if (password.Trim ()).Length < 6 then "Password must be at least 6 characters" |> Some
    else None
let validateConfirmPassword (Password newPassword) (Password confirmPassword) =
    if newPassword <> confirmPassword then "Confirmation password must match new password" |> Some
    else validatePassword (Password confirmPassword)
