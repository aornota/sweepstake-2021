module Aornota.Sweepstake2018.Server.Authorization

open Aornota.Sweepstake2018.Common.Domain.Core

type MetaToken = private | MetaToken

type ChangePasswordToken private (userId) =
    new (_:MetaToken, userId:UserId) = ChangePasswordToken userId
    member __.UserId = userId

type UserAdministrationToken private () =
    new (_:MetaToken) = UserAdministrationToken ()

type CreateUserToken private (userTypes) =
    new (_:MetaToken, userTypes:UserType list) = CreateUserToken userTypes
    member __.UserTypes = userTypes

type ResetPasswordToken private (userTarget) =
    new (_:MetaToken, userTarget:UserTarget) = ResetPasswordToken userTarget
    member __.UserTarget = userTarget

type ChangeUserTypeToken private (userTarget, userTypes) =
    new (_:MetaToken, userTarget:UserTarget, userTypes:UserType list) = ChangeUserTypeToken (userTarget, userTypes)
    member __.UserTarget = userTarget
    member __.UserTypes = userTypes

type private ValidatedUserTokens = {
    ChangePasswordToken : ChangePasswordToken option
    UserAdministrationToken : UserAdministrationToken option
    CreateUserToken : CreateUserToken option
    ResetPasswordToken : ResetPasswordToken option
    ChangeUserTypeToken : ChangeUserTypeToken option }

type UserTokens private (vut:ValidatedUserTokens) =
    new (permissions:Permissions) =
        let changePasswordToken = match permissions.ChangePasswordPermission with | Some userId -> (MetaToken, userId) |> ChangePasswordToken |> Some | None -> None
        let userAdministrationToken, createUserToken, resetPasswordToken, changeUserTypeToken =
            match permissions.UserAdministrationPermissions with
            | Some userAdministrationPermissions ->
                let userAdministrationToken = MetaToken |> UserAdministrationToken |> Some
                let createUserToken = (MetaToken, userAdministrationPermissions.CreateUserPermission) |> CreateUserToken |> Some
                let resetPasswordToken =
                    match userAdministrationPermissions.ResetPasswordPermission with
                    | Some userTarget -> (MetaToken, userTarget) |> ResetPasswordToken |> Some
                    | None -> None
                let changeUserTypeToken =
                    match userAdministrationPermissions.ChangeUserTypePermission with
                    | Some (userTarget, userTypes) -> (MetaToken, userTarget, userTypes) |> ChangeUserTypeToken |> Some
                    | None -> None
                userAdministrationToken, createUserToken, resetPasswordToken, changeUserTypeToken
            | None -> None, None, None, None           
        UserTokens {
            ChangePasswordToken = changePasswordToken
            UserAdministrationToken = userAdministrationToken
            CreateUserToken = createUserToken
            ResetPasswordToken = resetPasswordToken
            ChangeUserTypeToken = changeUserTypeToken }
    member __.ChangePasswordToken = vut.ChangePasswordToken
    member __.CreateUserToken = vut.CreateUserToken
    member __.ResetPasswordToken = vut.ResetPasswordToken
    member __.ChangeUserTypeToken = vut.ChangeUserTypeToken
