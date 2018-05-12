module Aornota.Sweepstake2018.Server.Authorization

open Aornota.Sweepstake2018.Common.Domain.Core

open Newtonsoft.Json

type MetaToken = private | MetaToken

type ChangePasswordToken [<JsonConstructor>] private (userId) =
    new (_:MetaToken, userId:UserId) = ChangePasswordToken userId
    member __.UserId = userId

type CreateUserToken [<JsonConstructor>] private (userTypes) =
    new (_:MetaToken, userTypes:UserType list) = CreateUserToken userTypes
    member __.UserTypes = userTypes

type ResetPasswordToken [<JsonConstructor>] private (userTarget) =
    new (_:MetaToken, userTarget:UserTarget) = ResetPasswordToken userTarget
    member __.UserTarget = userTarget

type ChangeUserTypeToken [<JsonConstructor>] private (userTarget, userTypes) =
    new (_:MetaToken, userTarget:UserTarget, userTypes:UserType list) = ChangeUserTypeToken (userTarget, userTypes)
    member __.UserTarget = userTarget
    member __.UserTypes = userTypes

type private ValidatedUserTokens = {
    ChangePasswordToken : ChangePasswordToken option
    CreateUserToken : CreateUserToken option
    ResetPasswordToken : ResetPasswordToken option
    ChangeUserTypeToken : ChangeUserTypeToken option }

type UserTokens [<JsonConstructor>] private (validatedUserTokens:ValidatedUserTokens) =
    new (permissions:Permissions) =
        let changePasswordToken = match permissions.ChangePasswordPermission with | Some userId -> ChangePasswordToken (MetaToken, userId) |> Some | None -> None
        let createUserToken, resetPasswordToken, changeUserTypeToken =
            match permissions.UserAdministrationPermissions with
            | Some userAdministrationPermissions ->
                let createUserToken = CreateUserToken (MetaToken, userAdministrationPermissions.CreateUserPermission) |> Some
                let resetPasswordToken =
                    match userAdministrationPermissions.ResetPasswordPermission with
                    | Some userTarget -> ResetPasswordToken (MetaToken, userTarget) |> Some
                    | None -> None
                let changeUserTypeToken =
                    match userAdministrationPermissions.ChangeUserTypePermission with
                    | Some (userTarget, userTypes) -> ChangeUserTypeToken (MetaToken, userTarget, userTypes) |> Some
                    | None -> None
                createUserToken, resetPasswordToken, changeUserTypeToken
            | None -> None, None, None           
        UserTokens {
            ChangePasswordToken = changePasswordToken
            CreateUserToken = createUserToken
            ResetPasswordToken = resetPasswordToken
            ChangeUserTypeToken = changeUserTypeToken }
    member __.ChangePasswordToken = validatedUserTokens.ChangePasswordToken
    member __.CreateUserToken = validatedUserTokens.CreateUserToken
    member __.ResetPasswordToken = validatedUserTokens.ResetPasswordToken
    member __.ChangeUserTypeToken = validatedUserTokens.ChangeUserTypeToken
