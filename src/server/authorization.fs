module Aornota.Sweepstake2018.Server.Authorization

open Aornota.Sweepstake2018.Common.Domain.User

type MetaToken = private | MetaToken

type ChangePasswordToken private (userId) =
    new (_:MetaToken, userId:UserId) = ChangePasswordToken userId
    member __.UserId = userId

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

type CreateSquadToken private () =
    new (_:MetaToken) = CreateSquadToken ()
type AddOrEditPlayerToken private () =
    new (_:MetaToken) = AddOrEditPlayerToken ()
type WithdrawPlayerToken private () =
    new (_:MetaToken) = WithdrawPlayerToken ()
type EliminateSquadToken private () =
    new (_:MetaToken) = EliminateSquadToken ()
    
type ChatProjectionQryToken private () =
    new (_:MetaToken) = ChatProjectionQryToken ()   
type SendChatMessageToken private () =
    new (_:MetaToken) = SendChatMessageToken ()
    
type private ValidatedUserTokens = {
    ChangePasswordToken : ChangePasswordToken option
    CreateUserToken : CreateUserToken option
    ResetPasswordToken : ResetPasswordToken option
    ChangeUserTypeToken : ChangeUserTypeToken option
    CreateSquadToken : CreateSquadToken option
    AddOrEditPlayerToken : AddOrEditPlayerToken option
    WithdrawPlayerToken : WithdrawPlayerToken option
    EliminateSquadToken : EliminateSquadToken option
    ChatProjectionQryToken : ChatProjectionQryToken option
    SendChatMessageToken : SendChatMessageToken option }

type UserTokens private (vut:ValidatedUserTokens) =
    new (permissions:Permissions) =
        let changePasswordToken = match permissions.ChangePasswordPermission with | Some userId -> (MetaToken, userId) |> ChangePasswordToken |> Some | None -> None
        let createUserToken, resetPasswordToken, changeUserTypeToken =
            match permissions.UserAdministrationPermissions with
            | Some userAdministrationPermissions ->
                let createUserToken = (MetaToken, userAdministrationPermissions.CreateUserPermission) |> CreateUserToken |> Some
                let resetPasswordToken =
                    match userAdministrationPermissions.ResetPasswordPermission with
                    | Some userTarget -> (MetaToken, userTarget) |> ResetPasswordToken |> Some
                    | None -> None
                let changeUserTypeToken =
                    match userAdministrationPermissions.ChangeUserTypePermission with
                    | Some (userTarget, userTypes) -> (MetaToken, userTarget, userTypes) |> ChangeUserTypeToken |> Some
                    | None -> None
                createUserToken, resetPasswordToken, changeUserTypeToken
            | None -> None, None, None
        let createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken =
            match permissions.SquadAdministrationPermissions with
            | Some squadAdministrationPermissions ->
                let createSquadToken = if squadAdministrationPermissions.CreateSquadPermission then MetaToken |> CreateSquadToken |> Some else None
                let addOrEditPlayerToken = if squadAdministrationPermissions.AddOrEditPlayerPermission then MetaToken |> AddOrEditPlayerToken |> Some else None
                let withdrawPlayerToken = if squadAdministrationPermissions.WithdrawPlayerPermission then MetaToken |> WithdrawPlayerToken |> Some else None
                let eliminateSquadToken = if squadAdministrationPermissions.EliminateSquadPermission then MetaToken |> EliminateSquadToken |> Some else None
                createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken
            | None -> None, None, None, None
        let chatProjectionQryToken, sendChatMessageToken =
            match permissions.ChatPermissions with
            | Some chatPermissions ->
                let chatProjectionQryToken = if chatPermissions.ChatProjectionQryPermission then MetaToken |> ChatProjectionQryToken |> Some else None
                let sendChatMessageToken = if chatPermissions.SendChatMessagePermission then MetaToken |> SendChatMessageToken |> Some else None
                chatProjectionQryToken, sendChatMessageToken
            | None -> None, None
        UserTokens {
            ChangePasswordToken = changePasswordToken
            CreateUserToken = createUserToken
            ResetPasswordToken = resetPasswordToken
            ChangeUserTypeToken = changeUserTypeToken
            CreateSquadToken = createSquadToken
            AddOrEditPlayerToken = addOrEditPlayerToken
            WithdrawPlayerToken = withdrawPlayerToken
            EliminateSquadToken = eliminateSquadToken
            ChatProjectionQryToken = chatProjectionQryToken
            SendChatMessageToken = sendChatMessageToken }
    member __.ChangePasswordToken = vut.ChangePasswordToken
    member __.CreateUserToken = vut.CreateUserToken
    member __.ResetPasswordToken = vut.ResetPasswordToken
    member __.ChangeUserTypeToken = vut.ChangeUserTypeToken
    member __.CreateSquadToken = vut.CreateSquadToken
    member __.AddOrEditPlayerToken = vut.AddOrEditPlayerToken
    member __.WithdrawPlayerToken = vut.WithdrawPlayerToken
    member __.EliminateSquadToken = vut.EliminateSquadToken
    member __.ChatProjectionQryToken = vut.ChatProjectionQryToken
    member __.SendChatMessageToken = vut.SendChatMessageToken
