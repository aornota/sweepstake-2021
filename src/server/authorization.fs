module Aornota.Sweepstake2018.Server.Authorization

open Aornota.Sweepstake2018.Common.Domain.User

type MetaToken = private | MetaToken

type ChangePasswordToken private (userId) =
    new (_:MetaToken, userId:UserId) = ChangePasswordToken userId
    member __.UserId = userId

type UserAdminProjectionQryToken private () =
    new (_:MetaToken) = UserAdminProjectionQryToken ()   
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

type SquadsProjectionAuthQryToken private () =
    new (_:MetaToken) = SquadsProjectionAuthQryToken ()
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
    UserAdminProjectionQryToken : UserAdminProjectionQryToken option
    CreateUserToken : CreateUserToken option
    ResetPasswordToken : ResetPasswordToken option
    ChangeUserTypeToken : ChangeUserTypeToken option
    SquadsProjectionAuthQryToken : SquadsProjectionAuthQryToken option
    CreateSquadToken : CreateSquadToken option
    AddOrEditPlayerToken : AddOrEditPlayerToken option
    WithdrawPlayerToken : WithdrawPlayerToken option
    EliminateSquadToken : EliminateSquadToken option
    ChatProjectionQryToken : ChatProjectionQryToken option
    SendChatMessageToken : SendChatMessageToken option }

type UserTokens private (vut:ValidatedUserTokens) =
    new (permissions:Permissions) =
        let changePasswordToken = match permissions.ChangePasswordPermission with | Some userId -> (MetaToken, userId) |> ChangePasswordToken |> Some | None -> None
        let userAdminProjectionQryToken, createUserToken, resetPasswordToken, changeUserTypeToken =
            match permissions.UserAdminPermissions with
            | Some userAdminPermissions ->
                let userAdminProjectionQryToken = MetaToken |> UserAdminProjectionQryToken |> Some
                let createUserToken = (MetaToken, userAdminPermissions.CreateUserPermission) |> CreateUserToken |> Some
                let resetPasswordToken =
                    match userAdminPermissions.ResetPasswordPermission with
                    | Some userTarget -> (MetaToken, userTarget) |> ResetPasswordToken |> Some
                    | None -> None
                let changeUserTypeToken =
                    match userAdminPermissions.ChangeUserTypePermission with
                    | Some (userTarget, userTypes) -> (MetaToken, userTarget, userTypes) |> ChangeUserTypeToken |> Some
                    | None -> None
                userAdminProjectionQryToken, createUserToken, resetPasswordToken, changeUserTypeToken
            | None -> None, None, None, None
        let squadsProjectionAuthQryToken, createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken =
            match permissions.SquadPermissions with
            | Some squadPermissions ->
                let squadsProjectionAuthQryToken = if squadPermissions.SquadProjectionAuthQryPermission then MetaToken |> SquadsProjectionAuthQryToken |> Some else None
                let createSquadToken = if squadPermissions.CreateSquadPermission then MetaToken |> CreateSquadToken |> Some else None
                let addOrEditPlayerToken = if squadPermissions.AddOrEditPlayerPermission then MetaToken |> AddOrEditPlayerToken |> Some else None
                let withdrawPlayerToken = if squadPermissions.WithdrawPlayerPermission then MetaToken |> WithdrawPlayerToken |> Some else None
                let eliminateSquadToken = if squadPermissions.EliminateSquadPermission then MetaToken |> EliminateSquadToken |> Some else None
                squadsProjectionAuthQryToken, createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken
            | None -> None, None, None, None, None
        let chatProjectionQryToken, sendChatMessageToken =
            match permissions.ChatPermissions with
            | Some chatPermissions ->
                let chatProjectionQryToken = MetaToken |> ChatProjectionQryToken |> Some
                let sendChatMessageToken = if chatPermissions.SendChatMessagePermission then MetaToken |> SendChatMessageToken |> Some else None
                chatProjectionQryToken, sendChatMessageToken
            | None -> None, None
        UserTokens {
            ChangePasswordToken = changePasswordToken
            UserAdminProjectionQryToken = userAdminProjectionQryToken
            CreateUserToken = createUserToken
            ResetPasswordToken = resetPasswordToken
            ChangeUserTypeToken = changeUserTypeToken
            SquadsProjectionAuthQryToken = squadsProjectionAuthQryToken
            CreateSquadToken = createSquadToken
            AddOrEditPlayerToken = addOrEditPlayerToken
            WithdrawPlayerToken = withdrawPlayerToken
            EliminateSquadToken = eliminateSquadToken
            ChatProjectionQryToken = chatProjectionQryToken
            SendChatMessageToken = sendChatMessageToken }
    member __.ChangePasswordToken = vut.ChangePasswordToken
    member __.UserAdminProjectionQryToken = vut.UserAdminProjectionQryToken
    member __.CreateUserToken = vut.CreateUserToken
    member __.ResetPasswordToken = vut.ResetPasswordToken
    member __.ChangeUserTypeToken = vut.ChangeUserTypeToken
    member __.SquadsProjectionAuthQryToken = vut.SquadsProjectionAuthQryToken
    member __.CreateSquadToken = vut.CreateSquadToken
    member __.AddOrEditPlayerToken = vut.AddOrEditPlayerToken
    member __.WithdrawPlayerToken = vut.WithdrawPlayerToken
    member __.EliminateSquadToken = vut.EliminateSquadToken
    member __.ChatProjectionQryToken = vut.ChatProjectionQryToken
    member __.SendChatMessageToken = vut.SendChatMessageToken
