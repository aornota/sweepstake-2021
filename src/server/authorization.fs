module Aornota.Sweepstake2018.Server.Authorization

open Aornota.Sweepstake2018.Common.Domain.Core

(* TEMP-NMB: Copied from serf-web-app: server-semantics.fs | authorization.fs...
open Newtonsoft.Json

type IAuthorizationToken = interface end

type MetaToken = private | MetaToken

type UnauthenticatedUserToken [<JsonConstructor>] private () =
    new (_:MetaToken) = UnauthenticatedUserToken ()
    interface IAuthorizationToken

type DatabaseInfoToken [<JsonConstructor>] private (allowedSqlServers:AllowedSqlServers) =
    new (_:MetaToken, allowedSqlServers:AllowedSqlServers) = DatabaseInfoToken allowedSqlServers
    member dit.AllowedSqlServers = allowedSqlServers
    interface IAuthorizationToken

type private ValidatedUserAuthorizationTokens = {
    UnauthenticatedUserToken : UnauthenticatedUserToken option
    AuthenticatedUserToken : AuthenticatedUserToken option
    DatabaseInfoToken : DatabaseInfoToken option
    BackupsToken : BackupsToken option
    RestoreBackupToken : RestoreBackupToken option }

type UserAuthorizationTokens [<JsonConstructor>] private (vuat:ValidatedUserAuthorizationTokens) =
    new userAuthorizations =
        UserAuthorizationTokens {
            UnauthenticatedUserToken = None
            AuthenticatedUserToken = Some (AuthenticatedUserToken MetaToken)
            DatabaseInfoToken =
                match userAuthorizations.DatabaseInfoAuthorization with
                | DatabaseInfoAuthorization.Authorized allowedSqlServers -> Some (DatabaseInfoToken (MetaToken, allowedSqlServers))
                | _ -> None
            BackupsToken =
                match userAuthorizations.BackupsAuthorization with
                | Authorized _ -> Some (BackupsToken MetaToken)
                | _ -> None
            RestoreBackupToken =
                match userAuthorizations.BackupsAuthorization with
                | Authorized (RestoreBackupAuthorization.Authorized allowedSqlServers) -> Some (RestoreBackupToken (MetaToken, allowedSqlServers))
                | _ -> None }
    static member ForUnauthenticatedUser =
        UserAuthorizationTokens {
            UnauthenticatedUserToken = Some (UnauthenticatedUserToken MetaToken)
            AuthenticatedUserToken = None
            DatabaseInfoToken = None
            BackupsToken = None
            RestoreBackupToken = None }
    member uat.UnauthenticatedUserToken = vuat.UnauthenticatedUserToken
    member uat.AuthenticatedUserToken = vuat.AuthenticatedUserToken
    member uat.DatabaseInfoToken = vuat.DatabaseInfoToken
    member uat.BackupsToken = vuat.BackupsToken
    member uat.RestoreBackupToken = vuat.RestoreBackupToken

type AuthorizedUser = {
    UserName : UserName
    UserAuthorizationTokens : UserAuthorizationTokens }*)

// TODO-NMB-HIGH: If = private | Xyz of thing : 'a, cannot access thing from other modules ;( ...
type ChangePasswordToken = ChangePasswordToken of onlyUserId : UserId
type CreateUserToken = CreateUserToken of onlyUserTypes : UserType list
type ResetPasswordToken = ResetPasswordToken // TODO-NMB-HIGH: of "targets" [not self; UserType list]...
type ChangeUserTypeToken = ChangeUserTypeToken // TODO-NMB-HIGH: of "targets" [not self; UserType list] * UserType list...

#if DEBUG
let changePasswordToken userId = ChangePasswordToken userId
let createUserAnyToken = CreateUserToken [ SuperUser ; Administrator ; Pleb ; PersonaNotGrata ]
let resetPasswordToken = ResetPasswordToken
let changeUserTypeToken = ChangeUserTypeToken
#endif
