module Aornota.Sweepstake2018.UI.Pages.UserAdmin.Common

open Aornota.Common.Revision

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg

open System
open System.Collections.Generic

type CreateUsersInput =
    | NewUserNameTextChanged of newUserNameText : string
    | NewPasswordTextChanged of newPasswordText : string
    | ConfirmPasswordTextChanged of confirmPasswordText : string
    | NewUserTypeChanged of newUserType : UserType
    | CreateUser
    | CancelCreateUsers

type ResetPasswordInput =
    | NewPasswordTextChanged of newPasswordText : string
    | ConfirmPasswordTextChanged of confirmPasswordText : string
    | ResetPassword
    | CancelResetPassword

type ChangeUserTypeInput =
    | UserTypeChanged of userType : UserType
    | ChangeUserType
    | CancelChangeUserType

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg
    | ShowCreateUsersModal of userTypes : UserType list
    | CreateUsersInput of createUsersInput : CreateUsersInput
    | ShowResetPasswordModal of userId : UserId
    | ResetPasswordInput of resetPasswordInput : ResetPasswordInput
    | ShowChangeUserTypeModal of userId : UserId * userTypes : UserType list
    | ChangeUserTypeInput of changeUserTypeInput : ChangeUserTypeInput

type User4Admin = { Rvn : Rvn ; UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }
type User4AdminDic = Dictionary<UserId, User4Admin>

type UserAdminProjection = { Rvn : Rvn ; User4AdminDic : User4AdminDic }

type CreateUserStatus =
    | CreateUserPending
    | CreateUserFailed of errorText : string

type CreateUsersState = {
    UserTypes : UserType list
    NewUserId : UserId
    NewUserNameText : string
    NewUserNameErrorText : string option
    NewPasswordKey : Guid
    NewPasswordText : string
    NewPasswordErrorText : string option
    ConfirmPasswordKey : Guid
    ConfirmPasswordText : string
    ConfirmPasswordErrorText : string option
    NewUserType : UserType
    CreateUserStatus : CreateUserStatus option }

type ResetPasswordStatus =
    | ResetPasswordPending
    | ResetPasswordFailed of errorText : string

type ResetPasswordState = {
    UserId : UserId
    NewPasswordKey : Guid
    NewPasswordText : string
    NewPasswordErrorText : string option
    ConfirmPasswordKey : Guid
    ConfirmPasswordText : string
    ConfirmPasswordErrorText : string option
    ResetPasswordStatus : ResetPasswordStatus option }

type ChangeUserTypeStatus =
    | ChangeUserTypePending
    | ChangeUserTypeFailed of errorText : string

type ChangeUserTypeState = {
    UserId : UserId
    UserTypes : UserType list
    UserType : UserType option
    ChangeUserTypeStatus : ChangeUserTypeStatus option }

type ActiveState = {
    UserAdminProjection : UserAdminProjection
    CreateUsersState : CreateUsersState option
    ResetPasswordState : ResetPasswordState option
    ChangeUserTypeState : ChangeUserTypeState option }

type ProjectionState =
    | Initializing
    | InitializationFailed
    | Active of activeState : ActiveState

type State = { 
    AuthUser : AuthUser
    ProjectionState : ProjectionState }

let userNames (user4AdminDic:User4AdminDic) = user4AdminDic |> List.ofSeq |> List.map (fun (KeyValue (_, user4Admin)) -> user4Admin.UserName)
