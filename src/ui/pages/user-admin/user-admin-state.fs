module Aornota.Sweepstake2018.UI.Pages.UserAdmin.State

open Aornota.Common.Delta
open Aornota.Common.IfDebug
open Aornota.Common.Revision
open Aornota.Common.UnexpectedError

open Aornota.UI.Common.Notifications
open Aornota.UI.Common.ShouldNeverHappen
open Aornota.UI.Common.Toasts

open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.Common.Domain.UserAdmin
open Aornota.Sweepstake2018.Common.WsApi.ServerMsg
open Aornota.Sweepstake2018.Common.WsApi.UiMsg
open Aornota.Sweepstake2018.UI.Pages.UserAdmin.Common

open System

open Elmish

let initialize authUser : State * Cmd<Input> =
    let state = {
        AuthUser = authUser
        ProjectionState = Initializing }
    let cmd = InitializeUserAdminProjectionQry |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
    state, cmd

let private defaultCreateUsersState userTypes userType createUserStatus = {
    UserTypes = userTypes
    NewUserId = UserId.Create ()
    NewUserNameText = String.Empty
    NewUserNameErrorText = None
    NewPasswordKey = Guid.NewGuid ()
    NewPasswordText = String.Empty
    NewPasswordErrorText = None
    ConfirmPasswordKey = Guid.NewGuid ()
    ConfirmPasswordText = String.Empty
    ConfirmPasswordErrorText = None
    NewUserType = userType
    CreateUserStatus = createUserStatus }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private user4Admin (user4AdminDto:User4AdminDto) =
    { Rvn = user4AdminDto.Rvn ; UserName = user4AdminDto.UserName ; UserType = user4AdminDto.UserType ; LastActivity = user4AdminDto.LastActivity }

let private userAdminProjection (userAdminProjectionDto:UserAdminProjectionDto) =
    let user4AdminDic = User4AdminDic ()
    userAdminProjectionDto.User4AdminDtos |> List.iter (fun user4AdminDto ->
        let userId = user4AdminDto.UserId
        if userId |> user4AdminDic.ContainsKey |> not then // note: silently ignore duplicate UserIds (should never happen)
            (userId, user4AdminDto |> user4Admin) |> user4AdminDic.Add)
    { Rvn = initialRvn ; User4AdminDic = user4AdminDic }

let private applyUsers4AdminDelta currentRvn deltaRvn (delta:Delta<UserId, User4AdminDto>) (user4AdminDic:User4AdminDic) =
    let user4AdminDic = User4AdminDic user4AdminDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (userId, user4AdminDto) -> if userId |> user4AdminDic.ContainsKey then (userId, user4AdminDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (userId, user4AdminDto) -> (userId, user4AdminDto |> user4Admin) |> user4AdminDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (userId, user4AdminDto) -> if userId |> user4AdminDic.ContainsKey |> not then (userId, user4AdminDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (userId, user4AdminDto) -> user4AdminDic.[userId] <- (user4AdminDto |> user4Admin)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun userId -> if userId |> user4AdminDic.ContainsKey |> not then userId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (user4AdminDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> user4AdminDic |> Ok)

let private cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
let private qryErrorText error = match error with | AuthQryJwtError _ | AuthQryAuthznError _ -> UNEXPECTED_ERROR | OtherAuthQryError (OtherError errorText) -> errorText

let private handleCreateUserCmdResult (result:Result<UserName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.CreateUsersState with
    | Some createUsersState ->
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                let createUsersState = defaultCreateUsersState createUsersState.UserTypes createUsersState.NewUserType None
                let activeState = { activeState with CreateUsersState = createUsersState |> Some }
                { state with ProjectionState = Active activeState }, sprintf "<strong>%s</strong> has been added" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "CreateUserCmdResult error -> %A" error) (error |> cmdErrorText)
                let createUsersState = { createUsersState with CreateUserStatus = errorText |> CreateUserFailed |> Some }
                let activeState = { activeState with CreateUsersState = createUsersState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to add user" |> errorToastCmd
        | Some (CreateUserFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected CreateUserCmdResult when CreateUserStatus is not CreateUserPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected CreateUserCmdResult when CreateUsersState is None -> %A" result)

let private handleResetPasswordCmdResult (result:Result<UserName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.ResetPasswordState with
    | Some resetPasswordState ->
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                let activeState = { activeState with ResetPasswordState = None }
                { state with ProjectionState = Active activeState }, sprintf "Password has been reset for <strong>%s</strong>" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ResetPasswordCmdResult error -> %A" error) (error |> cmdErrorText)
                let resetPasswordState = { resetPasswordState with ResetPasswordStatus = errorText |> ResetPasswordFailed |> Some }
                let activeState = { activeState with ResetPasswordState = resetPasswordState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to reset password" |> errorToastCmd
        | Some (ResetPasswordFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordCmdResult when ResetPasswordStatus is not ResetPasswordPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordCmdResult when ResetPasswordState is None -> %A" result)

let private handleChangeUserTypeCmdResult (result:Result<UserName, AuthCmdError<string>>) activeState state : State * Cmd<Input> =
    match activeState.ChangeUserTypeState with
    | Some changeUserTypeState ->
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                let activeState = { activeState with ChangeUserTypeState = None }
                { state with ProjectionState = Active activeState }, sprintf "Type has been changed for <strong>%s</strong>" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangeUserTypeCmdResult error -> %A" error) (error |> cmdErrorText)
                let changeUserTypeState = { changeUserTypeState with ChangeUserTypeStatus = errorText |> ChangeUserTypeFailed |> Some }
                let activeState = { activeState with ChangeUserTypeState = changeUserTypeState |> Some }
                { state with ProjectionState = Active activeState }, "Unable to change user type" |> errorToastCmd
        | Some (ChangeUserTypeFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeCmdResult when ChangeUserTypeStatus is not ChangeUserTypePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeCmdResult when ChangeUserTypeState is None -> %A" result)

let private handleServerUserAdminMsg serverUserAdminMsg state : State * Cmd<Input> =
    match serverUserAdminMsg, state.ProjectionState with
    | InitializeUserAdminProjectionQryResult (Ok userAdminProjectionDto), Initializing ->
        let userAdminProjection = userAdminProjectionDto |> userAdminProjection
        let activeState = {
            UserAdminProjection = userAdminProjection
            CreateUsersState = None
            ResetPasswordState = None
            ChangeUserTypeState = None }
        { state with ProjectionState = Active activeState }, Cmd.none
    | InitializeUserAdminProjectionQryResult (Error error), Initializing _ ->
        { state with ProjectionState = InitializationFailed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | CreateUserCmdResult result, Active activeState ->
        state |> handleCreateUserCmdResult result activeState
    | ResetPasswordCmdResult result, Active activeState ->
        state |> handleResetPasswordCmdResult result activeState
    | ChangeUserTypeCmdResult result, Active activeState ->
        state |> handleChangeUserTypeCmdResult result activeState
    | UserAdminProjectionMsg (Users4AdminDeltaMsg (deltaRvn, user4AdminDtoDelta)), Active activeState ->
        let userAdminProjection = activeState.UserAdminProjection
        let user4AdminDic = userAdminProjection.User4AdminDic
        match user4AdminDic |> applyUsers4AdminDelta userAdminProjection.Rvn deltaRvn user4AdminDtoDelta with
        | Ok user4AdminDic ->
            let userAdminProjection = { userAdminProjection with Rvn = deltaRvn ; User4AdminDic = user4AdminDic }
            let createUsersState =
                match activeState.CreateUsersState with
                | Some createUsersState ->
                    let newUserNameText = createUsersState.NewUserNameText
                    if String.IsNullOrWhiteSpace newUserNameText |> not then
                        let newUserNameErrorText = validateUserName (user4AdminDic |> userNames) (UserName newUserNameText)
                        { createUsersState with NewUserNameErrorText = newUserNameErrorText } |> Some
                    else createUsersState |> Some
                | Some _ | None -> None
            let activeState = { activeState with UserAdminProjection = userAdminProjection ; CreateUsersState = createUsersState }
            { state with ProjectionState = Active activeState }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" user4AdminDtoDelta user4AdminDic error)
            let state, cmd = initialize state.AuthUser
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | UserAdminProjectionMsg _, _ -> // note: silently ignore UserAdminProjectionMsg if not Active
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerUserAdminMsg when %A -> %A" state.ProjectionState serverUserAdminMsg)

let handleCreateUsersInput createUsersInput activeState state : State * Cmd<Input> * bool =
    match createUsersInput, activeState.CreateUsersState with
    | NewUserNameTextChanged newUserNameText, Some createUsersState ->
        let userNames = activeState.UserAdminProjection.User4AdminDic |> userNames
        let newUserNameErrorText = validateUserName userNames (UserName newUserNameText)
        let createUsersState = { createUsersState with NewUserNameText = newUserNameText ; NewUserNameErrorText = newUserNameErrorText }
        let activeState = { activeState with CreateUsersState = createUsersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | CreateUsersInput.NewPasswordTextChanged newPasswordText, Some createUsersState ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace createUsersState.ConfirmPasswordText then createUsersState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password createUsersState.ConfirmPasswordText)
        let createUsersState = { createUsersState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let activeState = { activeState with CreateUsersState = createUsersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | CreateUsersInput.ConfirmPasswordTextChanged confirmPasswordText, Some createUsersState ->
        let confirmPasswordErrorText = validateConfirmPassword (Password createUsersState.NewPasswordText) (Password confirmPasswordText)
        let createUsersState = { createUsersState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let activeState = { activeState with CreateUsersState = createUsersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | NewUserTypeChanged newUserType, Some createUsersState ->
        let createUsersState = { createUsersState with NewUserType = newUserType }
        let activeState = { activeState with CreateUsersState = createUsersState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | CreateUser, Some createUsersState -> // note: assume no need to validate NewUserNameText or NewPasswordText or ConfirmPasswordText (i.e. because UserAdmin.Render.renderCreateUsersModal will ensure that CreateUser can only be dispatched when valid)
        let createUsersState = { createUsersState with CreateUserStatus = CreateUserPending |> Some }   
        let activeState = { activeState with CreateUsersState = createUsersState |> Some }
        let createUserCmdParams =
            createUsersState.NewUserId, UserName (createUsersState.NewUserNameText.Trim ()), Password (createUsersState.NewPasswordText.Trim ()), createUsersState.NewUserType
        let cmd = createUserCmdParams |> CreateUserCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelCreateUsers, Some createUsersState ->
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending ->
            state, shouldNeverHappenCmd "Unexpected CancelCreateUsers when CreateUserPending", false
        | Some (CreateUserFailed _) | None ->
            let activeState = { activeState with CreateUsersState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected CreateUsersInput when CreateUsersState is None -> %A" createUsersInput), false

let handleResetPasswordInput resetPasswordInput activeState state : State * Cmd<Input> * bool =
    match resetPasswordInput, activeState.ResetPasswordState with
    | ResetPasswordInput.NewPasswordTextChanged newPasswordText, Some resetPasswordState ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace resetPasswordState.ConfirmPasswordText then resetPasswordState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password resetPasswordState.ConfirmPasswordText)
        let resetPasswordState = { resetPasswordState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let activeState = { activeState with ResetPasswordState = resetPasswordState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | ResetPasswordInput.ConfirmPasswordTextChanged confirmPasswordText, Some resetPasswordState ->
        let confirmPasswordErrorText = validateConfirmPassword (Password resetPasswordState.NewPasswordText) (Password confirmPasswordText)
        let resetPasswordState = { resetPasswordState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let activeState = { activeState with ResetPasswordState = resetPasswordState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | ResetPassword, Some resetPasswordState -> // note: assume no need to validate NewUserNameText or NewPasswordText or ConfirmPasswordText (i.e. because UserAdmin.Render.renderCreateUsersModal will ensure that CreateUser can only be dispatched when valid)
        let resetPasswordState = { resetPasswordState with ResetPasswordStatus = ResetPasswordPending |> Some }   
        let activeState = { activeState with ResetPasswordState = resetPasswordState |> Some }
        let user4AdminDic = activeState.UserAdminProjection.User4AdminDic
        let userId = resetPasswordState.UserId
        let currentRvn = if userId |> user4AdminDic.ContainsKey then user4AdminDic.[userId].Rvn else initialRvn
        let resetPasswordCmdParams = userId, currentRvn, Password (resetPasswordState.NewPasswordText.Trim ())
        let cmd = resetPasswordCmdParams |> ResetPasswordCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProjectionState = Active activeState }, cmd, true
    | CancelResetPassword, Some resetPasswordState ->
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending ->
            state, shouldNeverHappenCmd "Unexpected CancelResetPassword when ResetPasswordPending", false
        | Some (ResetPasswordFailed _) | None ->
            let activeState = { activeState with ResetPasswordState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordInput when ResetPasswordState is None -> %A" resetPasswordInput), false

let handleChangeUserTypeInput changeUserTypeInput activeState state : State * Cmd<Input> * bool =
    match changeUserTypeInput, activeState.ChangeUserTypeState with
    | UserTypeChanged userType, Some changeUserTypeState ->
        let changeUserTypeState = { changeUserTypeState with UserType = userType |> Some }
        let activeState = { activeState with ChangeUserTypeState = changeUserTypeState |> Some }
        { state with ProjectionState = Active activeState }, Cmd.none, true
    | ChangeUserType, Some changeUserTypeState -> // note: assume no need to validate UserType (i.e. because UserAdmin.Render.renderChangeUserTypeModal will ensure that ChangeUserType can only be dispatched when valid)
        match changeUserTypeState.UserType with
        | Some userType ->
            let changeUserTypeState = { changeUserTypeState with ChangeUserTypeStatus = ChangeUserTypePending |> Some }   
            let activeState = { activeState with ChangeUserTypeState = changeUserTypeState |> Some }
            let user4AdminDic = activeState.UserAdminProjection.User4AdminDic
            let userId = changeUserTypeState.UserId
            let currentRvn = if userId |> user4AdminDic.ContainsKey then user4AdminDic.[userId].Rvn else initialRvn
            let changeUserTypeCmdParams = userId, currentRvn, userType
            let cmd = changeUserTypeCmdParams |> ChangeUserTypeCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ProjectionState = Active activeState }, cmd, true
        | None -> // note: should never happen
            state, Cmd.none, false
    | CancelChangeUserType, Some changeUserTypeState ->
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangeUserType when ChangeUserTypePending", false
        | Some (ChangeUserTypeFailed _) | None ->
            let activeState = { activeState with ChangeUserTypeState = None }
            { state with ProjectionState = Active activeState }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeInput when ChangeUserTypeState is None -> %A" changeUserTypeInput), false

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ProjectionState with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerUserAdminMsg serverUserAdminMsg, _ ->
            let state, cmd = state |> handleServerUserAdminMsg serverUserAdminMsg
            state, cmd, false
        | ShowCreateUsersModal userTypes, Active activeState ->
            let createUsersState = defaultCreateUsersState userTypes Pleb None
            let activeState = { activeState with CreateUsersState = createUsersState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | CreateUsersInput createUsersInput, Active activeState ->
            state |> handleCreateUsersInput createUsersInput activeState
        | ShowResetPasswordModal userId, Active activeState -> // note: no need to check for unknown userId (should never happen)
            let resetPasswordState = {
                UserId = userId
                NewPasswordKey = Guid.NewGuid ()
                NewPasswordText = String.Empty
                NewPasswordErrorText = None
                ConfirmPasswordKey = Guid.NewGuid ()
                ConfirmPasswordText = String.Empty
                ConfirmPasswordErrorText = None
                ResetPasswordStatus = None }
            let activeState = { activeState with ResetPasswordState = resetPasswordState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ResetPasswordInput resetPasswordInput, Active activeState ->
            state |> handleResetPasswordInput resetPasswordInput activeState
        | ShowChangeUserTypeModal (userId, userTypes), Active activeState -> // note: no need to check for unknown userId (should never happen)
            let changeUserTypeState = { UserId = userId ; UserTypes = userTypes ; UserType = None ; ChangeUserTypeStatus = None }
            let activeState = { activeState with ChangeUserTypeState = changeUserTypeState |> Some }
            { state with ProjectionState = Active activeState }, Cmd.none, true
        | ChangeUserTypeInput changeUserTypeInput, Active activeState ->
            state |> handleChangeUserTypeInput changeUserTypeInput activeState
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ProjectionState input), false
    state, cmd, isUserNonApiActivity
