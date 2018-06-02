module Aornota.Sweepstake2018.UI.Pages.UserAdmin.Render

open Aornota.Common.UnitsOfMeasure

open Aornota.UI.Common.LazyViewOrHMR
open Aornota.UI.Render.Bulma
open Aornota.UI.Render.Common
open Aornota.UI.Theme.Common
open Aornota.UI.Theme.Render.Bulma
open Aornota.UI.Theme.Shared

open Aornota.Sweepstake2018.Common.Domain.User
open Aornota.Sweepstake2018.UI.Pages.UserAdmin.Common

open System

module Rct = Fable.Helpers.React

let [<Literal>] private RECENTLY_ACTIVE = 5.<minute>

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private (|Self|RecentlyActive|SignedIn|NotSignedIn|PersonaNonGrata|) (authUserId:UserId, (userId:UserId, user4Admin:User4Admin)) =
    if userId = authUserId then Self
    else if user4Admin.UserType = UserType.PersonaNonGrata then PersonaNonGrata
    else
        match user4Admin.LastActivity with
        | Some lastApi ->   
            let recentlyActiveCutoff = cutoff (int (RECENTLY_ACTIVE |> minutesToSeconds) * 1<second>)
            if lastApi > recentlyActiveCutoff then RecentlyActive else SignedIn
        | None -> NotSignedIn

let private semantic authUserId (userId, user4Admin) =
    match (authUserId, (userId, user4Admin)) with
    | Self -> Link
    | RecentlyActive -> Success
    | SignedIn -> Primary
    | NotSignedIn -> Dark
    | PersonaNonGrata -> Light

let private userTypes = [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]

let private userTypeSortOrder userType = match userType with | SuperUser -> 1 | Administrator -> 2 | Pleb -> 3 | UserType.PersonaNonGrata -> 4
let private userTypeText userType = match userType with | SuperUser -> "Super user" | Administrator -> "Administrator" | Pleb -> "Pleb" | UserType.PersonaNonGrata -> "Persona non grata"

let private userTypeRadios selectedUserType allowedUserTypes disabledUserType disableAll dispatch =
    let onChange userType = (fun _ -> userType |> dispatch)
    userTypes
    |> List.sortBy userTypeSortOrder
    |> List.map (fun userType ->
        let isSelected = userType |> Some = selectedUserType
        let disabled = disableAll || allowedUserTypes |> List.contains userType |> not || userType |> Some = disabledUserType
        let onChange = if isSelected || disabled then ignore else userType |> onChange
        radioInline (userType |> userTypeText) isSelected disabled onChange)
    |> List.collect id

let private renderCreateUsersModal (useDefaultTheme, user4AdminDic:User4AdminDic, createUsersState:CreateUsersState) dispatch =
    let theme = getTheme useDefaultTheme
    let onDismiss = match createUsersState.CreateUserStatus with | Some CreateUserPending -> None | Some _ | None -> (fun _ -> CancelCreateUsers |> dispatch) |> Some
    let userNames = user4AdminDic |> userNames
    let isCreatingUser, createUserInteraction, onEnter =
        let createUser = (fun _ -> CreateUser |> dispatch)
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending -> true, Loading, ignore
        | Some (CreateUserFailed _) | None ->
            let validUserName = validateUserName userNames (UserName createUsersState.NewUserNameText)
            let validPassword = validatePassword (Password createUsersState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password createUsersState.NewPasswordText) (Password createUsersState.ConfirmPasswordText)
            match validUserName, validPassword, validConfirmPassword with
            | None, None, None -> false, Clickable (createUser, None), createUser
            | _ -> false, NotEnabled None, ignore
    let errorText = match createUsersState.CreateUserStatus with | Some (CreateUserFailed errorText) -> errorText |> Some | Some CreateUserPending | None -> None
    let (UserId newUserKey) = createUsersState.NewUserId
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the name, password (twice) and type for the new user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme newUserKey createUsersState.NewUserNameText (iconUserSmall |> Some) false createUsersState.NewUserNameErrorText [] true isCreatingUser
                (NewUserNameTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme createUsersState.NewPasswordKey createUsersState.NewPasswordText (iconPasswordSmall |> Some) true createUsersState.NewPasswordErrorText []
                false isCreatingUser (CreateUsersInput.NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme createUsersState.ConfirmPasswordKey createUsersState.ConfirmPasswordText (iconPasswordSmall |> Some) true createUsersState.ConfirmPasswordErrorText []
                false isCreatingUser (CreateUsersInput.ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! userTypeRadios (createUsersState.NewUserType |> Some) createUsersState.UserTypes None isCreatingUser (NewUserTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Add user" ] |> button theme { buttonLinkSmall with Interaction = createUserInteraction } ] ]
    cardModal theme [ [ bold "Add user/s" ] |> para theme paraCentredSmall ] onDismiss body

let private renderResetPasswordModal (useDefaultTheme, user4AdminDic:User4AdminDic, resetPasswordState:ResetPasswordState) dispatch =
    let theme = getTheme useDefaultTheme
    let userId = resetPasswordState.UserId
    let user4Admin = if userId |> user4AdminDic.ContainsKey then user4AdminDic.[userId] |> Some else None
    let titleText =
        match user4Admin with
        | Some user4Admin ->
            let (UserName userName) = user4Admin.UserName
            sprintf "Reset password for %s" userName
        | None -> "Reset password" // note: should never happen
    let onDismiss = match resetPasswordState.ResetPasswordStatus with | Some ResetPasswordPending -> None | Some _ | None -> (fun _ -> CancelResetPassword |> dispatch) |> Some
    let isResettingPassword, resetPasswordInteraction, onEnter =
        let resetPassword = (fun _ -> ResetPassword |> dispatch)
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending -> true, Loading, ignore
        | Some (ResetPasswordFailed _) | None ->
            let validPassword = validatePassword (Password resetPasswordState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password resetPasswordState.NewPasswordText) (Password resetPasswordState.ConfirmPasswordText)
            match validPassword, validConfirmPassword with
            | None, None -> false, Clickable (resetPassword, None), resetPassword
            | _ -> false, NotEnabled None, ignore
    let errorText = match resetPasswordState.ResetPasswordStatus with | Some (ResetPasswordFailed errorText) -> errorText |> Some | Some ResetPasswordPending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the new password (twice) for the user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme resetPasswordState.NewPasswordKey resetPasswordState.NewPasswordText (iconPasswordSmall |> Some) true resetPasswordState.NewPasswordErrorText []
                true isResettingPassword (ResetPasswordInput.NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme resetPasswordState.ConfirmPasswordKey resetPasswordState.ConfirmPasswordText (iconPasswordSmall |> Some) true resetPasswordState.ConfirmPasswordErrorText []
                false isResettingPassword (ResetPasswordInput.ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Reset password" ] |> button theme { buttonLinkSmall with Interaction = resetPasswordInteraction } ] ]
    cardModal theme [ [ bold titleText ] |> para theme paraCentredSmall ] onDismiss body

let private renderChangeUserTypeModal (useDefaultTheme, user4AdminDic:User4AdminDic, changeUserTypeState:ChangeUserTypeState) dispatch =
    let theme = getTheme useDefaultTheme
    let userId = changeUserTypeState.UserId
    let user4Admin = if userId |> user4AdminDic.ContainsKey then user4AdminDic.[userId] |> Some else None
    let currentUserType, titleText =
        match user4Admin with
        | Some user4Admin ->
            let (UserName userName) = user4Admin.UserName
            user4Admin.UserType |> Some, sprintf "Change type for %s" userName
        | None -> None, "Change user type" // note: should never happen
    let onDismiss = match changeUserTypeState.ChangeUserTypeStatus with | Some ChangeUserTypePending -> None | Some _ | None -> (fun _ -> CancelChangeUserType |> dispatch) |> Some
    let isChangingUserType, changeUserTypeInteraction =
        let changeUserType = (fun _ -> ChangeUserType |> dispatch)
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending -> true, Loading
        | Some (ChangeUserTypeFailed _) | None ->
            let isValid = match changeUserTypeState.UserType with | Some userType -> userType |> Some <> currentUserType | None -> false
            if isValid |> not then false, NotEnabled None
            else false, Clickable (changeUserType, None)
    let errorText = match changeUserTypeState.ChangeUserTypeStatus with | Some (ChangeUserTypeFailed errorText) -> errorText |> Some | Some ChangeUserTypePending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please choose the new type for the user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! userTypeRadios changeUserTypeState.UserType changeUserTypeState.UserTypes currentUserType isChangingUserType (UserTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change type" ] |> button theme { buttonLinkSmall with Interaction = changeUserTypeInteraction } ] ]
    cardModal theme [ [ bold titleText ] |> para theme paraCentredSmall ] onDismiss body

let private tagUser4Admin = { tagDefault with IsRounded = false }

let private renderUser4Admin theme semantic user4Admin =
    let (UserName userName) = user4Admin.UserName
    [ str userName ] |> tag theme { tagUser4Admin with TagSemantic = semantic |> Some }

let private renderUsers4Admin (useDefaultTheme, user4AdminDic:User4AdminDic, authUser) dispatch =
    let theme = getTheme useDefaultTheme
    let resetPassword userId (user4Admin:User4Admin) =
        match authUser.Permissions.UserAdminPermissions with
        | Some userAdminPermissions ->
            match userAdminPermissions.ResetPasswordPermission with
            | Some (NotSelf userTypes) ->
                if userId <> authUser.UserId && userTypes |> List.contains user4Admin.UserType then
                    let onClick = (fun _ -> userId |> ShowResetPasswordModal |> dispatch)
                    [ [ str "Reset password" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink onClick) |> Some
                else None
            | None -> None
        | None -> None // note: should never happen
    let changeUserType userId (user4Admin:User4Admin) =
        match authUser.Permissions.UserAdminPermissions with
        | Some userAdminPermissions ->
            match userAdminPermissions.ChangeUserTypePermission with
            | Some (NotSelf userTypes, newUserTypes) ->
                if userId <> authUser.UserId && userTypes |> List.contains user4Admin.UserType then
                    let onClick = (fun _ -> (userId, newUserTypes) |> ShowChangeUserTypeModal |> dispatch)
                    [ [ str "Change type" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink onClick) |> Some
                else None
            | None -> None
        | None -> None // note: should never happen
    let user4AdminRow (userId, user4Admin, semantic) =
        let userName, userTypeText = user4Admin |> renderUser4Admin theme semantic, user4Admin.UserType |> userTypeText
        tr false [
            td [ [ userName ] |> para theme paraDefaultSmallest ]
            td [ [ str userTypeText ] |> para theme paraCentredSmallest ]
            td [ Rct.ofOption (changeUserType userId user4Admin) ]
            td [ Rct.ofOption (resetPassword userId user4Admin) ] ]
    let sortedUsers4Admin =
        user4AdminDic
        |> List.ofSeq
        |> List.map (fun (KeyValue (userId, user4Admin)) ->
            let semantic = (userId, user4Admin) |> semantic authUser.UserId
            userId, user4Admin, semantic)
        |> List.sortBy (fun (_, user4Admin, _) ->
            user4Admin.UserType |> userTypeSortOrder, user4Admin.UserName)
    let user4AdminRows = sortedUsers4Admin |> List.map (fun (userId, user4Admin, semantic) -> (userId, user4Admin, semantic) |> user4AdminRow)
    div divCentred [
        if user4AdminDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [ 
                    tr false [
                        th [ [ bold "User" ] |> para theme paraDefaultSmallest ]
                        th [ [ bold "Type" ] |> para theme paraCentredSmallest ]
                        th []
                        th [] ] ]
                tbody [ yield! user4AdminRows ] ]
        else yield [ str "Player details coming soon" ] |> para theme paraCentredSmaller ] // note: should never happen

let private createUsers theme authUser dispatch =
    match authUser.Permissions.UserAdminPermissions with
    | Some userAdminPermissions ->
        let userTypes = userAdminPermissions.CreateUserPermission
        match userTypes with
        | _ :: _ ->
            let onClick = (fun _ -> userTypes |> ShowCreateUsersModal |> dispatch)
            [ [ str "Add user/s" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (ClickableLink onClick) |> Some
        | [] -> None
    | None -> None // note: should never happen

let render (useDefaultTheme, state, hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ bold "User administration" ] |> para theme paraCentredSmall
        yield hr theme false
        match state.ProjectionState with
        | Initializing ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | InitializationFailed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Active activeState ->
            let user4AdminDic = activeState.UserAdminProjection.User4AdminDic
            match hasModal, activeState.CreateUsersState with
            | false, Some createUsersState ->
                yield div divDefault [ lazyViewOrHMR2 renderCreateUsersModal (useDefaultTheme, user4AdminDic, createUsersState) (CreateUsersInput >> dispatch) ]
            | _ -> ()
            match hasModal, activeState.ResetPasswordState with
            | false, Some resetPasswordState ->
                yield div divDefault [ lazyViewOrHMR2 renderResetPasswordModal (useDefaultTheme, user4AdminDic, resetPasswordState) (ResetPasswordInput >> dispatch) ]
            | _ -> ()
            match hasModal, activeState.ChangeUserTypeState with
            | false, Some changeUserTypeState ->
                yield div divDefault [ lazyViewOrHMR2 renderChangeUserTypeModal (useDefaultTheme, user4AdminDic, changeUserTypeState) (ChangeUserTypeInput >> dispatch) ]
            | _ -> ()
            yield lazyViewOrHMR2 renderUsers4Admin (useDefaultTheme, activeState.UserAdminProjection.User4AdminDic, state.AuthUser) dispatch
            yield Rct.ofOption (createUsers theme state.AuthUser dispatch) ]
