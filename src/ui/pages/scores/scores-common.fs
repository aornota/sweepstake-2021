module Aornota.Sweepstake2018.UI.Pages.Scores.Common

open Aornota.UI.Common.Notifications

open Aornota.Sweepstake2018.Common.Domain.User

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | ShowUser of userId : UserId

type State = { CurrentUserId : UserId option }
