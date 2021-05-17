module Aornota.Sweepstake2021.Server.Events.NewsEvents

open Aornota.Sweepstake2021.Common.Domain.News
open Aornota.Sweepstake2021.Common.Domain.User
open Aornota.Sweepstake2021.Common.Markdown

open System

type NewsEvent =
    | PostCreated of postId : PostId * userId : UserId * postType : PostType * messageText : Markdown * timestamp : DateTimeOffset
    | PostChanged of postId : PostId * messageText : Markdown
    | PostRemoved of postId : PostId
    with
        member self.PostId =
            match self with
            | PostCreated (postId, _, _, _, _) -> postId
            | PostChanged (postId, _) -> postId
            | PostRemoved postId -> postId
