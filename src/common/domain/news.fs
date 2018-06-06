module Aornota.Sweepstake2018.Common.Domain.News

open Aornota.Common.Markdown
open Aornota.Common.Revision

open Aornota.Sweepstake2018.Common.Domain.User

open System

type PostId = | PostId of guid : Guid with
    static member Create () = Guid.NewGuid () |> PostId

type PostType =
    | Standard
    // TODO-SOON... | MatchResult of matchResultId : MatchResultId

type PostTypeDto =
    | StandardDto of messageText : Markdown
    // TODO-SOON... | MatchResultDto of messageText : Markdown * matchResult : TBC

type PostDto = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset }

let [<Literal>] private MAX_NEWS_POST_LENGTH = 2000

let validatePostMessageText (Markdown messageText) =
    if String.IsNullOrWhiteSpace messageText then "News post must not be blank" |> Some
    else if (messageText.Trim ()).Length > MAX_NEWS_POST_LENGTH then "News post is too long" |> Some
    else None
