module Aornota.Sweepstake2018.UI.Pages.Scores.Common

open Aornota.Sweepstake2018.Common.Domain.User

type Best = | Teams | Players | Goalkeepers | Defenders | Midfielders | Forwards

type ScoresFilter =
    | Sweepstaker of userId : UserId option
    | Best of best : Best option
    | BestUnpicked of best : Best option

type Input =
    | ShowSweepstaker of userId : UserId option
    | ShowBest of best : Best option
    | ShowBestUnpicked of best : Best option

type State = {
    CurrentScoresFilter : ScoresFilter
    LastSweepstaker : UserId option
    LastBest : Best option
    LastBestUnpicked : Best option }
