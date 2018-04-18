module Aornota.UI.Common.Render.Markdown

open Aornota.Sweepstake2018.Shared.Domain

open Aornota.UI.Common.Marked

open Fable.Core
module Rct = Fable.Helpers.React
open Fable.Helpers.React.Props

[<Pojo>]
type DangerousInnerHtml = { __html : string }

let htmlFromMarkdown (Markdown markdown) = Rct.div [ DangerouslySetInnerHTML { __html = Marked.Globals.marked.parse markdown } ] []
