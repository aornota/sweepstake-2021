module Aornota.Sweepstake2018.Server.Agents.ConsoleLogger

open System

type Source = | ConsoleLogger | Broadcaster | Ticker | Persistence | Users | ChatProjection | Connections | WsMiddleware | Host

type Category = | Info of text : string | Warning of text : string | Danger of text : string | SkippedInput of text : string | IgnoredInput of text : string | AgentExn of exn : exn

type private ConsoleLoggerInput = | Log of source : Source * category : Category

let private sourceText source =
    match source with
    | ConsoleLogger -> "ConsoleLogger"
    | Broadcaster -> "Broadcaster"
    | Ticker -> "Ticker"
    | Persistence -> "Persistence"
    | Users -> "Users"
    | ChatProjection -> "ChatProjection"
    | Connections -> "Connections"
    | WsMiddleware -> "WsMiddleware"
    | Host -> "Host"

let private shouldLogSource source = // note: None is equivalent to "do not log"
    match source with
    | ConsoleLogger -> Some ConsoleColor.White
    | Broadcaster -> Some ConsoleColor.Yellow
    | Ticker -> Some ConsoleColor.DarkYellow
    | Persistence -> Some ConsoleColor.Cyan
    | Users -> Some ConsoleColor.Blue
    | ChatProjection -> Some ConsoleColor.DarkBlue
    | Connections -> Some ConsoleColor.Magenta
    | WsMiddleware -> Some ConsoleColor.DarkMagenta
    | Host -> Some ConsoleColor.Green

let private shouldLogCategory category =
    match category with
    | Info _ -> true
    | Warning _ -> true
    | Danger _ -> true
    | SkippedInput _ -> true
    | IgnoredInput _ -> true
    | AgentExn _ -> true

let private log source category =
    if shouldLogCategory category then
        match shouldLogSource source with
        | Some colour ->
            let prefix, text, colour =
                let sourceText = sourceText source
                match category with
                | Info text -> None, sprintf "%s => %s" sourceText text, colour
                | Warning text -> Some (" Warning ", ConsoleColor.Black, ConsoleColor.White), sprintf "%s => %s" sourceText text, ConsoleColor.DarkRed
                | Danger text -> Some (" Danger ", ConsoleColor.White, ConsoleColor.Red), sprintf "%s => %s" sourceText text, ConsoleColor.Red
                | SkippedInput text -> None, sprintf "%s => skipped input -> %s" sourceText text, ConsoleColor.Gray
                | IgnoredInput text -> None, sprintf "%s => ignored input -> %s" sourceText text, ConsoleColor.DarkGray
                | AgentExn exn -> Some (" CRITICAL ", ConsoleColor.White, ConsoleColor.Red), sprintf "%s => agent terminated -> %s" sourceText exn.Message, ConsoleColor.Red
#if DEBUG
            // Note: No need for lock since only called from ConsoleLogger agent (though can still get mixed up with ASP.Net Core logging output, i.e. since Console not thread-safe).
            let timestampText = sprintf "%s " ((DateTime.Now.ToUniversalTime ()).ToString ("HH:mm:ss.fff"))
            let previousForegroundColour = Console.ForegroundColor
            Console.Write timestampText
            match prefix with
            | Some (prefixText, foregroundColour, backgroundColor) ->
                let previousBackgroundColour = Console.BackgroundColor
                Console.ForegroundColor <- foregroundColour
                Console.BackgroundColor <- backgroundColor 
                Console.Write prefixText
                Console.BackgroundColor <- previousBackgroundColour
                Console.Write " "
            | None -> ()
            Console.ForegroundColor <- colour
            Console.WriteLine text
            Console.ForegroundColor <- previousForegroundColour
#else
            ()
#endif
        | None -> ()

type ConsoleLogger () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec receiving () = async {
            let! input = inbox.Receive ()
            match input with
            | Log (source, category) ->
                log source category
                return! receiving () }
        log Source.ConsoleLogger (Info "agent instantiated -> receiving")
        receiving ())
    do agent.Error.Add (fun exn -> log Source.ConsoleLogger (AgentExn exn)) // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Log (source, category) = Log (source, category) |> agent.Post

let consoleLogger = ConsoleLogger ()

let logAgentExn source exn = (source, AgentExn exn) |> consoleLogger.Log

// Note: No ensureInstantiated function since host.fs has explicit call/s to consoleLogger.Log.
