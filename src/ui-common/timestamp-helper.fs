module Aornota.UI.Common.TimestampHelper

open Aornota.Common.UnitsOfMeasure

open System

let [<Literal>] private SHOULD_NEVER_HAPPEN = "SHOULD NEVER HAPPEN"
let [<Literal>] private MONTHS_PER_YEAR = 12

let ago (timestamp:DateTime) =
    let dayName dayOfWeek =
        match dayOfWeek with
        | DayOfWeek.Monday -> "Monday" | DayOfWeek.Tuesday -> "Tuesday" | DayOfWeek.Wednesday -> "Wednesday" | DayOfWeek.Thursday -> "Thursday"
        | DayOfWeek.Friday -> "Friday" | DayOfWeek.Saturday -> "Saturday" | DayOfWeek.Sunday -> "Sunday" | _ -> SHOULD_NEVER_HAPPEN
    let suffix day = match day with | 1 | 21 | 31 -> "st" | 2 | 22 -> "nd" | 3 | 23 -> "rd" | _ -> "th"
    let monthName month =
        match month with
        | 1 -> "January" | 2 -> "February" | 3 -> "March" | 4 -> "April" | 5 -> "May" | 6 -> "June" | 7 -> "July" | 8 -> "August" | 9 -> "September" | 10 -> "October"
        | 11 -> "November" | 12 -> "December" | _ -> SHOULD_NEVER_HAPPEN
    let now = DateTime.Now
    let elapsed = now - timestamp
    let sameTimeToday = DateTime (now.Year, now.Month, now.Day, timestamp.Hour, timestamp.Minute, timestamp.Second)   
    let elapsedDays = round (sameTimeToday - timestamp).TotalDays // note: use 'round' function since TotalDays could be something like 0.9999... (i.e. even when use of "same time" means that we'd expect it to be 1.0)
    let isFuture = timestamp > now && (timestamp - now).TotalSeconds > 10. // note: a bit of leeway in case the system that provided the timestamp is "running fast"
    match isFuture, (now.Year - timestamp.Year, now.Month - timestamp.Month, elapsedDays, elapsed.TotalHours, elapsed.TotalMinutes, elapsed.TotalSeconds) with
    | true, _ -> sprintf "%s %i%s %s %i" (dayName timestamp.DayOfWeek) timestamp.Day (suffix timestamp.Day) (monthName timestamp.Month) timestamp.Year // note: timestamp expected to be in the past - but do something sensible if not
    | false, (_, _, days, _, _, _) when floor days = 1. -> sprintf "yesterday (%s)" (dayName timestamp.DayOfWeek)
    | false, (_, _, _, _, _, seconds) when floor seconds <= 0. -> "just now"
    | false, (_, _, _, _, _, seconds) when floor seconds = 1. -> "1 second ago"
    | false, (_, _, _, _, _, seconds) when floor seconds < float SECONDS_PER_MINUTE -> sprintf "%i seconds ago" (int seconds)
    | false, (_, _, _, _, minutes, _) when floor minutes = 1. -> "1 minute ago"
    | false, (_, _, _, _, minutes, _) when floor minutes < float MINUTES_PER_HOUR -> sprintf "%i minutes ago" (int minutes)
    | false, (_, _, _, hours, _, _) when floor hours = 1. -> "1 hour ago"
    | false, (_, _, _, hours, _, _) when floor hours < float HOURS_PER_DAY -> sprintf "%i hours ago" (int hours)
    | false, (_, _, days, _, _, _) when floor days < 7. -> sprintf "%i days ago (%s)" (int days) (dayName timestamp.DayOfWeek)
    | false, (years, months, days, _, _, _) when years = 0 && months = 0 ->
        sprintf "%i days ago (%s %i%s)" (int days) (dayName timestamp.DayOfWeek) timestamp.Day (suffix timestamp.Day)
    | false, (years, months, _, _, _, _) when ((years * MONTHS_PER_YEAR) + months) = 1 ->
        sprintf "last month (%i%s %s)" timestamp.Day (suffix timestamp.Day) (monthName timestamp.Month)
    | false, (years, months, _, _, _, _) when ((years * MONTHS_PER_YEAR) + months) < MONTHS_PER_YEAR ->
        sprintf "%i months ago (%s)" ((years * MONTHS_PER_YEAR) + months) (monthName timestamp.Month)
    | false, (years, _, _, _, _, _) when years = 1 -> sprintf "last year (%s %i)" (monthName timestamp.Month) timestamp.Year
    | false, (years, _, _, _, _, _) when years > 1 -> sprintf "%i years ago (%i)" years timestamp.Year
    | _ -> SHOULD_NEVER_HAPPEN

let expiresIn (timestamp:DateTime) = // TODO-NMB-LOW: Make more generic?...
    let now = DateTime.Now
    let elapsed = timestamp - now
    let isPast = timestamp < now && (now - timestamp).TotalSeconds > 10. // note: a bit of leeway in case the system that provided the timestamp is "running slow"
    match isPast, (elapsed.TotalMinutes, elapsed.TotalSeconds) with
    | true, _ -> "has expired" // note: timestamp expected to be in the future - but do something sensible if not       
    | false, (_, seconds) when floor seconds <= 0. -> "is about to expire"
    | false, (_, seconds) when floor seconds = 1. -> "expires in 1 second"
    | false, (_, seconds) when floor seconds < float SECONDS_PER_MINUTE -> sprintf "expires in %i seconds" (int seconds)
    | false, (minutes, _) when floor minutes = 1. -> "expires in 1 minute"
    | false, (minutes, _) when floor minutes < float MINUTES_PER_HOUR -> sprintf "expires in %i minutes" (int minutes)
    | _ -> "does not expire for at least 1 hour"
