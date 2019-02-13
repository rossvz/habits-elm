module Utils exposing (monthToString, timeTilEndOfDay, timeTilStartOfDay, timeTilStartOfWeek)

import Time


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


timeTilStartOfDay : Time.Posix -> Time.Zone -> Int
timeTilStartOfDay time zone =
    let
        hour =
            Time.toHour zone time

        min =
            Time.toMinute zone time

        sec =
            Time.toSecond zone time

        ms =
            Time.toMillis zone time

        hourToMs =
            60 * 60 * 1000

        minToMs =
            60 * 1000

        secToMs =
            1000
    in
    (hour * hourToMs)
        + (min * minToMs)
        + (sec * secToMs)
        + ms


timeTilEndOfDay : Time.Posix -> Time.Zone -> Int
timeTilEndOfDay time zone =
    let
        hour =
            24 - Time.toHour zone time

        min =
            59 - Time.toMinute zone time

        sec =
            59 - Time.toSecond zone time

        ms =
            1000 - Time.toMillis zone time

        hourToMs =
            60 * 60 * 1000

        minToMs =
            60 * 1000

        secToMs =
            1000
    in
    (hour * hourToMs)
        + (min * minToMs)
        + (sec * secToMs)
        + ms


timeTilStartOfWeek : Time.Posix -> Time.Zone -> Int
timeTilStartOfWeek time zone =
    let
        dayStart =
            timeTilStartOfDay time zone

        msInDay days =
            days * (24 * 60 * 60 * 1000)
    in
    case Time.toWeekday zone time of
        Time.Mon ->
            dayStart

        Time.Tue ->
            dayStart + msInDay 1

        Time.Wed ->
            dayStart + msInDay 2

        Time.Thu ->
            dayStart + msInDay 3

        Time.Fri ->
            dayStart + msInDay 4

        Time.Sat ->
            dayStart + msInDay 5

        Time.Sun ->
            dayStart + msInDay 6
