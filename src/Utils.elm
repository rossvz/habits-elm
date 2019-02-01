module Utils exposing (monthToString, timeTilEndOfDay, timeTilStartOfDay)

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
