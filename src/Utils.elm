module Utils exposing (monthToString, timeTilStartOfDay)

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
timeTilStartOfDay posix zone =
    let
        hour =
            Time.toHour zone posix

        min =
            Time.toMinute zone posix

        sec =
            Time.toSecond zone posix

        ms =
            Time.toMillis zone posix

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
