module DateUtils exposing (..)

import Time


oneDay =
    -- A day's worth of milliseconds.
    24 * 3600 * 1000


oneHour =
    3600 * 1000


fourWeeks =
    oneDay * 28


oneYear =
    oneDay * 365


oneYearBefore : Time.Posix -> Time.Posix
oneYearBefore now =
    Time.millisToPosix (Time.posixToMillis now - oneYear)


mostRecent : Time.Posix -> Time.Posix -> Time.Posix
mostRecent a b =
    Time.millisToPosix <| max (Time.posixToMillis a) (Time.posixToMillis b)


monthNumberInfix : Time.Month -> String
monthNumberInfix m =
    case m of
        Time.Jan ->
            "-01-"

        Time.Feb ->
            "-02-"

        Time.Mar ->
            "-03-"

        Time.Apr ->
            "-04-"

        Time.May ->
            "-05-"

        Time.Jun ->
            "-06-"

        Time.Jul ->
            "-07-"

        Time.Aug ->
            "-08-"

        Time.Sep ->
            "-09-"

        Time.Oct ->
            "-10-"

        Time.Nov ->
            "-11-"

        Time.Dec ->
            "-12-"


dateFromPosix p =
    let
        year =
            Time.toYear Time.utc p

        month =
            Time.toMonth Time.utc p

        day =
            Time.toDay Time.utc p
    in
    String.fromInt year
        ++ monthNumberInfix month
        ++ (if day < 10 then
                "0"

            else
                ""
           )
        ++ String.fromInt day


isWorkday : Time.Posix -> Bool
isWorkday p =
    case Time.toWeekday Time.utc p of
        Time.Mon ->
            True

        Time.Tue ->
            True

        Time.Wed ->
            True

        Time.Thu ->
            True

        Time.Fri ->
            True

        Time.Sat ->
            False

        Time.Sun ->
            False


olderThanFourWeeks : Time.Posix -> Time.Posix -> Bool
olderThanFourWeeks now candidate =
    Time.posixToMillis now - Time.posixToMillis candidate > fourWeeks


withinLast30Days : Time.Posix -> Time.Posix -> Bool
withinLast30Days now candidate =
    Time.posixToMillis now - Time.posixToMillis candidate <= 30 * oneDay
