module Days exposing (list, offsetFrom, toString)

import Time.Date as Date


offsetFrom : Date.Date -> Date.Weekday -> Int -> Date.Date
offsetFrom today day weeksAway =
    Date.addDays ((7 * weeksAway) + dayOffset today day) today


dayOffset : Date.Date -> Date.Weekday -> Int
dayOffset today day =
    indexOf day - indexOf (Date.weekday today)


indexOf : Date.Weekday -> Int
indexOf day =
    case day of
        Date.Mon ->
            0

        Date.Tue ->
            1

        Date.Wed ->
            2

        Date.Thu ->
            3

        Date.Fri ->
            4

        Date.Sat ->
            5

        Date.Sun ->
            6


toString : Date.Weekday -> String
toString day =
    case day of
        Date.Mon ->
            "Monday"

        Date.Tue ->
            "Tuesday"

        Date.Wed ->
            "Wednesday"

        Date.Thu ->
            "Thursday"

        Date.Fri ->
            "Friday"

        Date.Sat ->
            "Saturday"

        Date.Sun ->
            "Sunday"


list : List Date.Weekday
list =
    [ Date.Mon
    , Date.Tue
    , Date.Wed
    , Date.Thu
    , Date.Fri
    , Date.Sat
    , Date.Sun
    ]
