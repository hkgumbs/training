module Days exposing (list, toString)

import Date
import Json.Decode as D


toString : Date.Day -> String
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


list : List Date.Day
list =
    [ Date.Mon
    , Date.Tue
    , Date.Wed
    , Date.Thu
    , Date.Fri
    , Date.Sat
    , Date.Sun
    ]
