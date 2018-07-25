module Days exposing (decoder, toString)

import Date
import Json.Decode as D


decoder : D.Decoder (List Date.Day)
decoder =
    D.andThen (String.toList >> parseDay []) D.string


parseDay : List Date.Day -> List Char -> D.Decoder (List Date.Day)
parseDay acc chars =
    case chars of
        [] ->
            D.succeed (List.reverse acc)

        'M' :: rest ->
            parseDay (Date.Mon :: acc) rest

        'T' :: 'u' :: rest ->
            parseDay (Date.Tue :: acc) rest

        'W' :: rest ->
            parseDay (Date.Wed :: acc) rest

        'T' :: 'h' :: rest ->
            parseDay (Date.Thu :: acc) rest

        'F' :: rest ->
            parseDay (Date.Fri :: acc) rest

        'S' :: 'a' :: rest ->
            parseDay (Date.Sat :: acc) rest

        'S' :: 'u' :: rest ->
            parseDay (Date.Sun :: acc) rest

        _ ->
            D.fail <| "`" ++ String.fromList chars ++ "` is not a valid DAY"


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
