module Client.Days exposing (days, mondayFirst)

import Date
import Json.Decode as D


days : D.Decoder (List Date.Day)
days =
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


mondayFirst : Date.Day -> Int
mondayFirst day =
    case day of
        Date.Mon ->
            1

        Date.Tue ->
            2

        Date.Wed ->
            3

        Date.Thu ->
            4

        Date.Fri ->
            5

        Date.Sat ->
            6

        Date.Sun ->
            7
