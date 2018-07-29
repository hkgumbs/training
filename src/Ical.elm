module Ical exposing (Event, generate)

import Time.Date exposing (Date)


type alias Event =
    { date : Date
    , title : String
    , details : String
    }


generate : Date -> List Event -> String
generate today events =
    let
        prefix =
            [ "BEGIN:VCALENDAR"
            , "VERSION:2.0"
            , "PRODID:-//T//Training Calendar//EN"
            , "CALSCALE:GREGORIAN"
            , "METHOD:PUBLISH"
            ]

        suffix =
            [ "END:VCALENDAR"
            ]
    in
    String.join "\x0D\n" <|
        prefix
            ++ (List.concat <| List.indexedMap (generateEvent today) events)
            ++ suffix


generateEvent : Date -> Int -> Event -> List String
generateEvent today unique { title, details, date } =
    [ "BEGIN:VEVENT"
    , "DTSTART:" ++ iso date
    , "DTEND:" ++ iso date
    , "DTSTAMP:" ++ iso today
    , "UID:id-" ++ toString unique ++ "@training.app"
    , "CREATED:" ++ iso today
    , "DESCRIPTION:" ++ details
    , "LAST-MODIFIED:" ++ iso today
    , "SEQUENCE:0"
    , "STATUS:CONFIRMED"
    , "SUMMARY:" ++ title
    , "TRANSP:OPAQUE"
    , "END:VEVENT"
    ]


iso : Date -> String
iso date =
    String.concat
        [ toString <| Time.Date.year date
        , paddedTwoPlaces <| Time.Date.month date
        , paddedTwoPlaces <| Time.Date.day date
        , "T000000Z"
        ]


paddedTwoPlaces : Int -> String
paddedTwoPlaces =
    toString >> String.padLeft 2 '0'
