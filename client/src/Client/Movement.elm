module Client.Movement
    exposing
        ( Load(..)
        , Movement
        , Plan
        , generate
        , plan
        )

import Client.Days as Days
import Client.Exercise as Exercise
import Date
import Json.Decode as D


type alias Plan =
    { schedule : List Date.Day
    , exercises : List Exercise.Exercise
    }


plan : D.Decoder Plan
plan =
    D.map2 Plan
        (D.field "schedule" Days.days)
        (D.field "exercises" <| D.list Exercise.exercise)


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : Load
    }


type Load
    = Kgs Int
    | Lbs Int


generate : Int -> Plan -> List (List Movement)
generate weeksToProject plan =
    List.repeat weeksToProject <|
        List.repeat (List.length plan.schedule)
            { name = "Deadlift"
            , sets = 1
            , reps = 1
            , load = Kgs 1
            }
