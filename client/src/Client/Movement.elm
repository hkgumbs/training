module Client.Movement
    exposing
        ( Load(..)
        , Movement
        , Plan
        , Seed
        , generate
        , plan
        )

import Client.Days as Days
import Date
import Json.Decode as D


type alias Plan =
    { seed : Seed
    , schedule : List Date.Day
    }


type Seed
    = Seed


type alias Movement =
    { sets : Int
    , reps : Int
    , load : Load
    }


type Load
    = Kgs Int
    | Lbs Int



-- JSON


plan : D.Decoder Plan
plan =
    D.map2 Plan
        (D.succeed Seed)
        (D.field "schedule" Days.days)



-- VIEW


generate : Int -> Plan -> List (List Movement)
generate weeksToProject plan =
    List.repeat weeksToProject <|
        List.repeat (List.length plan.schedule)
            { sets = 1
            , reps = 1
            , load = Kgs 1
            }
