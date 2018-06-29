module Resources exposing (..)

import Json.Decode as D
import Json.Encode as E
import PostgRest as PG


type Client
    = Client


client =
    PG.schema "clients"
        Client
        { id = PG.string "id"
        , name = PG.string "name"
        }


type Exercise
    = Exercise


exercise =
    PG.schema "exercises"
        Exercise
        { id = PG.string "id"
        , name = PG.string "name"
        , movements = PG.hasMany "exercise_id" Movement
        , features = PG.hasMany "exercise_id" Feature
        }


type Movement
    = Movement


type Load
    = Lbs Int
    | Kgs Int


movement =
    PG.schema "movements"
        Movement
        -- TODO: load
        { id = PG.string "id"
        , name = PG.string "name"
        , sets = PG.int "sets"
        , reps = PG.int "reps"
        , load =
            PG.attribute
                { decoder = D.succeed <| Just (Lbs 3)
                , encoder = \_ -> Debug.crash {- TODO -} ""
                , urlEncoder = \_ -> Debug.crash {- TODO -} ""
                }
                "load"
        , rest = PG.int "rest"
        , exercise = PG.hasOne "exercise_id" Exercise
        }


type Feature
    = Feature


exerciseFeature =
    PG.schema "exercise_features"
        Feature
        { id = PG.string "id"
        , value = PG.string "value"
        }
