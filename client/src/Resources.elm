module Resources exposing (..)

import PostgRest as PG


type Client
    = Client


client =
    PG.resource Client
        "clients"
        { id = PG.string "id"
        , name = PG.string "movements"
        }


type Exercise
    = Exercise


exercise =
    PG.resource Exercise
        "exercises"
        { id = PG.string "id"
        , name = PG.string "name"
        , movements = PG.hasMany Movement
        , features = PG.hasMany Feature
        }


type Movement
    = Movement


movement =
    PG.resource Movement
        "movements"
        -- TODO: load
        { id = PG.string "id"
        , sets = PG.int "sets"
        , reps = PG.int "reps"
        , rest = PG.int "rest"
        , exercise = PG.hasOne Exercise
        }


type Feature
    = Feature


exerciseFeature =
    PG.resource Feature
        "exercise_features"
        { id = PG.string "id"
        , value = PG.string "value"
        }
