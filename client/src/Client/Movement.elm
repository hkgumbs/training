module Client.Movement
    exposing
        ( Movement
        , Plan
        , plan
        , project
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
    , load : Maybe Exercise.Load
    , rest : Int
    , isProgression : Bool
    }



--  { "name": "1/2 Foam Roller Hamstring Stretch"
--  , "sets": 1
--  , "reps": 15
--  , "load": null
--  , "rest": 0
--  , "progression_rate": 4
--  , "minimum_fms": 1
--  , "option": "movement_prep"
--  }


project : Int -> Plan -> List (List Movement)
project weeksToProject plan =
    List.concatMap projectExercise plan.exercises
        |> List.map (List.repeat (List.length plan.schedule))
        |> List.take weeksToProject


projectExercise : Exercise.Exercise -> List Movement
projectExercise exercise =
    List.concatMap projectProgression exercise.progressions


projectProgression : Exercise.Progression -> List Movement
projectProgression progression =
    case
        List.repeat progression.progressionRate
            { name = progression.name
            , sets = progression.sets
            , reps = progression.reps
            , load = progression.load
            , rest = progression.rest
            , isProgression = False
            }
    of
        first :: rest ->
            { first | isProgression = True } :: rest

        original ->
            original
