module Client.Movement exposing (Movement, project)

import Client.Data as Data


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : Maybe Data.Load
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


project :
    { weeksToProject : Int
    , daysPerWeek : Int
    , exercises : List Data.Exercise
    }
    -> List (List Movement)
project { weeksToProject, daysPerWeek, exercises } =
    List.concatMap projectExercise exercises
        |> List.map (List.repeat daysPerWeek)
        |> List.take weeksToProject


projectExercise : Data.Exercise -> List Movement
projectExercise exercise =
    List.concatMap projectProgression exercise.progressions


projectProgression : Data.Progression -> List Movement
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
