module TestData exposing (..)


client : String
client =
    """
{ "name": "Mrs. Incredible"
, "plan":
    { "schedule": "MWF"
    , "exercises":
        [ { "name": "Deadlift"
          , "type": "HIP_DOMINENT"
          , "plane_of_motion": "SAGGITAL"
          , "joint_action": "MULTI"
          , "progressions":
              [ { "name": "1/2 Foam Roller Hamstring Stretch"
                , "sets": 1
                , "reps": 15
                , "load": null
                , "rest": 0
                , "progression_rate": 4
                , "minimum_fms": 1
                , "option": "MOVEMENT_PREP"
                }
              , { "name": "Active Straight Leg Raise With Assist "
                , "sets": 1
                , "reps": 12
                , "load": null
                , "rest": 0
                , "progression_rate": 3
                , "minimum_fms": 1
                , "option": "MOVEMENT_PREP"
                }
              , { "name": "Active Straight Leg Raise Without Assist "
                , "sets": 1
                , "reps": 12
                , "load": null
                , "rest": 0
                , "progression_rate": 2
                , "minimum_fms": 1
                , "option": "MOVEMENT_PREP"
                }
              , { "name": "Glute Bridges on Back"
                , "sets": 1
                , "reps": 15
                , "load": null
                , "rest": 0
                , "progression_rate": 2
                , "minimum_fms": 1
                , "option": "MOVEMENT_PREP"
                }
              , { "name": "Foam Roll Hip Hinge"
                , "sets": 2
                , "reps": 20
                , "load": null
                , "rest": 30
                , "progression_rate": 2
                , "minimum_fms": 1
                , "option": "RESISTANCE_TRAINING"
                }
              ]
          }
        ]
    }
}"""
