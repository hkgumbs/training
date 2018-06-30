module Exercise.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Global
import Html.Attributes exposing (defaultValue, type_)
import Http
import PostgRest as PG
import Resources
import Task exposing (Task)


type alias Model =
    { exercise : Exercise
    , levels : List Level
    }


type alias Exercise =
    { name : String }


type Level
    = Level (List Movement)


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map2 Model
        (pg context <| getExercise id)
        (Task.succeed
            [ Level [ { name = "1/2 Foam Roller Hamstring Stretch", sets = 1, reps = 3 } ]
            , Level [ { name = "Active Straight Leg Raise With Assist ", sets = 1, reps = 3 } ]
            , Level [ { name = "Foam Roll Hip Hinge", sets = 1, reps = 3 }, { name = "ViPR Hip Hinge", sets = 1, reps = 3 } ]
            ]
        )


{-| TODO: move to Global
-}
pg : Global.Context -> PG.Request a -> Task Global.Error a
pg { auth, dbapi } =
    PG.toHttpRequest { timeout = Nothing, token = auth, url = dbapi }
        >> Http.toTask
        >> Task.mapError Global.httpError


getExercise : String -> PG.Request Exercise
getExercise id =
    PG.readOne Resources.exercise
        { select =
            PG.map Exercise
                (PG.field .name)
        , where_ = PG.eq id .id
        }


type Msg
    = NoOp


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Element a Msg
view model =
    columns
        [ column [ width.is3 ]
            [ notification [ background.light ]
                [ field []
                    [ -- label "Name"
                      title [ textInput [ defaultValue model.exercise.name ] ]
                    ]
                ]
            , tile [] [ parent [ button [ color.primary ] [ bold "Save" ] ] ]
            ]
        , column [] <|
            List.intersperse
                (divider <| "in 2 weeks")
                (List.map viewLevel model.levels)
        ]


viewLevel : Level -> Element a Msg
viewLevel level =
    case level of
        Level movements ->
            Bulma.level <|
                List.intersperse [ dividerVertical "or" ] <|
                    List.map (\m -> [ viewMovement m ]) movements


viewMovement : Movement -> Element a Msg
viewMovement movement =
    box []
        [ rows
            [ textInput [ defaultValue movement.name ]
            , hr
            , level
                [ [ field []
                        [ label "Sets"
                        , textInput
                            [ type_ "number"
                            , defaultValue <| toString movement.sets
                            ]
                        ]
                  ]
                , [ field []
                        [ label "Reps"
                        , textInput
                            [ type_ "number"
                            , defaultValue <| toString movement.reps
                            ]
                        ]
                  ]
                ]
            ]
        ]
