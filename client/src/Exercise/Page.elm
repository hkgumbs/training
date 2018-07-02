module Exercise.Page exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import PostgRest as PG
import Resources
import Task exposing (Task)
import Ui exposing (..)


type alias Model =
    { exercise : Exercise
    , steps : List Step
    , draggingMovement : Maybe ( Int, Int )
    }


type alias Exercise =
    { name : String }


type Step
    = Movements (List Movement)
    | Interval Int


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context (getExercise id))
        (Task.succeed [ newMovements ])
        (Task.succeed Nothing)


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
    | AddStep


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddStep ->
            ( { model | steps = addNewSteps model.steps }, Cmd.none )


addNewSteps : List Step -> List Step
addNewSteps old =
    old ++ [ Interval 2, newMovements ]


newMovements : Step
newMovements =
    Movements [ { name = "Jumping jacks", reps = 15, sets = 1, load = "" } ]


view : Model -> Element Msg
view model =
    html div
        [ bulma.columns ]
        [ html div
            [ bulma.column ]
            [ viewHeader model.exercise
            , concat <| List.map viewStep model.steps
            , viewFooter
            ]
        ]


viewHeader : Exercise -> Element Msg
viewHeader { name } =
    concat
        [ html div
            [ bulma.level ]
            [ html div
                [ bulma.levelLeft ]
                [ html div
                    [ bulma.levelItem ]
                    [ html h1 [ bulma.title ] [ string name ]
                    ]
                ]
            , html div
                [ bulma.levelRight ]
                [ html div
                    [ bulma.levelItem ]
                    [ html button
                        [ bulma.button, is.primary ]
                        [ icon "check"
                        , html span [] [ string "Save" ]
                        ]
                    ]
                ]
            ]
        , html hr [] []
        ]


viewStep : Step -> Element Msg
viewStep step =
    case step of
        Movements movements ->
            List.map viewMovement movements
                |> List.intersperse viewOr
                |> html div [ bulma.level ]

        Interval weeks ->
            viewInterval weeks


viewInterval : Int -> Element msg
viewInterval weeks =
    html div
        [ bulma.notification, has.backgroundLight ]
        [ html div
            [ bulma.level ]
            [ html div [ bulma.levelItem ] <|
                List.intersperse nbsp <|
                    [ string "progress in "
                    , numberInput "" weeks
                    , string " weeks"
                    ]
            ]
        ]


viewOr : Element msg
viewOr =
    html div
        [ bulma.levelItem ]
        [ html button [ bulma.button, is.static ] [ string "OR" ] ]


viewMovement : Movement -> Element Msg
viewMovement movement =
    html div
        [ bulma.levelItem ]
        [ html div
            [ bulma.box, cursorGrab, draggable "true" ]
            [ viewMovementHeader
            , viewMovementName movement.name
            , viewMovementDetails movement.sets movement.reps movement.load
            ]
        ]


viewMovementHeader : Element Msg
viewMovementHeader =
    html div
        [ bulma.field ]
        [ html div
            [ bulma.control ]
            [ html div
                [ bulma.select ]
                [ html select
                    []
                    [ html option [] [ string "Movement Prep" ]
                    , html option [] [ string "Resistance Training" ]
                    ]
                ]
            ]
        ]


viewMovementName : String -> Element msg
viewMovementName name =
    html div
        [ bulma.field ]
        [ html div
            [ bulma.control ]
            [ html input
                [ bulma.input, is.medium, type_ "text", defaultValue name ]
                []
            ]
        ]


viewMovementDetails : Int -> Int -> String -> Element msg
viewMovementDetails sets reps load =
    html div
        [ bulma.level ]
        [ html div [ bulma.levelItem ] [ numberInput "sets" sets ]
        , html div [ bulma.levelItem ] [ nbsp, string "×", nbsp ]
        , html div [ bulma.levelItem ] [ numberInput "reps" reps ]
        , html div [ bulma.levelItem ] [ nbsp, string "@", nbsp ]
        , html div [ bulma.levelItem ] [ loadInput load ]
        ]


viewFooter : Element Msg
viewFooter =
    concat
        [ html hr [] []
        , html div
            [ bulma.level ]
            [ html div
                [ bulma.levelItem ]
                [ html div
                    [ bulma.buttons ]
                    [ html button
                        [ bulma.button
                        , is.outlined
                        , is.primary
                        , onClick AddStep
                        ]
                        [ icon "tasks"
                        , html span [] [ string "Add progression" ]
                        ]
                    ]
                ]
            ]
        ]


numberInput : String -> Int -> Element msg
numberInput name n =
    html div
        [ bulma.field ]
        [ html input
            [ bulma.input
            , type_ "number"
            , placeholder name
            , defaultValue (toString n)
            , style [ ( "width", "4rem" ) ]
            ]
            []
        ]


loadInput : String -> Element msg
loadInput content =
    html div
        [ bulma.field ]
        [ html input
            [ bulma.input
            , type_ "text"
            , placeholder "loads"
            , defaultValue content
            , style [ ( "width", "10rem" ) ]
            ]
            []
        ]


cursorGrab : Attribute msg
cursorGrab =
    style
        [ ( "cursor", "grab" )
        , ( "cursor", "-webkit-grab" )
        ]
