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
    , levels : List Layer
    , draggingMovement : Maybe ( Int, Movement )
    }


type alias Exercise =
    { name : String }


type alias Layer =
    { movements : List Movement }


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , notes : String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context <| getExercise id)
        (Task.succeed
            [ Layer
                [ { name = "1/2 Foam Roller Hamstring Stretch", sets = 1, reps = 3, notes = "" }
                ]
            , Layer
                [ { name = "Active Straight Leg Raise With Assist "
                  , sets = 1
                  , reps = 3
                  , notes = ""
                  }
                ]
            , Layer
                [ { name = "Foam Roll Hip Hinge", sets = 1, reps = 3, notes = "" }
                , { name = "ViPR Hip Hinge", sets = 1, reps = 3, notes = "" }
                ]
            ]
        )
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
    | AddLevel
    | StartDrag Int Movement
    | EndDrag Int


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddLevel ->
            ( { model | levels = model.levels ++ [ newLayer ] }, Cmd.none )

        StartDrag index movement ->
            ( { model | draggingMovement = Just ( index, movement ) }, Cmd.none )

        EndDrag index ->
            ( { model
                | draggingMovement = Nothing
                , levels = moveMovement index model.draggingMovement model.levels
              }
            , Cmd.none
            )


newLayer : Layer
newLayer =
    Layer [ { name = "Jumping jacks", reps = 15, sets = 1, notes = "" } ]


moveMovement : Int -> Maybe ( Int, Movement ) -> List Layer -> List Layer
moveMovement newIndex dragging levels =
    case dragging of
        Nothing ->
            levels

        Just ( oldIndex, movement ) ->
            if oldIndex /= newIndex then
                levels
            else
                List.indexedMap
                    (\index layer ->
                        if index == oldIndex then
                            Layer <| List.filter ((==) movement) layer.movements
                        else if index == newIndex then
                            Layer <| movement :: layer.movements
                        else
                            layer
                    )
                    levels


view : Model -> Element Msg
view model =
    html div
        [ bulma.columns ]
        [ html div
            [ bulma.column ]
            [ Ui.embed <|
                List.intersperse viewInterval
                    (List.indexedMap viewLayer model.levels)
            , html hr [] []
            , html div
                [ bulma.level ]
                [ html div
                    [ bulma.levelItem ]
                    [ html button
                        [ bulma.button
                        , is.outlined
                        , is.primary
                        , onClick AddLevel
                        ]
                        [ icon "plus" []
                        , html span [] [ Ui.text "Add another step" ]
                        ]
                    ]
                ]
            ]
        ]


viewInterval =
    html div
        [ bulma.notification, has.backgroundLight ]
        [ html div
            [ bulma.level ]
            [ html div [ bulma.levelItem ] <|
                List.intersperse nbsp <|
                    [ Ui.text "progress in"
                    , numberInput "2" {- TODO -} 2
                    , Ui.text "weeks"
                    ]
            ]
        ]


viewLayer : Int -> Layer -> Element Msg
viewLayer index { movements } =
    html div [ bulma.level, onMouseUp <| EndDrag index ] <|
        List.intersperse
            (html div
                [ bulma.levelItem ]
                [ html button [ bulma.button, is.static ] [ Ui.text "OR" ] ]
            )
        <|
            List.map (viewMovement index) movements


viewMovement : Int -> Movement -> Element Msg
viewMovement index movement =
    html div
        [ bulma.levelItem ]
        [ html div
            [ bulma.box ]
            [ html div
                [ bulma.columns ]
                [ html
                    div
                    [ bulma.column ]
                    [ html div
                        [ bulma.level ]
                        [ html div
                            [ bulma.levelLeft ]
                            [ html div
                                [ bulma.field ]
                                [ html input
                                    [ bulma.input
                                    , is.medium
                                    , defaultValue movement.name
                                    ]
                                    []
                                ]
                            ]
                        , html div
                            [ bulma.levelRight ]
                            [ html button
                                [ bulma.button
                                , is.white
                                , has.textGrey
                                , cursorGrab
                                , onMouseDown <| StartDrag index movement
                                ]
                                [ icon "grip-horizontal" [] ]
                            ]
                        ]
                    , html label
                        [ bulma.checkbox ]
                        [ html input [ type_ "checkbox" ] []
                        , Ui.text " movement prep"
                        ]
                    , html hr [] []
                    , html div
                        [ bulma.level ]
                        [ html div [ bulma.levelItem ] [ numberInput "sets" movement.sets ]
                        , html div [ bulma.levelItem ] [ nbsp, Ui.text "×", nbsp ]
                        , html div [ bulma.levelItem ] [ numberInput "reps" movement.reps ]
                        ]
                    , html div
                        [ bulma.field ]
                        [ html textarea
                            [ bulma.textarea
                            , placeholder "30 kgs, sitting down, other notes..."
                            , Html.Attributes.rows 2
                            ]
                            []
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
            ]
            []
        ]


cursorGrab : Attribute msg
cursorGrab =
    Html.Attributes.style
        [ ( "cursor", "grab" )
        , ( "cursor", "-webkit-grab" )
        ]
