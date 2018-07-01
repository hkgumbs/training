module Exercise.Page exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Attributes exposing (defaultValue, type_)
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
    , load : String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context <| getExercise id)
        (Task.succeed [ newLayer ]
         -- , Layer [ { name = "1/2 Foam Roller Hamstring Stretch", sets = 1, reps = 3 } ]
         -- , Layer [ { name = "Active Straight Leg Raise With Assist ", sets = 1, reps = 3 } ]
         -- , Layer [ { name = "Foam Roll Hip Hinge", sets = 1, reps = 3 }, { name = "ViPR Hip Hinge", sets = 1, reps = 3 } ]
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
    Layer [ { name = "Jumping jacks", reps = 15, sets = 1, load = "" } ]


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
    html span [] [ Ui.text "__EXERCISE__" ]



--     rows
--         [ concat <|
--             List.intersperse
--                 (notification
--                     [ background.light ]
--                     [ level []
--                         [ levelItem [] <|
--                             List.intersperse nbsp <|
--                                 [ text "progress in"
--                                 , numberInput "2" {- TODO -} 2
--                                 , text "weeks"
--                                 ]
--                         ]
--                     ]
--                 )
--                 (List.indexedMap viewLayer model.levels)
--         , hr
--         , level []
--             [ levelItem []
--                 [ button
--                     [ outline, color.primary, onClick AddLevel ]
--                     [ icon plus ]
--                 ]
--             ]
--         ]
--
--
-- viewLayer : Int -> Layer -> Element Msg
-- viewLayer index { movements } =
--     level [ onMouseUp <| EndDrag index ] <|
--         List.intersperse (levelItem [] [ button [ static ] [ text "or" ] ]) <|
--             List.map (viewMovement index) movements
--
--
-- viewMovement : Int -> Movement -> Element Msg
-- viewMovement index movement =
--     levelItem []
--         [ box []
--             [ rows
--                 [ level []
--                     [ levelLeft []
--                         [ field []
--                             [ textInput
--                                 [ input.medium
--                                 , defaultValue movement.name
--                                 ]
--                             ]
--                         ]
--                     , levelRight []
--                         [ button
--                             [ color.white
--                             , textColor.grey
--                             , cursorGrab
--                             , onMouseDown <| StartDrag index movement
--                             ]
--                             [ icon handle ]
--                         ]
--                     ]
--                 , hr
--                 , level []
--                     [ levelItem [] [ numberInput "sets" movement.sets ]
--                     , levelItem [] [ nbsp, text "×", nbsp ]
--                     , levelItem [] [ numberInput "reps" movement.reps ]
--                     ]
--                 , field []
--                     [ textarea
--                         [ Html.Attributes.rows 2
--                         , placeholder "30 kgs, sitting down, other notes..."
--                         ]
--                     ]
--                 ]
--             ]
--         ]
--
--
-- numberInput : String -> Int -> Element msg
-- numberInput name n =
--     field []
--         [ textInput
--             [ type_ "number"
--             , placeholder name
--             ]
--         ]
--
--
-- cursorGrab : Attribute msg
-- cursorGrab =
--     Html.Attributes.style
--         [ ( "cursor", "grab" )
--         , ( "cursor", "-webkit-grab" )
--         ]
