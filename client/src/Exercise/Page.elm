module Exercise.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Global
import Html.Attributes exposing (defaultValue, type_)
import Html.Events exposing (..)
import Http
import PostgRest as PG
import Resources
import Task exposing (Task)


type alias Model =
    { exercise : Exercise
    , levels : List Level
    , draggingMovement : Maybe ( Int, Movement )
    }


type alias Exercise =
    { name : String }


type alias Level =
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
        (Task.succeed [ newLevel ]
         -- , Level [ { name = "1/2 Foam Roller Hamstring Stretch", sets = 1, reps = 3 } ]
         -- , Level [ { name = "Active Straight Leg Raise With Assist ", sets = 1, reps = 3 } ]
         -- , Level [ { name = "Foam Roll Hip Hinge", sets = 1, reps = 3 }, { name = "ViPR Hip Hinge", sets = 1, reps = 3 } ]
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
            ( { model | levels = model.levels ++ [ newLevel ] }, Cmd.none )

        StartDrag index movement ->
            ( { model | draggingMovement = Just ( index, movement ) }, Cmd.none )

        EndDrag index ->
            ( { model
                | draggingMovement = Nothing
                , levels = moveMovement index model.draggingMovement model.levels
              }
            , Cmd.none
            )


newLevel : Level
newLevel =
    Level [ { name = "Jumping jacks", reps = 15, sets = 1, load = "" } ]


moveMovement : Int -> Maybe ( Int, Movement ) -> List Level -> List Level
moveMovement newIndex dragging levels =
    case dragging of
        Nothing ->
            levels

        Just ( oldIndex, movement ) ->
            let
                _ =
                    Debug.log "stuff" ( oldIndex, newIndex )
            in
            if oldIndex /= newIndex then
                levels
            else
                List.indexedMap
                    (\index level ->
                        if index == oldIndex then
                            Level <| List.filter ((==) movement) level.movements
                        else if index == newIndex then
                            Level <| movement :: level.movements
                        else
                            level
                    )
                    levels


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
            [ concat <|
                List.intersperse
                    (divider "in 2 weeks")
                    (List.indexedMap viewLevel model.levels)
            , hr
            , level []
                [ [ button [ outline, color.primary, onClick AddLevel ] [ icon plus ]
                  ]
                ]
            ]
        ]


viewLevel : Int -> Level -> Element a Msg
viewLevel index { movements } =
    Bulma.level [ onMouseUp <| EndDrag index ] <|
        List.intersperse [ dividerVertical "or" ] <|
            List.map (viewMovement index) movements


viewMovement : Int -> Movement -> List (Element a Msg)
viewMovement index movement =
    [ box []
        [ button
            [ color.primary
            , onMouseDown <| StartDrag index movement
            ]
            [ text "DRAG" ]
        , rows
            [ textInput [ defaultValue movement.name ]
            , hr
            , level []
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
                , [ field []
                        [ label "Load"
                        , textInput [ defaultValue movement.load ]
                        ]
                  ]
                ]
            ]
        ]
    ]
