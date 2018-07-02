module Client.Page exposing (Model, Msg, init, update, view)

import Client.Days as Days
import Date
import Global
import Html exposing (..)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import PostgRest as PG
import Resources
import Task exposing (Task)
import TestData
import Ui exposing (..)


type alias Model =
    { weeksToProject : Int
    , client : Client
    , exercises : List Exercise
    }


type alias Client =
    { name : String
    , schedule : List Date.Day
    }


type alias Exercise =
    { name : String
    , features : List String
    , progressions : List Progression
    }


type alias Progression =
    { rate : Int
    , from : Movement
    , to : Movement
    }


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : Maybe Resources.Load
    , rest : Int
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map2 (Model 6)
        (getClient context)
        (pg context getExercises)


{-| TODO: move to Global
-}
pg : Global.Context -> PG.Request a -> Task Global.Error a
pg { auth, dbapi } =
    PG.toHttpRequest { timeout = Nothing, token = auth, url = dbapi }
        >> Http.toTask
        >> Task.mapError Global.httpError


getClient : Global.Context -> Task Global.Error Client
getClient context =
    let
        decoder =
            D.map2 Client
                (D.field "name" D.string)
                (D.field "schedule" Days.decoder)
    in
    case D.decodeString decoder TestData.client of
        Ok client ->
            Task.succeed client

        Err _ ->
            Debug.crash {- TODO -} ""


getExercises : PG.Request (List Exercise)
getExercises =
    let
        movement =
            PG.map5 Movement
                (PG.field .name)
                (PG.field .sets)
                (PG.field .reps)
                (PG.field .load)
                (PG.field .rest)
    in
    PG.readAll Resources.exercise
        (PG.map3 Exercise
            (PG.field .name)
            (PG.embedAll .features Resources.exerciseFeature (PG.field .value))
            (PG.embedAll .progressions
                Resources.progression
                (PG.map3 Progression
                    (PG.field .rate)
                    (PG.embedOne .fromMovement Resources.movement movement)
                    (PG.embedOne .toMovement Resources.movement movement)
                )
            )
        )


type Msg
    = NoOp
    | SetSchedule Date.Day
    | LoadMore


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSchedule day ->
            ( { model | client = adjustSchedule day model.client }, Cmd.none )

        LoadMore ->
            ( { model | weeksToProject = 6 + model.weeksToProject }, Cmd.none )


adjustSchedule : Date.Day -> Client -> Client
adjustSchedule day client =
    let
        schedule =
            if List.member day client.schedule then
                List.filter ((/=) day) client.schedule
            else
                day :: client.schedule
    in
    { client | schedule = schedule }


view : Model -> Element Msg
view model =
    html div
        [ bulma.tile, is.ancestor ]
        [ html div
            [ bulma.tile, is.vertical ]
            [ html div [ bulma.tile ] <|
                [ viewSidebar model.client model.exercises
                , viewSchedule model.weeksToProject model.client model.exercises
                ]
            ]
        ]


viewSidebar : Client -> List Exercise -> Element Msg
viewSidebar client exercises =
    html div
        [ bulma.tile, is.four ]
        [ html div
            [ is.parent ]
            [ html div
                [ bulma.notification, is.white ]
                [ html h1 [ bulma.title ] [ string client.name ]
                , html label [ bulma.label ] [ string "Schedule" ]
                , html div
                    [ bulma.field, has.addons ]
                    [ dayOfWeek client.schedule Date.Mon
                    , dayOfWeek client.schedule Date.Tue
                    , dayOfWeek client.schedule Date.Wed
                    , dayOfWeek client.schedule Date.Thu
                    , dayOfWeek client.schedule Date.Fri
                    , dayOfWeek client.schedule Date.Sat
                    , dayOfWeek client.schedule Date.Sun
                    ]
                , html hr [] []
                , html div
                    [ bulma.field ]
                    [ html label [ bulma.label ] [ string "Exercises" ]
                    , html div
                        [ bulma.control, has.iconsLeft ]
                        [ icon "search"
                        , html input
                            [ bulma.input, placeholder "Add an exercise..." ]
                            []
                        ]
                    ]
                , html div
                    [ bulma.columns ]
                    [ html div [ bulma.column ] <| List.map viewExercise exercises ]
                ]
            ]
        ]


viewExercise : Exercise -> Element Msg
viewExercise exercise =
    html div
        [ bulma.box ]
        [ html div
            [ bulma.level ]
            [ html div
                [ bulma.levelLeft ]
                [ html h2 [ bulma.subtitle ] [ string exercise.name ] ]
            , html div
                [ bulma.levelRight ]
                [ html button
                    [ bulma.button
                    , is.danger
                    , is.outlined
                    ]
                    [ icon "minus-circle" ]
                ]
            ]
        , html div [ bulma.tags ] <|
            List.map
                (\name -> html div [ bulma.tag ] [ string name ])
                exercise.features
        ]


dayOfWeek : List Date.Day -> Date.Day -> Element Msg
dayOfWeek schedule day =
    html div
        [ bulma.control ]
        [ html button
            (scheduleAttrs schedule day
                ++ [ bulma.button, onClick <| SetSchedule day ]
            )
            [ string <| Days.toString day ]
        ]


viewSchedule : Int -> Client -> List Exercise -> Element Msg
viewSchedule weeksToProject client exercises =
    let
        projection =
            generatePlan
                { exercises = exercises
                , weeksToProject = weeksToProject
                , daysPerWeek = List.length client.schedule
                }
    in
    html div [ bulma.tile, is.vertical ] <|
        List.map viewWeek projection
            ++ [ html div
                    [ bulma.tile, is.parent ]
                    [ html button
                        [ bulma.button, bulma.tile, is.child, onClick LoadMore ]
                        [ string "Load more" ]
                    ]
               ]


viewWeek : { week : List { workout : List Movement } } -> Element Msg
viewWeek { week } =
    html div [ bulma.tile ] <| List.map viewWorkout week


viewWorkout : { workout : List Movement } -> Element Msg
viewWorkout { workout } =
    html div
        [ bulma.tile, is.parent ]
        [ html div
            [ bulma.notification, bulma.tile, is.child, is.light ]
            (List.map viewMovement workout)
        ]


viewMovement : Movement -> Element Msg
viewMovement movement =
    concat
        [ html div
            [ bulma.columns ]
            [ html div
                [ bulma.column ]
                [ html h2 [ bulma.subtitle ] [ string movement.name ]
                , html div
                    [ bulma.level ]
                    [ html div
                        [ bulma.levelItem ]
                        [ viewSetsReps movement.sets movement.reps ]
                    , html div [ bulma.levelItem ] [ viewLoad movement.load ]
                    ]
                ]
            ]
        ]


viewSetsReps : Int -> Int -> Element msg
viewSetsReps sets reps =
    concat
        [ html span [ has.textWeightBold ] [ string <| toString sets ]
        , nbsp
        , string "×"
        , nbsp
        , html span [ has.textWeightBold ] [ string <| toString reps ]
        ]


viewLoad : Maybe Resources.Load -> Element msg
viewLoad load =
    let
        toElement n unit =
            concat
                [ html span [ has.textWeightBold ] [ string <| toString n ]
                , nbsp
                , string <| pluralize n unit
                ]
    in
    case load of
        Nothing ->
            empty

        Just (Resources.Kgs n) ->
            toElement n "kg"

        Just (Resources.Lbs n) ->
            toElement n "lb"


scheduleAttrs : List Date.Day -> Date.Day -> List (Attribute msg)
scheduleAttrs schedule day =
    if List.member day schedule then
        [ is.selected, is.info ]
    else
        []


onInt : (Int -> msg) -> Attribute msg
onInt toMsg =
    let
        parseInt str =
            case String.toInt str of
                Ok i ->
                    D.succeed (toMsg i)

                Err reason ->
                    D.fail reason
    in
    on "input" <| D.andThen parseInt targetValue


pluralize : Int -> String -> String
pluralize n singular =
    if n == 1 then
        singular
    else
        singular ++ "s"



-- PLAN


generatePlan :
    { exercises : List Exercise
    , weeksToProject : Int
    , daysPerWeek : Int
    }
    -> List { week : List { workout : List Movement } }
generatePlan { exercises, weeksToProject, daysPerWeek } =
    List.repeat weeksToProject
        { week =
            List.repeat daysPerWeek
                { workout = []
                }
        }
