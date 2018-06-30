module Client.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Client.Days as Days
import Date
import Global
import Html.Events exposing (..)
import Http
import Json.Decode as D
import PostgRest as PG
import Resources
import Task exposing (Task)
import TestData


type alias Model =
    { showSidebar : Bool
    , weeksToProject : Int
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
    Task.map2 (Model True 6)
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
    | ToggleSidebar
    | SetSchedule Date.Day
    | LoadMore


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleSidebar ->
            ( { model | showSidebar = not model.showSidebar }, Cmd.none )

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


view : Model -> Element a Msg
view model =
    ancestor
        [ tile [ vertical ]
            [ parent
                [ levelLeftRight
                    [ button [ onClick ToggleSidebar ]
                        [ toggle model.showSidebar
                            [ icon arrowLeft, icon user ]
                            [ icon user, icon arrowRight ]
                        ]
                    ]
                    []
                ]
            , tile [] <|
                if model.showSidebar then
                    [ viewSidebar model.client model.exercises
                    , viewSchedule model.weeksToProject model.client model.exercises
                    ]
                else
                    [ viewSchedule model.weeksToProject model.client model.exercises
                    ]
            ]
        ]


viewSidebar : Client -> List Exercise -> Element Tile Msg
viewSidebar client exercises =
    tile [ width.is4 ]
        [ parent
            [ notification [ color.white ]
                [ title client.name
                , label "Schedule"
                , field [ form.addons ]
                    [ dayOfWeek client.schedule Date.Mon
                    , dayOfWeek client.schedule Date.Tue
                    , dayOfWeek client.schedule Date.Wed
                    , dayOfWeek client.schedule Date.Thu
                    , dayOfWeek client.schedule Date.Fri
                    , dayOfWeek client.schedule Date.Sat
                    , dayOfWeek client.schedule Date.Sun
                    ]
                , hr
                , field []
                    [ label "Excercises"
                    , control
                        [ icons.left ]
                        [ icon search, textInput [ placeholder "Add an exercise..." ] ]
                    ]
                , rows <| List.map viewExercise exercises
                ]
            ]
        ]


viewExercise : Exercise -> Element a Msg
viewExercise exercise =
    box []
        [ levelLeftRight
            [ subtitle exercise.name ]
            [ button [ color.danger, outline ] [ icon delete ] ]
        , tags [] <| List.map (tag []) exercise.features
        ]


dayOfWeek : List Date.Day -> Date.Day -> Element a Msg
dayOfWeek schedule day =
    control []
        [ button
            [ selected <| List.member day schedule
            , onClick <| SetSchedule day
            ]
            [ text <| Days.toString day ]
        ]


viewSchedule : Int -> Client -> List Exercise -> Element Tile Msg
viewSchedule weeksToProject client exercises =
    let
        projection =
            generatePlan
                { exercises = exercises
                , weeksToProject = weeksToProject
                , daysPerWeek = List.length client.schedule
                }
    in
    tile [ vertical ] <|
        List.map viewWeek projection
            ++ [ parent [ button [ onClick LoadMore ] [ text "Load more" ] ] ]


viewWeek : { week : List { workout : List Movement } } -> Element Tile Msg
viewWeek { week } =
    tile [] <| List.map viewWorkout week


viewWorkout : { workout : List Movement } -> Element Tile Msg
viewWorkout { workout } =
    parent [ notification [ color.light ] <| List.map viewMovement workout ]


viewMovement : Movement -> Element Tile Msg
viewMovement movement =
    concat
        [ rows
            [ subtitle movement.name
            , level
                [ [ viewSetsReps movement.sets movement.reps ]
                , [ viewLoad movement.load ]
                ]
            ]
        ]


viewSetsReps : Int -> Int -> Element a msg
viewSetsReps sets reps =
    [ bold <| toString sets, text "×", bold <| toString reps ]
        |> List.intersperse nbsp
        |> concat


viewLoad : Maybe Resources.Load -> Element a msg
viewLoad load =
    let
        toElement n unit =
            concat [ bold <| toString n, nbsp, text <| pluralize n unit ]
    in
    case load of
        Nothing ->
            empty

        Just (Resources.Kgs n) ->
            toElement n "kg"

        Just (Resources.Lbs n) ->
            toElement n "lb"


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
