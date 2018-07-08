module Client.Page exposing (Model, Msg, init, update, view)

import Client.Days as Days
import Date
import Global
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Http
import Json.Decode as D
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
    , movements : List Movement
    }


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : String
    , rest : Int
    , progressionRate : Int
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        getWeeksToProject
        (getClient context)
        getExercises


getWeeksToProject : Task x Int
getWeeksToProject =
    Task.succeed 6


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


getExercises : Task Global.Error (List Exercise)
getExercises =
    Http.get "/api/exercises"
        (D.list
            (D.map3 Exercise
                (D.field "name" D.string)
                (D.field "features" (D.list D.string))
                (D.field "movements"
                    (D.list
                        (D.map6 Movement
                            (D.field "name" D.string)
                            (D.field "sets" D.int)
                            (D.field "reps" D.int)
                            (D.field "load" D.string)
                            (D.field "rest" D.int)
                            (D.field "progression_rate" D.int)
                        )
                    )
                )
            )
        )
        |> Http.toTask
        |> Task.mapError Global.httpError


type Msg
    = NoOp
    | AddToSchedule Date.Day
    | RemoveFromSchedule Date.Day
    | LoadMore


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg ({ client } as model) =
    case msg of
        NoOp ->
            pure model

        AddToSchedule day ->
            pure { model | client = { client | schedule = day :: client.schedule } }

        RemoveFromSchedule day ->
            pure { model | client = { client | schedule = remove day client.schedule } }

        LoadMore ->
            pure { model | weeksToProject = 6 + model.weeksToProject }


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


remove : a -> List a -> List a
remove a =
    List.filter ((/=) a)


view : Model -> Element Msg
view model =
    el
        [ bulma.container ]
        [ el
            [ bulma.section ]
            [ el
                [ bulma.tile, is.ancestor ]
                [ el
                    [ bulma.tile, is.vertical ]
                    [ el [ bulma.tile ] [ viewContent model ] ]
                ]
            ]
        ]


viewContent : Model -> Element Msg
viewContent model =
    concat
        [ viewSidebar model.client model.exercises
        , viewSchedule model.weeksToProject model.client model.exercises
        ]


viewSidebar : Client -> List Exercise -> Element Msg
viewSidebar client exercises =
    el
        [ bulma.tile, is.four ]
        [ el
            [ is.parent ]
            [ el
                [ bulma.notification, is.white ]
                [ h1 [ bulma.title ] [ text client.name ]
                , label [ bulma.label ] [ text "Schedule" ]
                , el
                    [ bulma.field, has.addons ]
                    [ viewDay client.schedule Date.Mon
                    , viewDay client.schedule Date.Tue
                    , viewDay client.schedule Date.Wed
                    , viewDay client.schedule Date.Thu
                    , viewDay client.schedule Date.Fri
                    , viewDay client.schedule Date.Sat
                    , viewDay client.schedule Date.Sun
                    ]
                , hr
                , el
                    [ bulma.field ]
                    [ label [ bulma.label ] [ text "Exercises" ]
                    , el
                        [ bulma.control, has.iconsLeft ]
                        [ icon "search"
                        , input [ bulma.input, placeholder "Add an exercise..." ]
                        ]
                    ]
                , el
                    [ bulma.columns ]
                    [ el [ bulma.column ] <| List.map viewExercise exercises ]
                ]
            ]
        ]


viewExercise : Exercise -> Element Msg
viewExercise exercise =
    el
        [ bulma.box ]
        [ el
            [ bulma.level ]
            [ el
                [ bulma.levelLeft ]
                [ h2 [ bulma.subtitle ] [ text exercise.name ] ]
            , el
                [ bulma.levelRight ]
                [ button
                    [ bulma.button
                    , is.danger
                    , is.outlined
                    ]
                    [ icon "minus-circle" ]
                ]
            ]
        , el [ bulma.tags ] <|
            List.map
                (\name -> el [ bulma.tag ] [ text name ])
                exercise.features
        ]


viewDay : List Date.Day -> Date.Day -> Element Msg
viewDay schedule day =
    el
        [ bulma.control ]
        [ button
            (bulma.button :: scheduleAttrs schedule day)
            [ text <| Days.toString day ]
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
    el [ bulma.tile, is.vertical ] <|
        List.map viewWeek projection
            ++ [ el
                    [ bulma.tile, is.parent ]
                    [ button
                        [ bulma.button, bulma.tile, is.child, onClick LoadMore ]
                        [ text "Load more" ]
                    ]
               ]


viewWeek : { week : List { workout : List Movement } } -> Element Msg
viewWeek { week } =
    el [ bulma.tile ] <| List.map viewWorkout week


viewWorkout : { workout : List Movement } -> Element Msg
viewWorkout { workout } =
    el
        [ bulma.tile, is.parent ]
        [ el
            [ bulma.notification, bulma.tile, is.child, is.light ]
            (List.map viewMovement workout)
        ]


viewMovement : Movement -> Element Msg
viewMovement movement =
    concat
        [ el
            [ bulma.columns ]
            [ el
                [ bulma.column ]
                [ h2 [ bulma.subtitle ] [ text movement.name ]
                , el
                    [ bulma.level ]
                    [ el
                        [ bulma.levelItem ]
                        [ viewSetsReps movement.sets movement.reps ]
                    , el
                        [ bulma.levelItem ]
                        [ el [ has.textWeightBold ] [ text movement.load ] ]
                    ]
                ]
            ]
        ]


viewSetsReps : Int -> Int -> Element msg
viewSetsReps sets reps =
    concat
        [ el [ has.textWeightBold ] [ text <| toString sets ]
        , nbsp
        , text "×"
        , nbsp
        , el [ has.textWeightBold ] [ text <| toString reps ]
        ]


scheduleAttrs : List Date.Day -> Date.Day -> List (Attribute Msg)
scheduleAttrs schedule day =
    if List.member day schedule then
        [ is.selected, is.info, onClick <| RemoveFromSchedule day ]
    else
        [ onClick <| AddToSchedule day ]


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
