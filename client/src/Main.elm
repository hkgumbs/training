module Main exposing (Model, Msg, init, update, view)

import Date
import Days
import Html
import Html.Attributes exposing (placeholder)
import Html.Events exposing (..)
import Js
import Json.Decode as D
import Ui exposing (..)


type alias Model =
    { settings : Settings
    , sheetSearch : String
    , exercises : Maybe (List Exercise)
    }


type alias Settings =
    { client : String
    , weeksToProject : Int
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


init : ( Model, Cmd Msg )
init =
    pure
        { settings = { client = "Client", weeksToProject = 6 }
        , sheetSearch = ""
        , exercises = Nothing
        }


exercises : D.Decoder (List Exercise)
exercises =
    D.list
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


type Msg
    = Login
    | SheetSearch
    | LoadMore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        Login ->
            ( model, Js.login )

        SheetSearch ->
            ( model, Js.pickSheet model.sheetSearch )

        LoadMore ->
            pure { model | settings = { settings | weeksToProject = 6 + settings.weeksToProject } }


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


remove : a -> List a -> List a
remove a =
    List.filter ((/=) a)


view : Model -> Element Msg
view model =
    case model.exercises of
        Nothing ->
            el
                [ bulma.hero, is.bold, is.light, is.fullheight ]
                [ el [ bulma.heroBody ] [ viewStep1 ] ]

        Just exercises ->
            el
                [ bulma.container ]
                [ el [ bulma.section ] [ viewStep2 model.settings exercises ] ]


viewStep1 : Element Msg
viewStep1 =
    el
        [ bulma.container ]
        [ h1
            [ bulma.title ]
            [ text "1. Import your exercises" ]
        , el
            [ bulma.buttons ]
            [ el
                [ bulma.control ]
                [ button [ bulma.button, onClick Login ] [ text "Login" ] ]
            , el
                [ bulma.control ]
                [ button
                    [ bulma.button, is.primary, onClick SheetSearch ]
                    [ text "Pick" ]
                ]
            ]
        ]


viewStep2 : Settings -> List Exercise -> Element Msg
viewStep2 settings exercises =
    el
        [ bulma.tile, is.ancestor ]
        [ el
            [ bulma.tile, is.vertical ]
            [ h1
                [ bulma.tile, is.child, bulma.title ]
                [ text "2. Create a plan" ]
            , el
                [ bulma.tile ]
                [ viewSidebar settings exercises
                , viewSchedule settings exercises
                ]
            ]
        ]


viewSidebar : Settings -> List Exercise -> Element Msg
viewSidebar settings exercises =
    el
        [ bulma.tile, is.four ]
        [ el
            [ is.parent ]
            [ el
                [ bulma.notification, is.white ]
                [ h1 [ bulma.title ] [ text settings.client ]
                , label [ bulma.label ] [ text "Schedule" ]
                , el
                    [ bulma.field, has.addons ]
                    [ viewDay [] Date.Mon
                    , viewDay [] Date.Tue
                    , viewDay [] Date.Wed
                    , viewDay [] Date.Thu
                    , viewDay [] Date.Fri
                    , viewDay [] Date.Sat
                    , viewDay [] Date.Sun
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
            [ bulma.button ]
            [ text <| Days.toString day ]
        ]


viewSchedule : Settings -> List Exercise -> Element Msg
viewSchedule settings exercises =
    let
        projection =
            generatePlan
                { exercises = exercises
                , weeksToProject = settings.weeksToProject
                , daysPerWeek = {- TODO -} 3
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


main : Program D.Value Model Msg
main =
    Html.programWithFlags
        { init = \_ -> init
        , update = update
        , view = \model -> Ui.toHtml identity [ view model ]
        , subscriptions = \_ -> Sub.none
        }
