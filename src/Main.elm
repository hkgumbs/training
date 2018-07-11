module Main exposing (Model, Msg, init, update, view)

import Date
import Days
import Html
import Html.Attributes exposing (defaultValue, placeholder)
import Html.Events exposing (..)
import Json.Decode as D
import Ui exposing (..)


type alias Model =
    { doc : String
    , clientName : String
    , weeksToProject : Int
    , exercises : List Exercise
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


init : D.Value -> ( Result String Model, Cmd Msg )
init flag =
    D.decodeValue exercises flag
        |> Result.map (\( doc, exercises ) -> Model doc "" 6 exercises)
        |> pure


exercises : D.Decoder ( String, List Exercise )
exercises =
    D.map2 (,)
        (D.field "doc" D.string)
        (D.andThen (List.foldr (D.map2 (::)) (D.succeed []))
            (D.map2 (List.map2 parseExercise)
                (D.field "names" (D.list D.string))
                (D.field "exercises" (D.list (D.list (D.list D.string))))
            )
        )


parseExercise : String -> List (List String) -> D.Decoder Exercise
parseExercise name data =
    D.succeed
        { name = name
        , features = {- TODO -} []
        , movements = {- TODO -} []
        }



-- D.list
--     (D.map3 Exercise
--         (D.field "name" D.string)
--         (D.field "features" (D.list D.string))
--         (D.field "movements"
--             (D.list
--                 (D.map6 Movement
--                     (D.field "name" D.string)
--                     (D.field "sets" D.int)
--                     (D.field "reps" D.int)
--                     (D.field "load" D.string)
--                     (D.field "rest" D.int)
--                     (D.field "progression_rate" D.int)
--                 )
--             )
--         )
--     )


type Msg
    = NoOp


updateResult : Msg -> Result x Model -> ( Result x Model, Cmd Msg )
updateResult msg result =
    case result of
        Err _ ->
            pure result

        Ok model ->
            Tuple.mapFirst Ok <| update msg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            pure model


pure : x -> ( x, Cmd Msg )
pure x =
    ( x, Cmd.none )


remove : a -> List a -> List a
remove a =
    List.filter ((/=) a)


view : Result String Model -> Element Msg
view result =
    case result of
        Err reason ->
            text reason

        Ok model ->
            concat
                [ viewNav model.doc
                , el [ bulma.section ]
                    [ el
                        [ bulma.tile, is.ancestor ]
                        [ el
                            [ bulma.tile ]
                            [ viewSidebar model
                            , viewSchedule model
                            ]
                        ]
                    ]
                ]


viewNav : String -> Element Msg
viewNav doc =
    el {- TODO -} [] []


viewSidebar : Model -> Element Msg
viewSidebar model =
    el
        [ bulma.tile, is.four ]
        [ el
            [ is.parent ]
            [ el
                [ bulma.notification, is.white ]
                [ el
                    [ bulma.field ]
                    [ input
                        [ bulma.input
                        , is.medium
                        , placeholder "Client"
                        , defaultValue model.clientName
                        ]
                    ]
                , hr
                , el
                    [ bulma.field ]
                    [ label [ bulma.label ] [ text "Exercises" ]
                    , el
                        [ bulma.control, has.iconsLeft ]
                        [ icon "search"
                        , input [ bulma.input, placeholder "Find an exercise..." ]
                        ]
                    ]
                , el
                    [ bulma.columns ]
                    [ el [ bulma.column ] <| List.map viewExercise model.exercises ]
                ]
            ]
        ]


viewExercise : Exercise -> Element Msg
viewExercise exercise =
    el
        [ bulma.box ]
        [ viewHeader exercise.name
        , viewDays {- TODO -} []
        , viewFeatures exercise.features
        ]


viewHeader : String -> Element Msg
viewHeader name =
    el
        [ bulma.level ]
        [ el
            [ bulma.levelLeft ]
            [ h2 [ bulma.subtitle ] [ text name ] ]
        ]


viewDays : List Date.Day -> Element Msg
viewDays schedule =
    el
        [ bulma.field, has.addons ]
        [ viewDay schedule Date.Mon
        , viewDay schedule Date.Tue
        , viewDay schedule Date.Wed
        , viewDay schedule Date.Thu
        , viewDay schedule Date.Fri
        , viewDay schedule Date.Sat
        , viewDay schedule Date.Sun
        ]


viewDay : List Date.Day -> Date.Day -> Element Msg
viewDay schedule day =
    el
        [ bulma.control ]
        [ button
            [ bulma.button ]
            [ text <| Days.toString day ]
        ]


viewFeatures : List String -> Element Msg
viewFeatures =
    el [ bulma.tags ] << List.map (\name -> el [ bulma.tag ] [ text name ])


viewSchedule : Model -> Element Msg
viewSchedule model =
    let
        projection =
            generatePlan
                { exercises = model.exercises
                , weeksToProject = model.weeksToProject
                , daysPerWeek = {- TODO -} 3
                }
    in
    el [ bulma.tile, is.vertical ] <| List.map viewWeek projection


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


main : Program D.Value (Result String Model) Msg
main =
    Html.programWithFlags
        { init = init
        , update = updateResult
        , view = \model -> Ui.toHtml identity [ view model ]
        , subscriptions = \_ -> Sub.none
        }
