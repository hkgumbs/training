module Main exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Date
import Days
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Ui exposing (..)


type alias Model =
    { clientName : String
    , weeksToProject : Int
    , selected : List ( Date.Day, Exercise )
    , document : String
    , library : List Exercise
    }


type alias Exercise =
    { name : String
    , features : List String
    , movements : List Movement
    }


type alias Movement =
    Row String Int


type alias Headers =
    Row Int Int


type alias Row string int =
    { name : string
    , sets : int
    , reps : int
    , load : string
    , rest : int
    , progressionRate : int
    , notes : string
    }


init : D.Value -> ( Result String Model, Cmd Msg )
init flag =
    D.decodeValue exercises flag
        |> Result.map (uncurry (Model "" 6 []))
        |> pure


exercises : D.Decoder ( String, List Exercise )
exercises =
    D.map2 (,)
        (D.field "document" D.string)
        (D.andThen (List.foldr (D.map2 (::)) (D.succeed []))
            (D.map2 (List.map2 parseExercise)
                (D.field "names" (D.list D.string))
                (D.field "exercises" (D.list (D.list (D.list D.string))))
            )
        )


parseExercise : String -> List (List String) -> D.Decoder Exercise
parseExercise name table =
    case table of
        [] ->
            badFormat name "since this it is empty"

        headers :: exercises ->
            D.map7 Row
                (findHeader "name" name headers)
                (findHeader "sets" name headers)
                (findHeader "reps" name headers)
                (findHeader "load" name headers)
                (findHeader "rest" name headers)
                (findHeader "progression rate" name headers)
                (findHeader "notes" name headers)
                |> D.andThen
                    (\headers ->
                        case
                            exercises
                                |> List.map (parseMovement headers << Array.fromList)
                                |> List.foldr (Result.map2 (::)) (Ok [])
                        of
                            Err reason ->
                                badFormat name reason

                            Ok movements ->
                                D.succeed <| Exercise name [{- TODO -}] movements
                    )


parseMovement : Headers -> Array String -> Result String Movement
parseMovement headers data =
    Result.map4
        (\sets reps rest progressionRate ->
            { sets = sets
            , reps = reps
            , rest = rest
            , progressionRate = progressionRate
            , name = findCell headers.name data
            , load = findCell headers.load data
            , notes = findCell headers.notes data
            }
        )
        (String.toInt <| findCell headers.sets data)
        (String.toInt <| findCell headers.reps data)
        (String.toInt <| findCell headers.rest data)
        (String.toInt <| findCell headers.progressionRate data)


findCell : Int -> Array String -> String
findCell index =
    Array.get index >> Maybe.withDefault ""


findHeader : String -> String -> List String -> D.Decoder Int
findHeader name sheet headers =
    findHeaderHelp name sheet headers 0


findHeaderHelp : String -> String -> List String -> Int -> D.Decoder Int
findHeaderHelp name sheet headers index =
    case headers of
        [] ->
            badFormat sheet <|
                "since it does not have a `"
                    ++ name
                    ++ "` column!"

        next :: rest ->
            if next == name then
                D.succeed index
            else
                findHeaderHelp name sheet rest (index + 1)


badFormat : String -> String -> D.Decoder a
badFormat sheet because =
    D.fail <|
        "The sheet `"
            ++ sheet
            ++ "` is not formatted incorreclty, "
            ++ because
            ++ "!"


type Msg
    = Toggle Exercise Date.Day Bool


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
        Toggle exercise day True ->
            pure { model | selected = ( day, exercise ) :: model.selected }

        Toggle exercise day False ->
            pure { model | selected = List.filter ((/=) ( day, exercise )) model.selected }


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
            el
                [ bulma.section ]
                [ el
                    [ bulma.container ]
                    [ el
                        [ bulma.tile, is.ancestor ]
                        [ el
                            [ bulma.tile ]
                            [ viewSidebar model, viewSchedule model ]
                        ]
                    ]
                ]


viewSidebar : Model -> Element Msg
viewSidebar model =
    el
        [ bulma.tile, is.three, is.parent ]
        [ el
            [ bulma.tile, is.child ]
            [ el
                [ bulma.field ]
                [ input
                    [ bulma.input
                    , is.medium
                    , placeholder "Client name"
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
            , el [] <| List.map (viewExercise model.selected) model.library
            ]
        ]


viewExercise : List ( Date.Day, Exercise ) -> Exercise -> Element Msg
viewExercise selected exercise =
    el
        [ bulma.box ]
        [ el
            [ bulma.level ]
            [ el
                [ bulma.levelLeft ]
                [ el [ bulma.levelItem ] [ viewHeader exercise.name ] ]
            , el
                [ bulma.levelRight ]
                [ el [ bulma.levelItem ] [ viewDays exercise selected ] ]
            ]
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


viewDays : Exercise -> List ( Date.Day, Exercise ) -> Element Msg
viewDays exercise selected =
    let
        schedule =
            selected
                |> List.filter (Tuple.second >> (==) exercise)
                |> List.map Tuple.first
    in
    el
        [ bulma.dropdown, is.hoverable ]
        [ el
            [ bulma.dropdownTrigger ]
            [ button
                [ bulma.button
                , is.info |> when (not <| List.isEmpty schedule)
                ]
                [ icon "calendar" ]
            ]
        , el
            [ bulma.dropdownMenu ]
            [ el
                [ bulma.dropdownContent ]
                [ el
                    [ bulma.dropdownItem ]
                    [ viewDay exercise schedule Date.Mon
                    , viewDay exercise schedule Date.Tue
                    , viewDay exercise schedule Date.Wed
                    , viewDay exercise schedule Date.Thu
                    , viewDay exercise schedule Date.Fri
                    , viewDay exercise schedule Date.Sat
                    , viewDay exercise schedule Date.Sun
                    ]
                ]
            ]
        ]


viewDay : Exercise -> List Date.Day -> Date.Day -> Element Msg
viewDay exercise schedule day =
    el
        [ bulma.control ]
        [ label
            []
            [ input
                [ type_ "checkbox"
                , checked <| List.member day schedule
                , onCheck <| Toggle exercise day
                ]
            , nbsp
            , text <| Days.toString day
            ]
        ]


viewFeatures : List String -> Element Msg
viewFeatures =
    el [ bulma.tags ] << List.map (\name -> el [ bulma.tag ] [ text name ])


viewSchedule : Model -> Element Msg
viewSchedule model =
    let
        projection =
            generatePlan model.weeksToProject model.selected
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
            [ bulma.tile, bulma.notification, is.child, is.light ]
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
    Int
    -> List ( Date.Day, Exercise )
    -> List { week : List { workout : List Movement } }
generatePlan weeksToProject selected =
    List.repeat weeksToProject
        { week =
            -- TODO
            List.repeat 3
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
