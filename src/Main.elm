module Main exposing (Model, Msg, init, update, view)

import Date
import Days
import Dict exposing (Dict)
import Exercise exposing (Exercise, Movement)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Ui exposing (..)


type alias Model =
    { clientName : String
    , search : String
    , weeksToProject : Int
    , selected : List ( Date.Day, Exercise )
    , startingMovements : Dict String Int

    -- FROM FLAGS
    , document : String
    , library : List Exercise
    }


init : D.Value -> ( Result String Model, Cmd Msg )
init flag =
    D.decodeValue Exercise.fromSheet flag
        |> Result.map (uncurry (Model "" "" 6 [] Dict.empty))
        |> pure


type Msg
    = InputClientName String
    | InputSearch String
    | StartExerciseAt Exercise Int
    | ToggleSchedule Exercise Date.Day Bool


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
        InputClientName value ->
            pure { model | clientName = value }

        InputSearch value ->
            pure { model | search = value }

        StartExerciseAt { name } index ->
            pure { model | startingMovements = Dict.insert name index model.startingMovements }

        ToggleSchedule exercise day True ->
            pure { model | selected = ( day, exercise ) :: model.selected }

        ToggleSchedule exercise day False ->
            pure { model | selected = List.filter ((/=) ( day, exercise )) model.selected }


pure : x -> ( x, Cmd Msg )
pure x =
    ( x, Cmd.none )


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
                [ bulma.field, has.addons ]
                [ el
                    [ bulma.control ]
                    [ input
                        [ bulma.input
                        , is.medium
                        , placeholder "Client name"
                        , defaultValue model.clientName
                        , onInput InputClientName
                        ]
                    ]
                , el
                    [ bulma.control ]
                    [ button
                        [ bulma.button
                        , is.medium
                        , disabled <|
                            List.isEmpty model.selected
                                || String.isEmpty model.clientName
                        ]
                        [ icon "file-download" ]
                    ]
                ]
            , hr
            , el
                [ bulma.field ]
                [ label [ bulma.label ] [ text "Exercises" ]
                , el
                    [ bulma.control, has.iconsLeft ]
                    [ icon "search"
                    , input
                        [ bulma.input
                        , placeholder "Find an exercise..."
                        , onInput InputSearch
                        ]
                    ]
                ]
            , model.library
                |> List.filter (meetsSearchCriteria model.search)
                |> List.map (viewExercise model.selected)
                |> el []
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
        [ el [ bulma.levelLeft ] [ h2 [ bulma.subtitle ] [ text name ] ] ]


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
                    [ el
                        [ bulma.field ]
                        [ el [ bulma.control ]
                            [ label [ bulma.label ] [ text "Schedule" ]
                            , concat <| List.map (viewDay exercise schedule) Days.list
                            ]
                        ]
                    , el
                        [ bulma.field ]
                        [ el
                            [ bulma.control ]
                            [ label [ bulma.label ] [ text "Starting movement" ]
                            , select
                                [ bulma.select
                                , onInt "change" <| StartExerciseAt exercise
                                ]
                                (List.indexedMap viewStartingOption exercise.movements)
                            ]
                        ]
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
                , onCheck <| ToggleSchedule exercise day
                ]
            , nbsp
            , text <| Days.toString day
            ]
        ]


viewStartingOption : Int -> Movement -> Element Msg
viewStartingOption index movement =
    option
        [ value <| toString index ]
        [ text movement.name, text " (", viewSetRepSummary movement, text ")" ]


viewFeatures : List String -> Element Msg
viewFeatures =
    el [ bulma.tags ] << List.map (\name -> el [ bulma.tag ] [ text name ])


viewSchedule : Model -> Element Msg
viewSchedule model =
    let
        projection =
            generatePlan model.startingMovements model.weeksToProject model.selected
    in
    if List.all (.week >> List.isEmpty) projection then
        el
            [ bulma.tile, is.parent ]
            [ el
                [ bulma.tile, is.child ]
                [ h2 [ bulma.subtitle ] [ text "Nothing to see yet!" ]
                , text "Try using "
                , icon "calendar"
                , text " to add exercises to the schedule."
                ]
            ]
    else
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
    el
        [ bulma.columns ]
        [ el
            [ bulma.column ]
            [ h2 [ has.textWeightBold ] [ text movement.name ]
            , el [] [ viewSetRepSummary movement ]
            , el [] [ text movement.load ]
            , el [] [ text movement.notes ]
            ]
        ]


onInt : String -> (Int -> msg) -> Attribute msg
onInt event toMsg =
    let
        parseInt str =
            case String.toInt str of
                Ok i ->
                    D.succeed (toMsg i)

                Err reason ->
                    D.fail reason
    in
    on event <| D.andThen parseInt targetValue


viewSetRepSummary : Movement -> Element msg
viewSetRepSummary movement =
    text <|
        toString movement.sets
            ++ " "
            ++ pluralize movement.sets "set"
            ++ " × "
            ++ toString movement.reps
            ++ " "
            ++ pluralize movement.reps "rep"


pluralize : Int -> String -> String
pluralize n singular =
    if n == 1 then
        singular
    else
        singular ++ "s"


meetsSearchCriteria : String -> Exercise -> Bool
meetsSearchCriteria term exercise =
    String.contains term exercise.name
        || List.any (String.contains term) exercise.features



-- PLAN


generatePlan :
    Dict String Int
    -> Int
    -> List ( Date.Day, Exercise )
    -> List { week : List { workout : List Movement } }
generatePlan startingMovements weeksToProject selected =
    List.range 1 weeksToProject
        |> List.map (\index -> generateWeek startingMovements index selected)


generateWeek : Dict String Int -> Int -> List ( Date.Day, Exercise ) -> { week : List { workout : List Movement } }
generateWeek startingMovements index selected =
    let
        onDay day =
            selected
                |> List.filter (Tuple.first >> (==) day)
                |> List.map Tuple.second
    in
    { week =
        List.map (generateWorkout startingMovements index << onDay) Days.list
            |> List.filter (not << List.isEmpty << .workout)
    }


generateWorkout : Dict String Int -> Int -> List Exercise -> { workout : List Movement }
generateWorkout startingMovements index exercises =
    let
        skipMovements { name } =
            Dict.get name startingMovements
                |> Maybe.withDefault 0

        plan exercise =
            List.drop (skipMovements exercise) exercise.movements
                |> generateMovement index
    in
    { workout = List.filterMap plan exercises }


generateMovement : Int -> List Movement -> Maybe Movement
generateMovement index movements =
    case movements of
        [] ->
            Nothing

        first :: rest ->
            if index <= first.progressionRate then
                Just first
            else
                generateMovement (index - first.progressionRate) rest


main : Program D.Value (Result String Model) Msg
main =
    Html.programWithFlags
        { init = init
        , update = updateResult
        , view = \model -> Ui.toHtml identity [ view model ]
        , subscriptions = \_ -> Sub.none
        }
