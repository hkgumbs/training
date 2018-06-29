module Client.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Client.Data as Data
import Client.Movement as Movement
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
    , exercises : List Data.Exercise
    }


type alias Client =
    { name : String
    , schedule : List Date.Day
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map2 (Model True 6)
        (getClient context)
        (getExercises context)


getClient : Global.Context -> Task Global.Error Client
getClient context =
    let
        decoder =
            D.map2 Client
                (D.field "name" D.string)
                (D.field "schedule" Data.days)
    in
    case D.decodeString decoder TestData.client of
        Ok client ->
            Task.succeed client

        Err _ ->
            Debug.crash {- TODO -} ""


getExercises : Global.Context -> Task Global.Error (List Data.Exercise)
getExercises context =
    PG.query Resources.exercise Data.Exercise
        |> PG.select .name
        |> PG.hardcoded {- TODO -} Data.HipDominent
        |> PG.hardcoded {- TODO -} Data.Saggital
        |> PG.hardcoded {- TODO -} Data.Multi
        |> PG.hardcoded {- TODO -} []
        |> PG.list PG.noLimit context.dbapi (Global.authorize context)
        |> Http.toTask
        |> Task.mapError (Debug.log "HTTP" >> Global.httpError)


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


viewSidebar : Client -> List Data.Exercise -> Element Tile Msg
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


viewExercise : Data.Exercise -> Element a Msg
viewExercise exercise =
    box []
        [ levelLeftRight
            [ subtitle exercise.name ]
            [ button [ color.danger, outline ] [ icon delete ] ]
        , tags []
            [ tag [] <| Data.typeString exercise.type_
            , tag [] <| Data.planeOfMotionString exercise.planeOfMotion
            , tag [] <| Data.jointActionString exercise.jointAction
            ]
        ]


dayOfWeek : List Date.Day -> Date.Day -> Element a Msg
dayOfWeek schedule day =
    control []
        [ button
            [ selected <| List.member day schedule
            , onClick <| SetSchedule day
            ]
            [ text <| Data.dayString day ]
        ]


viewSchedule : Int -> Client -> List Data.Exercise -> Element Tile Msg
viewSchedule weeksToProject client exercises =
    let
        projection =
            Movement.project
                { exercises = exercises
                , weeksToProject = weeksToProject
                , daysPerWeek = List.length client.schedule
                }
    in
    tile [ vertical ] <|
        List.map viewWeek projection
            ++ [ parent [ button [ onClick LoadMore ] [ text "Load more" ] ] ]


viewWeek : List Movement.Movement -> Element Tile Msg
viewWeek =
    tile [] << List.map viewMovement


viewMovement : Movement.Movement -> Element Tile Msg
viewMovement movement =
    parent
        [ notification
            [ if movement.isProgression then
                color.info
              else
                color.light
            ]
            [ rows
                [ subtitle movement.name
                , level
                    [ [ viewAmount movement.sets "set" ]
                    , [ viewAmount movement.reps "rep" ]
                    , [ viewLoad movement.load ]
                    ]
                ]
            ]
        ]


viewAmount : Int -> String -> Element a msg
viewAmount n unit =
    concat
        [ bold <| toString n
        , nbsp
        , text <| pluralize n unit
        ]


viewLoad : Maybe Data.Load -> Element a msg
viewLoad load =
    let
        toElement n unit =
            bold <| toString n ++ " " ++ pluralize n unit
    in
    case load of
        Nothing ->
            empty

        Just (Data.Kgs n) ->
            toElement n "kg"

        Just (Data.Lbs n) ->
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
