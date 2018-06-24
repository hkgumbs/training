module Client.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Client.Days as Days
import Client.Exercise as Exercise
import Client.Movement as Movement
import Date
import Global
import Html.Events exposing (..)
import Json.Decode as D
import Task exposing (Task)
import TestData


type alias Model =
    { showSidebar : Bool
    , weeksToProject : Int
    , client : Client
    }


type alias Client =
    { name : String
    , plan : Movement.Plan
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map (Model True 6) getClient


getClient : Task Global.Error Client
getClient =
    let
        decoder =
            D.map2 Client
                (D.field "name" D.string)
                (D.field "plan" Movement.plan)
    in
    case D.decodeString decoder TestData.client of
        Ok client ->
            Task.succeed client

        Err _ ->
            Debug.crash {- TODO -} ""


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
adjustSchedule day ({ plan } as client) =
    let
        schedule =
            if List.member day plan.schedule then
                List.filter ((/=) day) client.plan.schedule
            else
                day :: client.plan.schedule
    in
    { client | plan = { plan | schedule = schedule } }


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
                    [ viewSidebar model.client
                    , viewSchedule model.weeksToProject model.client
                    ]
                else
                    [ viewSchedule model.weeksToProject model.client
                    ]
            ]
        ]


viewSidebar : Client -> Element Tile Msg
viewSidebar client =
    let
        days =
            [ Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun ]
    in
    tile [ width.is4 ]
        [ parent
            [ notification [ color.white ]
                [ title client.name
                , label "Schedule"
                , field [ form.addons ] <|
                    List.map
                        (dayOfWeek client.plan.schedule)
                        (List.sortBy Days.mondayFirst days)
                , hr
                , field []
                    [ label "Excercises"
                    , control
                        [ icons.left ]
                        [ icon search, textInput [ placeholder "Add an exercise..." ] ]
                    ]
                , rows <| List.map viewExercise client.plan.exercises
                ]
            ]
        ]


viewExercise : Exercise.Exercise -> Element a Msg
viewExercise exercise =
    box []
        [ levelLeftRight
            [ subtitle exercise.name ]
            [ button [ color.danger, outline ] [ icon delete ] ]
        , tags []
            [ tag [] <| Exercise.typeString exercise.type_
            , tag [] <| Exercise.planeOfMotionString exercise.planeOfMotion
            , tag [] <| Exercise.jointActionString exercise.jointAction
            ]
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


viewSchedule : Int -> Client -> Element Tile Msg
viewSchedule weeksToProject client =
    tile [ vertical ] <|
        List.map viewWeek (Movement.project weeksToProject client.plan)
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


viewLoad : Maybe Exercise.Load -> Element a msg
viewLoad load =
    let
        toElement n unit =
            bold <| toString n ++ " " ++ pluralize n unit
    in
    case load of
        Nothing ->
            empty

        Just (Exercise.Kgs n) ->
            toElement n "kg"

        Just (Exercise.Lbs n) ->
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
