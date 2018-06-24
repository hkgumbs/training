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
    case D.decodeString decoder """
        { "name": "Mrs. Incredible"
        , "plan":
            { "schedule": "MWF"
            , "exercises":
                [ { "name": "Deadlift"
                  , "type": "HIP_DOMINENT"
                  , "plane_of_motion": "SAGGITAL"
                  , "joint_action": "MULTI"
                  , "movements":
                      [ { "name": "1/2 Foam Roller Hamstring Stretch"
                        , "sets": 1
                        , "reps": 15
                        , "load": null
                        , "rest": 0
                        , "progression_rate": 4
                        , "minimum_fms": 1
                        , "option": "movement_prep"
                        }
                      , { "name": "Active Straight Leg Raise With Assist "
                        , "sets": 1
                        , "reps": 12
                        , "load": null
                        , "rest": 0
                        , "progression_rate": 3
                        , "minimum_fms": 1
                        , "option": "movement_prep"
                        }
                      ]
                  }
                ]
            }
        }
        """ of
        Ok client ->
            Task.succeed client

        Err _ ->
            Debug.crash {- TODO -} ""


type Msg
    = NoOp
    | ToggleSidebar
    | SetSchedule Date.Day


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToggleSidebar ->
            ( { model | showSidebar = not model.showSidebar }, Cmd.none )

        SetSchedule day ->
            ( { model | client = adjustSchedule day model.client }, Cmd.none )


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


view : Model -> Element Msg
view model =
    ancestor
        [ Tile [ vertical ]
            [ Parent
                [ levelLeftRight
                    [ button [ onClick ToggleSidebar ]
                        [ toggle model.showSidebar
                            [ icon arrowLeft, icon user ]
                            [ icon user, icon arrowRight ]
                        ]
                    ]
                    []
                ]
            , Tile [] <|
                if model.showSidebar then
                    [ viewSidebar model.client
                    , viewSchedule model.weeksToProject model.client
                    ]
                else
                    [ viewSchedule model.weeksToProject model.client
                    ]
            ]
        ]


viewSidebar : Client -> Tile Msg
viewSidebar client =
    let
        days =
            [ Date.Mon, Date.Tue, Date.Wed, Date.Thu, Date.Fri, Date.Sat, Date.Sun ]
    in
    Tile [ width.is4 ]
        [ Parent
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


viewExercise : Exercise.Exercise -> Element Msg
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


dayOfWeek : List Date.Day -> Date.Day -> Element Msg
dayOfWeek schedule day =
    control []
        [ button
            [ selected <| List.member day schedule
            , onClick <| SetSchedule day
            ]
            [ text <| Days.toString day ]
        ]


viewSchedule : Int -> Client -> Tile Msg
viewSchedule weeksToProject client =
    Tile [ vertical ] <|
        List.map2 viewWeek
            (List.repeat weeksToProject color.light)
            (Movement.generate weeksToProject client.plan)


viewWeek : Attribute Msg -> List Movement.Movement -> Tile Msg
viewWeek highlight =
    Tile [] << List.map (viewMovement highlight)


viewMovement : Attribute Msg -> Movement.Movement -> Tile Msg
viewMovement highlight { name, sets, reps, load } =
    Parent
        [ notification
            [ highlight ]
            [ rows
                [ subtitle name
                , level
                    [ [ viewAmount sets "set" ]
                    , [ viewAmount reps "rep" ]
                    , [ viewLoad load ]
                    ]
                ]
            ]
        ]


viewAmount : Int -> String -> Element msg
viewAmount n unit =
    concat
        [ bold <| toString n
        , nbsp
        , text <| pluralize n unit
        ]


viewLoad : Movement.Load -> Element msg
viewLoad load =
    let
        toElement n unit =
            bold <| toString n ++ " " ++ pluralize n unit
    in
    case load of
        Movement.Kgs n ->
            toElement n "kg"

        Movement.Lbs n ->
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
