module Client.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Client.Days as Days
import Client.Movement as Movement
import Date
import Global
import Html.Events exposing (..)
import Json.Decode as D
import Task exposing (Task)


type alias Model =
    { name : String
    , plan : Movement.Plan
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    let
        decoder =
            D.map2 Model
                (D.field "name" D.string)
                (D.field "plan" Movement.plan)
    in
    case D.decodeString decoder """
        { "name": "Mrs. Incredible"
        , "plan":
            { "schedule": "MWF"
            }
        }
        """ of
        Ok model ->
            Task.succeed model

        Err _ ->
            Debug.crash {- TODO -} ""


type Msg
    = NoOp
    | SetSchedule Date.Day Bool


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSchedule day checked ->
            ( { model | plan = adjustSchedule day checked model.plan }, Cmd.none )


adjustSchedule : Date.Day -> Bool -> Movement.Plan -> Movement.Plan
adjustSchedule day checked plan =
    let
        schedule =
            if checked then
                List.sortBy Days.mondayFirst (day :: plan.schedule)
            else
                List.filter ((/=) day) plan.schedule
    in
    { plan | schedule = schedule }


view : Model -> Element Msg
view model =
    ancestor
        [ Tile [ width.is4 ]
            [ Parent [ viewSidebar model.name model.plan ] ]
        , Tile [ width.is8, vertical ] <|
            List.map viewWeek <|
                Movement.generate 6 model.plan
        ]


viewSidebar : String -> Movement.Plan -> Element Msg
viewSidebar name { schedule } =
    notification [ color.white ]
        [ title name
        , field []
            [ label "Schedule"
            , dayOfWeek schedule Date.Mon "Monday"
            , dayOfWeek schedule Date.Tue "Tuesday"
            , dayOfWeek schedule Date.Wed "Wednesday"
            , dayOfWeek schedule Date.Thu "Thursday"
            , dayOfWeek schedule Date.Fri "Friday"
            , dayOfWeek schedule Date.Sat "Saturday"
            , dayOfWeek schedule Date.Sun "Sunday"
            ]
        ]


dayOfWeek : List Date.Day -> Date.Day -> String -> Element Msg
dayOfWeek schedule day name =
    checkbox [ onCheck <| SetSchedule day ] (List.member day schedule) name


viewWeek : List Movement.Movement -> Tile Msg
viewWeek =
    Tile [] << List.map viewMovement


viewMovement : Movement.Movement -> Tile Msg
viewMovement { sets, reps, load } =
    Parent
        [ notification
            [ color.primary ]
            [ text "__DAY__" ]
        ]


onInt : (Int -> msg) -> Attribute msg
onInt toMsg =
    on "input" <|
        D.andThen
            (\str ->
                case String.toInt str of
                    Ok i ->
                        D.succeed (toMsg i)

                    Err reason ->
                        D.fail reason
            )
            targetValue
