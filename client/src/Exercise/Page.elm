module Exercise.Page exposing (Model, Msg, init, update, view)

import Exercise.Field as Field exposing (Field)
import Exercise.Steps as Steps
import Global
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import PostgRest as PG
import Resources
import Task exposing (Task)
import Ui exposing (..)


type alias Model =
    { exercise : Exercise
    , steps : Steps.State Interval Movement
    , hover : Maybe Steps.Reference
    }


type alias Exercise =
    { name : String }


type alias Interval =
    Field Int


type alias Movement =
    { name : Field String
    , sets : Field Int
    , reps : Field Int
    , load : Field String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context (getExercise id))
        (Task.succeed (Steps.empty newInterval newMovement))
        (Task.succeed Nothing)


{-| TODO: move to Global
-}
pg : Global.Context -> PG.Request a -> Task Global.Error a
pg { auth, dbapi } =
    PG.toHttpRequest { timeout = Nothing, token = auth, url = dbapi }
        >> Http.toTask
        >> Task.mapError Global.httpError


getExercise : String -> PG.Request Exercise
getExercise id =
    PG.readOne Resources.exercise
        { select =
            PG.map Exercise
                (PG.field .name)
        , where_ = PG.eq id .id
        }


newInterval : Interval
newInterval =
    Field.int "2"


newMovement : Movement
newMovement =
    { name = Field.nonEmptyString ""
    , sets = Field.int ""
    , reps = Field.int ""
    , load = Field.string ""
    }


type Msg
    = NoOp
    | AddStep
    | SetHover (Maybe Steps.Reference)
    | Delete Steps.Reference
    | EditMovement Steps.Reference Change
    | EditInterval Steps.Reference String


type Change
    = Name String
    | Sets String
    | Reps String
    | Load String


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetHover ref ->
            ( { model | hover = ref }, Cmd.none )

        AddStep ->
            pure { model | steps = Steps.addStep model.steps }

        Delete ref ->
            pure { model | steps = Steps.remove ref model.steps }

        EditMovement ref change ->
            pure { model | steps = Steps.editMovement (apply change) ref model.steps }

        EditInterval ref new ->
            pure { model | steps = Steps.editInterval (\_ -> Field.int new) ref model.steps }


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


apply : Change -> Movement -> Movement
apply change movement =
    case change of
        Name new ->
            { movement | name = Field.nonEmptyString new }

        Sets new ->
            { movement | sets = Field.int new }

        Reps new ->
            { movement | reps = Field.int new }

        Load new ->
            { movement | load = Field.string new }


view : Model -> Element Msg
view model =
    el
        [ bulma.columns ]
        [ el
            [ bulma.column ]
            [ viewHeader model.exercise
            , viewSteps model.hover model.steps
            , viewFooter
            ]
        ]


viewHeader : Exercise -> Element Msg
viewHeader { name } =
    concat
        [ el
            [ bulma.level ]
            [ el
                [ bulma.levelLeft ]
                [ el
                    [ bulma.levelItem ]
                    [ input [ bulma.input, is.large, value name ] ]
                ]
            , el
                [ bulma.levelRight ]
                [ el
                    [ bulma.levelItem ]
                    [ button
                        [ bulma.button, is.primary, is.large ]
                        [ icon "check"
                        , el [] [ text "Save" ]
                        ]
                    ]
                ]
            ]
        , hr
        ]


viewSteps : Maybe Steps.Reference -> Steps.State Interval Movement -> Element Msg
viewSteps hover =
    Steps.view
        { interval = viewInterval
        , movements =
            List.map (viewMovement hover)
                >> List.intersperse viewOr
                >> el [ bulma.level ]
        }
        >> concat


viewInterval : Steps.Reference -> Field Int -> Element Msg
viewInterval ref weeks =
    el
        [ bulma.notification, has.backgroundLight ]
        [ el
            [ bulma.level ]
            [ el [ bulma.levelItem ] <|
                List.intersperse nbsp
                    [ text "progress in "
                    , numberInput (EditInterval ref) "" weeks
                    , text " weeks"
                    ]
            ]
        ]


viewOr : Element msg
viewOr =
    el
        [ bulma.levelItem ]
        [ button [ bulma.button, is.static ] [ text "OR" ] ]


viewMovement : Maybe Steps.Reference -> ( Steps.Context, Movement ) -> Element Msg
viewMovement hover ( context, movement ) =
    el
        [ bulma.levelItem ]
        [ el
            [ bulma.box
            , onMouseLeave (SetHover Nothing)
            , onMouseEnter (SetHover <| Just context.reference)
            ]
            [ el
                [ bulma.media ]
                [ el
                    [ bulma.mediaContent ]
                    [ viewMovementHeader
                    , viewMovementName context movement.name
                    , viewMovementDetails context movement
                    ]
                , el
                    [ bulma.mediaRight
                    , is.invisible |> when (hover /= Just context.reference)
                    ]
                    [ viewMovementActions context ]
                ]
            ]
        ]


viewMovementHeader : Element Msg
viewMovementHeader =
    el
        [ bulma.field ]
        [ el
            [ bulma.control ]
            [ el
                [ bulma.select ]
                [ select
                    []
                    [ option [] [ text "Movement Prep" ]
                    , option [] [ text "Resistance Training" ]
                    ]
                ]
            ]
        ]


viewMovementName : Steps.Context -> Field String -> Element Msg
viewMovementName { reference } name =
    el
        [ bulma.field ]
        [ el
            [ bulma.control ]
            [ input
                [ bulma.input
                , is.medium
                , type_ "text"
                , placeholder "Movement name"
                , value (Field.view name)
                , is.warning |> when (Field.isInvalid name)
                , onInput <| EditMovement reference << Name
                ]
            ]
        ]


viewMovementDetails : Steps.Context -> Movement -> Element Msg
viewMovementDetails { reference } { sets, reps, load } =
    let
        change f =
            EditMovement reference << f
    in
    el
        [ bulma.level ]
        [ el [ bulma.levelItem ] [ numberInput (change Sets) "sets" sets ]
        , el [ bulma.levelItem ] [ nbsp, text "×", nbsp ]
        , el [ bulma.levelItem ] [ numberInput (change Reps) "reps" reps ]
        , el [ bulma.levelItem ] [ nbsp, text "@", nbsp ]
        , el [ bulma.levelItem ] [ loadInput load reference ]
        ]


viewMovementActions : Steps.Context -> Element Msg
viewMovementActions context =
    let
        break =
            concat [ br, br ]
    in
    concat <|
        List.intersperse break
            [ button
                [ bulma.button, is.light, disabled context.firstStep ]
                [ icon "level-up-alt" ]
            , button
                [ bulma.button
                , is.danger
                , is.inverted
                , onClick (Delete context.reference)
                ]
                [ icon "trash" ]
            , button
                [ bulma.button, is.light, disabled context.lastStep ]
                [ icon "level-down-alt" ]
            ]


viewFooter : Element Msg
viewFooter =
    concat
        [ hr
        , el
            [ bulma.level ]
            [ el [ bulma.levelItem ] [ el [ bulma.buttons ] [ viewStepButton ] ] ]
        ]


viewStepButton : Element Msg
viewStepButton =
    button
        [ bulma.button, is.outlined, is.primary, onClick AddStep ]
        [ icon "tasks", el [] [ text "Add progression" ] ]


numberInput : (String -> msg) -> String -> Field Int -> Element msg
numberInput toMsg name n =
    el
        [ bulma.field ]
        [ input
            [ bulma.input
            , type_ "number"
            , placeholder name
            , value (Field.view n)
            , Html.Attributes.min "0"
            , style [ ( "width", "4rem" ) ]
            , is.warning |> when (Field.isInvalid n)
            , onInput toMsg
            ]
        ]


loadInput : Field String -> Steps.Reference -> Element Msg
loadInput content ref =
    el
        [ bulma.field ]
        [ input
            [ bulma.input
            , type_ "text"
            , placeholder "load(s)"
            , value (Field.view content)
            , style [ ( "width", "10rem" ) ]
            , onInput <| EditMovement ref << Load
            ]
        ]
