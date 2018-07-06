module Exercise.Page exposing (Model, Msg, init, subscriptions, update, view)

import Exercise.Field as Field exposing (Field)
import Exercise.Steps as Steps
import Global
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Mouse
import PostgRest as PG
import Resources
import Task exposing (Task)
import Ui exposing (..)


type alias Model =
    { exercise : Exercise
    , steps : Steps.State Interval Movement
    , hover : Maybe Steps.Reference
    , drag : Maybe Steps.Reference
    , cursor : Mouse.Position
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
    Task.succeed Model
        |> andMap (pg context (getExercise id))
        |> andMap (Task.succeed (Steps.empty newInterval newMovement))
        |> andMap (Task.succeed Nothing)
        |> andMap (Task.succeed Nothing)
        |> andMap (Task.succeed <| Mouse.Position 0 0)


andMap : Task x a -> Task x (a -> b) -> Task x b
andMap =
    Task.map2 (|>)


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
    = AddStep
    | MouseMove Mouse.Position
    | SetHover (Maybe Steps.Reference)
    | Drag (Maybe Steps.Reference)
    | Duplicate Steps.Reference
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
        AddStep ->
            pure { model | steps = Steps.addStep model.steps }

        MouseMove position ->
            pure { model | cursor = position }

        SetHover ref ->
            pure { model | hover = ref }

        Drag ref ->
            pure <| { model | drag = ref }

        Duplicate ref ->
            pure { model | steps = Steps.duplicate ref model.steps }

        Delete ref ->
            pure { model | steps = Steps.delete ref model.steps }

        EditMovement ref change ->
            pure { model | steps = Steps.editMovement (apply change) ref model.steps }

        EditInterval ref new ->
            pure { model | steps = Steps.editInterval (\_ -> Field.int new) ref model.steps }


drag : { a | drag : Bool } -> { a | drag : Bool }
drag record =
    { record | drag = True }


drop : { a | drag : Bool } -> { a | drag : Bool }
drop record =
    { record | drag = False }


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


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves MouseMove
        , Mouse.ups (\_ -> Drag Nothing)
        ]


view : Model -> Element Msg
view model =
    el
        [ bulma.section
        , cursorGrabbing |> when (model.drag /= Nothing)
        ]
        [ el
            [ bulma.container ]
            [ el
                [ bulma.columns ]
                [ el
                    [ bulma.column ]
                    [ viewHeader model.exercise, viewSteps model, viewFooter ]
                ]
            ]
        , viewGhost model.drag model.cursor
        ]


viewGhost : Maybe Steps.Reference -> Mouse.Position -> Element Msg
viewGhost ref cursor =
    case ref of
        Nothing ->
            empty

        Just _ ->
            el
                [ has.backgroundPrimary
                , style
                    [ ( "top", px cursor.y )
                    , ( "left", px cursor.x )
                    , ( "width", "5rem" )
                    , ( "height", "5rem" )
                    , ( "position", "absolute" )
                    ]
                ]
                []


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


viewSteps : Model -> Element Msg
viewSteps model =
    concat <|
        Steps.view
            { interval = viewInterval
            , movements = List.map (viewMovement model) >> el [ bulma.level ]
            }
            model.steps


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


viewMovement : Model -> ( Steps.Context, Movement ) -> Element Msg
viewMovement { hover, drag, cursor } ( context, movement ) =
    el
        [ bulma.levelItem ]
        [ el
            [ bulma.box
            , cursorGrab
            , draggable "true"
            , faded |> when (drag == Just context.reference)
            , onDragStart <| Drag (Just context.reference)
            , onMouseLeave <| SetHover Nothing
            , onMouseEnter <| SetHover (Just context.reference)
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
                    , when
                        (hover /= Just context.reference || drag /= Nothing)
                        is.invisible
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
    concat
        [ button
            [ bulma.button
            , is.white
            , onClick (Duplicate context.reference)
            , style [ ( "margin-bottom", "1rem" ) ]
            ]
            [ icon "clone" ]
        , br
        , button
            [ bulma.button
            , is.danger
            , is.inverted
            , onClick (Delete context.reference)
            ]
            [ icon "trash" ]
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


onDragStart : msg -> Attribute msg
onDragStart msg =
    onWithOptions "dragstart"
        { preventDefault = True, stopPropagation = True }
        (D.succeed msg)


cursorGrab : Attribute msg
cursorGrab =
    style
        [ ( "cursor", "grab" )
        , ( "cursor", "-webkit-grab" )
        ]


cursorGrabbing : Attribute msg
cursorGrabbing =
    style
        [ ( "cursor", "grabbing" )
        , ( "cursor", "-webkit-grabbing" )
        ]


faded : Attribute msg
faded =
    style
        [ ( "opacity", "0.4" )
        ]


px : Int -> String
px n =
    toString n ++ "px"
