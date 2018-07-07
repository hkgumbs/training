module Exercise.Page exposing (Model, Msg, init, subscriptions, update, view)

import Animation
import Animation.Messenger as AM
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
    , drag : DragState Steps.Reference
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


type alias DragState a =
    { subject : Maybe a
    , position : Mouse.Position
    , original : AM.State (DragMsg a)
    , cursor : AM.State (DragMsg a)
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    let
        fromExercise exercise =
            { exercise = exercise
            , steps = Steps.empty newInterval newMovement
            , hover = Nothing
            , drag = initialDrag
            }
    in
    Task.map fromExercise
        (pg context (getExercise id))


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


initialDrag : DragState a
initialDrag =
    { subject = Nothing
    , position = Mouse.Position 0 0
    , original = Animation.style opaque
    , cursor = Animation.style gone
    }


type Msg
    = DragMsg (DragMsg Steps.Reference)
    | AddStep
    | SetHover (Maybe Steps.Reference)
    | Duplicate Steps.Reference
    | Delete Steps.Reference
    | EditMovement Steps.Reference Change
    | EditInterval Steps.Reference String


type Change
    = Name String
    | Sets String
    | Reps String
    | Load String


type DragMsg a
    = Start a Mouse.Position
    | Stop
    | Reset
    | MouseMove Mouse.Position
    | Animate Animation.Msg


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        DragMsg dragMsg ->
            updateDrag dragMsg model.drag
                |> Tuple.mapFirst (\drag -> { model | drag = drag })
                |> Tuple.mapSecond (Cmd.map DragMsg)

        AddStep ->
            pure { model | steps = Steps.addStep model.steps }

        SetHover ref ->
            pure { model | hover = ref }

        Duplicate ref ->
            pure { model | steps = Steps.duplicate ref model.steps }

        Delete ref ->
            pure { model | steps = Steps.delete ref model.steps }

        EditMovement ref change ->
            pure { model | steps = Steps.editMovement (apply change) ref model.steps }

        EditInterval ref new ->
            pure { model | steps = Steps.editInterval (\_ -> Field.int new) ref model.steps }


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


updateDrag : DragMsg a -> DragState a -> ( DragState a, Cmd (DragMsg a) )
updateDrag msg state =
    case msg of
        Start subject position ->
            ( { state
                | subject = Just subject
                , position = position
                , cursor = Animation.interrupt [ Animation.to opaque ] state.cursor
                , original = Animation.interrupt [ Animation.to faded ] state.original
              }
            , Cmd.none
            )

        Stop ->
            ( { state
                | cursor = Animation.interrupt [ Animation.to gone ] state.cursor
                , original = Animation.interrupt [ Animation.to opaque, AM.send Reset ] state.original
              }
            , Cmd.none
            )

        Reset ->
            ( { state | subject = Nothing }, Cmd.none )

        MouseMove position ->
            ( { state | position = position }, Cmd.none )

        Animate tick ->
            let
                ( cursor, a ) =
                    AM.update tick state.cursor

                ( original, b ) =
                    AM.update tick state.original
            in
            ( { state | original = original, cursor = cursor }, Cmd.batch [ a, b ] )


opaque : List Animation.Property
opaque =
    [ Animation.opacity 1.0, Animation.scale 1, Animation.grayscale 0 ]


faded : List Animation.Property
faded =
    [ Animation.opacity 0.6, Animation.scale 0.8, Animation.grayscale 100 ]


gone : List Animation.Property
gone =
    [ Animation.opacity 0.0, Animation.scale 0.6 ]


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DragMsg <|
        Sub.batch
            [ Mouse.ups (\_ -> Stop)
            , Animation.subscription Animate [ model.drag.original, model.drag.cursor ]
            , if model.drag.subject == Nothing then
                Sub.none
              else
                Mouse.moves MouseMove
            ]


view : Model -> Element Msg
view model =
    el
        [ bulma.section ]
        [ el
            [ bulma.container ]
            [ viewHeader model.exercise
            , viewSteps model
            , viewFooter
            ]
        , viewGhost model.drag
        ]


viewGhost : DragState Steps.Reference -> Element Msg
viewGhost { subject, cursor, position } =
    if subject == Nothing then
        empty
    else
        concat
            [ stylesheet """
                * {
                  cursor: grabbing !important;
                  cursor: -moz-grabbing !important;
                  cursor: -webkit-grabbing !important;
                }
              """
            , button
                (Animation.render cursor
                    ++ [ bulma.button
                       , is.info
                       , is.hovered
                       , is.large
                       , style
                            [ ( "top", px position.y )
                            , ( "left", px position.x )
                            , ( "position", "absolute" )
                            ]
                       ]
                )
                [ icon "dumbbell" ]
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


viewSteps : Model -> Element Msg
viewSteps model =
    concat <|
        Steps.view
            { interval = viewInterval
            , movements =
                List.map (viewMovement model)
                    >> List.intersperse viewOr
                    >> el
                        [ style
                            [ ( "display", "flex" )
                            , ( "flex-wrap", "nowrap" )
                            , ( "overflow-x", "auto" )
                            , ( "align-items", "center" )
                            , ( "padding", "1rem" )
                            ]
                        ]
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
viewMovement { hover, drag } ( context, movement ) =
    el
        [ style
            [ ( "flex", "0 0 auto" )
            , ( "margin", "0 auto" )
            ]
        ]
        [ el
            (animateSubject context drag
                ++ [ bulma.box
                   , cursorGrab
                   , draggable "true"
                   , onDragStart <| DragMsg << Start context.reference
                   , onMouseLeave <| SetHover Nothing
                   , onMouseEnter <| SetHover (Just context.reference)
                   ]
            )
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
                        (hover /= Just context.reference || drag.subject /= Nothing)
                        is.invisible
                    ]
                    [ viewMovementActions context ]
                ]
            ]
        ]


viewOr : Element Msg
viewOr =
    el
        [ is.italic, is.uppercase, style [ ( "padding", "0 1rem" ) ] ]
        [ text "or" ]


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


onDragStart : (Mouse.Position -> msg) -> Attribute msg
onDragStart toMsg =
    onWithOptions "dragstart"
        { preventDefault = True, stopPropagation = True }
        (D.map2 (\x y -> toMsg (Mouse.Position x y))
            (D.field "clientX" D.int)
            (D.field "clientY" D.int)
        )


animateSubject : Steps.Context -> DragState Steps.Reference -> List (Attribute msg)
animateSubject context drag =
    if drag.subject == Just context.reference then
        Animation.render drag.original
    else
        []


cursorGrab : Attribute msg
cursorGrab =
    style
        [ ( "cursor", "grab" )
        , ( "cursor", "-moz-grab" )
        , ( "cursor", "-webkit-grab" )
        ]


px : Int -> String
px n =
    toString n ++ "px"
