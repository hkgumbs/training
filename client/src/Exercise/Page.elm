module Exercise.Page exposing (Model, Msg, init, update, view)

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
    , steps : Steps.Steps Movement
    , hover : Maybe Steps.Reference
    }


type alias Exercise =
    { name : String }


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context (getExercise id))
        (Task.succeed (Steps.empty { name = "", sets = 10, reps = 1, load = "" }))
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


type Msg
    = NoOp
    | AddStep
    | SetHover (Maybe Steps.Reference)
    | Delete Steps.Reference


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddStep ->
            ( { model | steps = Steps.addStep model.steps }, Cmd.none )

        SetHover ref ->
            ( { model | hover = ref }, Cmd.none )

        Delete ref ->
            ( { model | steps = Steps.remove ref model.steps }, Cmd.none )


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


viewSteps : Maybe Steps.Reference -> Steps.Steps Movement -> Element Msg
viewSteps hover =
    Steps.view
        { interval = viewInterval
        , movements =
            List.map (viewMovement hover)
                >> List.intersperse viewOr
                >> el [ bulma.level ]
        }
        >> concat


viewInterval : Int -> Element msg
viewInterval weeks =
    el
        [ bulma.notification, has.backgroundLight ]
        [ el
            [ bulma.level ]
            [ el [ bulma.levelItem ] <|
                List.intersperse nbsp
                    [ text "progress in ", numberInput "" weeks, text " weeks" ]
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
                    , viewMovementName movement.name
                    , viewMovementDetails movement.sets movement.reps movement.load
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


viewMovementName : String -> Element msg
viewMovementName name =
    el
        [ bulma.field ]
        [ el
            [ bulma.control ]
            [ input
                [ bulma.input
                , is.medium
                , type_ "text"
                , placeholder "Movement name"
                , value name
                ]
            ]
        ]


viewMovementDetails : Int -> Int -> String -> Element msg
viewMovementDetails sets reps load =
    el
        [ bulma.level ]
        [ el [ bulma.levelItem ] [ numberInput "sets" sets ]
        , el [ bulma.levelItem ] [ nbsp, text "×", nbsp ]
        , el [ bulma.levelItem ] [ numberInput "reps" reps ]
        , el [ bulma.levelItem ] [ nbsp, text "@", nbsp ]
        , el [ bulma.levelItem ] [ loadInput load ]
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


numberInput : String -> Int -> Element msg
numberInput name n =
    el
        [ bulma.field ]
        [ input
            [ bulma.input
            , type_ "number"
            , placeholder name
            , value (toString n)
            , style [ ( "width", "4rem" ) ]
            ]
        ]


loadInput : String -> Element msg
loadInput content =
    el
        [ bulma.field ]
        [ input
            [ bulma.input
            , type_ "text"
            , placeholder "load(s)"
            , value content
            , style [ ( "width", "10rem" ) ]
            ]
        ]
