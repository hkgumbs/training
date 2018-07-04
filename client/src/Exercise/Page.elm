module Exercise.Page exposing (Model, Msg, init, update, view)

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
    , steps : List Step
    , hover : Maybe Reference
    }


type alias Exercise =
    { name : String }


type Step
    = Movements (List Movement)
    | Interval Int


type alias Movement =
    { name : String
    , sets : Int
    , reps : Int
    , load : String
    }


type alias Reference =
    { step : Int, movement : Int }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.map3 Model
        (pg context (getExercise id))
        (Task.succeed [ newMovements ])
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
    | SetHover (Maybe Reference)
    | Delete Reference


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddStep ->
            ( { model | steps = model.steps ++ [ newInterval, newMovements ] }, Cmd.none )

        SetHover ref ->
            ( { model | hover = ref }, Cmd.none )

        Delete ref ->
            ( { model | steps = fixSteps <| removeMovement ref model.steps }, Cmd.none )


newInterval : Step
newInterval =
    Interval 2


newMovements : Step
newMovements =
    Movements [ { name = "", reps = 10, sets = 1, load = "" } ]


removeMovement : Reference -> List Step -> List Step
removeMovement { step, movement } =
    editMovementsAt step <|
        \movements ->
            mapAt movement Just (\_ -> Nothing) movements
                |> List.filterMap identity


editMovementsAt : Int -> (List Movement -> List Movement) -> List Step -> List Step
editMovementsAt index f =
    mapAt index identity <|
        \step ->
            case step of
                Interval _ ->
                    step

                Movements movements ->
                    Movements <| f movements


fixSteps : List Step -> List Step
fixSteps =
    let
        collapse steps =
            case steps of
                -- alternate movements
                ((Movements _) as a) :: ((Interval _) as b) :: (((Movements _) :: _) as rest) ->
                    a :: b :: collapse rest

                -- end with movements
                (Movements _) :: [] ->
                    steps

                -- skip trailing intervals
                ((Movements _) as a) :: (Interval _) :: rest ->
                    collapse (a :: rest)

                -- skip leading intervals
                _ :: rest ->
                    collapse rest

                -- ensure at least one movement
                [] ->
                    [ newMovements ]
    in
    List.filter ((/=) (Movements [])) >> collapse


mapAt : Int -> (a -> b) -> (a -> b) -> List a -> List b
mapAt index default ifMatch =
    List.indexedMap <|
        \i x ->
            if i == index then
                ifMatch x
            else
                default x


view : Model -> Element Msg
view model =
    el
        [ bulma.columns ]
        [ el
            [ bulma.column ]
            [ viewHeader model.exercise
            , concat <|
                List.indexedMap
                    (viewStep model.hover (List.length model.steps))
                    model.steps
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


viewStep : Maybe Reference -> Int -> Int -> Step -> Element Msg
viewStep hover total index step =
    case step of
        Movements movements ->
            List.indexedMap
                (viewMovement hover (total == index + 1) index)
                movements
                |> List.intersperse viewOr
                |> el [ bulma.level ]

        Interval weeks ->
            viewInterval weeks


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


viewMovement : Maybe Reference -> Bool -> Int -> Int -> Movement -> Element Msg
viewMovement hover atEnd step index movement =
    let
        here =
            { step = step, movement = index }
    in
    el
        [ bulma.levelItem ]
        [ el
            [ bulma.box
            , onMouseLeave (SetHover Nothing)
            , onMouseEnter (SetHover <| Just here)
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
                    , is.invisible |> when (hover /= Just here)
                    ]
                    [ viewMovementActions atEnd here ]
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


viewMovementActions : Bool -> Reference -> Element Msg
viewMovementActions atEnd here =
    let
        break =
            concat [ br, br ]
    in
    concat <|
        List.intersperse break
            [ button
                [ bulma.button, is.light, disabled <| here.step == 0 ]
                [ icon "level-up-alt" ]
            , button
                [ bulma.button
                , is.danger
                , is.inverted
                , onClick (Delete here)
                ]
                [ icon "trash" ]
            , button
                [ bulma.button, is.light, disabled atEnd ]
                [ icon "level-down-alt" ]
            ]


viewFooter : Element Msg
viewFooter =
    concat
        [ hr
        , el
            [ bulma.level ]
            [ el
                [ bulma.levelItem ]
                [ el [ bulma.buttons ] [ viewStepButton ] ]
            ]
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
