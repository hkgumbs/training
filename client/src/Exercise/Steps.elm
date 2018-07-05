module Exercise.Steps
    exposing
        ( Context
        , Reference
        , State
        , addStep
        , duplicate
        , editInterval
        , editMovement
        , empty
        , view
        )


type State interval movement
    = State interval movement (List (Step interval movement))


type Step interval movement
    = Interval interval
    | Movements (List movement)


type alias Context =
    { firstStep : Bool
    , lastStep : Bool
    , reference : Reference
    }


type Reference
    = Ref { step : Int, movement : Int }


view :
    { interval : Reference -> i -> html, movements : List ( Context, m ) -> html }
    -> State i m
    -> List html
view config (State _ _ steps) =
    let
        withContext step index movement =
            ( { firstStep = step == 0
              , lastStep = step == List.length steps - 1
              , reference = Ref { step = step, movement = index }
              }
            , movement
            )
    in
    List.indexedMap
        (\index step ->
            case step of
                Interval n ->
                    config.interval (Ref { step = index, movement = -1 }) n

                Movements movements ->
                    config.movements <|
                        List.indexedMap (withContext index) movements
        )
        steps


empty : interval -> movement -> State interval movement
empty defaultInterval defaultMovement =
    State defaultInterval defaultMovement <| [ Movements [ defaultMovement ] ]


addStep : State interval movement -> State interval movement
addStep (State defaultInterval defaultMovement steps) =
    State defaultInterval defaultMovement <|
        steps
            ++ [ Interval defaultInterval, Movements [ defaultMovement ] ]


editMovement : (movement -> movement) -> Reference -> State interval movement -> State interval movement
editMovement f (Ref { step, movement }) (State defaultInterval defaultMovement steps) =
    editAt step identity (mapAt movement identity f) steps
        |> State defaultInterval defaultMovement


editInterval : (interval -> interval) -> Reference -> State interval movement -> State interval movement
editInterval f (Ref { step, movement }) (State defaultInterval defaultMovement steps) =
    editAt step f identity steps
        |> State defaultInterval defaultMovement


duplicate : Reference -> State interval movement -> State interval movement
duplicate (Ref { step, movement }) (State defaultInterval defaultMovement steps) =
    editAt step
        identity
        (List.concat << mapAt movement List.singleton (\x -> [ x, x ]))
        steps
        |> State defaultInterval defaultMovement


editAt : Int -> (i -> i) -> (List m -> List m) -> List (Step i m) -> List (Step i m)
editAt index intervalMap movementsMap =
    mapAt index identity <|
        \step ->
            case step of
                Interval n ->
                    Interval <| intervalMap n

                Movements movements ->
                    Movements <| movementsMap movements


mapAt : Int -> (a -> b) -> (a -> b) -> List a -> List b
mapAt index default ifMatch =
    List.indexedMap <|
        \i x ->
            if i == index then
                ifMatch x
            else
                default x
