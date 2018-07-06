module Exercise.Steps
    exposing
        ( Context
        , Diff
        , Reference
        , State
        , addStep
        , delete
        , duplicate
        , editInterval
        , editMovement
        , empty
        , move
        , next
        , previous
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


type Diff
    = Diff Int


next : Diff
next =
    Diff 2


previous : Diff
previous =
    Diff -2


view :
    { interval : Reference -> i -> html
    , movements : List ( Context, m ) -> html
    }
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
editMovement transform (Ref { step, movement }) =
    editAt step identity (mapAt movement identity transform)


editInterval : (interval -> interval) -> Reference -> State interval movement -> State interval movement
editInterval transform (Ref { step }) =
    editAt step transform identity


duplicate : Reference -> State interval movement -> State interval movement
duplicate (Ref { step, movement }) =
    editAt step
        identity
        (mapAt movement List.singleton (\x -> [ x, x ]) >> List.concat)


delete : Reference -> State interval movement -> State interval movement
delete (Ref { step, movement }) =
    editAt step identity (mapAt movement List.singleton (\_ -> []) >> List.concat)
        >> fix


move : Diff -> movement -> Reference -> State interval movement -> State interval movement
move (Diff by) new (Ref { step, movement }) =
    editAt step identity (mapAt movement List.singleton (\_ -> []) >> List.concat)
        >> editAt (step + by) identity (\old -> old ++ [ new ])
        >> fix


editAt : Int -> (i -> i) -> (List m -> List m) -> State i m -> State i m
editAt index intervalMap movementsMap (State defaultInterval defaultMovement steps) =
    editAtHelp index intervalMap movementsMap steps
        |> State defaultInterval defaultMovement


editAtHelp : Int -> (i -> i) -> (List m -> List m) -> List (Step i m) -> List (Step i m)
editAtHelp index intervalMap movementsMap =
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


fix : State interval movement -> State interval movement
fix (State defaultInterval defaultMovement original) =
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
                    [ Movements [ defaultMovement ] ]
    in
    List.filter ((/=) (Movements [])) original
        |> collapse
        |> State defaultInterval defaultMovement
