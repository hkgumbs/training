module Exercise.Steps exposing (Context, Reference, Steps, addStep, empty, remove, view)


type Steps a
    = Steps a (List (Step a))


type Step a
    = Interval Int
    | Movements (List a)


type alias Context =
    { firstStep : Bool
    , lastStep : Bool
    , reference : Reference
    }


type Reference
    = Ref { step : Int, movement : Int }


view :
    { interval : Int -> html, movements : List ( Context, a ) -> html }
    -> Steps a
    -> List html
view config (Steps _ steps) =
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
                    config.interval n

                Movements movements ->
                    config.movements <|
                        List.indexedMap (withContext index) movements
        )
        steps


empty : a -> Steps a
empty default =
    Steps default <| [ Movements [ default ] ]


addStep : Steps a -> Steps a
addStep (Steps default steps) =
    Steps default <| steps ++ [ Interval 2, Movements [ default ] ]


remove : Reference -> Steps a -> Steps a
remove (Ref { step, movement }) (Steps default steps) =
    editMovementsAt step
        (\movements ->
            mapAt movement Just (\_ -> Nothing) movements
                |> List.filterMap identity
        )
        steps
        |> fix default


editMovementsAt : Int -> (List a -> List a) -> List (Step a) -> List (Step a)
editMovementsAt index f =
    mapAt index identity <|
        \step ->
            case step of
                Interval _ ->
                    step

                Movements movements ->
                    Movements <| f movements


fix : a -> List (Step a) -> Steps a
fix default original =
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
                    [ Movements [ default ] ]
    in
    Steps default <| collapse <| List.filter ((/=) (Movements [])) original


mapAt : Int -> (a -> b) -> (a -> b) -> List a -> List b
mapAt index default ifMatch =
    List.indexedMap <|
        \i x ->
            if i == index then
                ifMatch x
            else
                default x
