module Exercise exposing (Exercise, Movement, fromSheet)

import Array exposing (Array)
import Json.Decode as D


type alias Exercise =
    { name : String
    , tags : List String
    , movements : List Movement
    }


type alias Movement =
    Row String Int


type alias Headers =
    Row Int Int


type alias Row string int =
    { name : string
    , sets : int
    , reps : int
    , load : string
    , rest : int
    , progressionRate : int
    , notes : string
    }


fromSheet : D.Decoder (List Exercise)
fromSheet =
    D.andThen (List.foldr (D.map2 (::)) (D.succeed []))
        (D.map2 (List.map2 parseExercise)
            (D.field "names" (D.list D.string))
            (D.field "exercises" (D.list (D.list (D.list D.string))))
        )


parseExercise : String -> List (List String) -> D.Decoder Exercise
parseExercise name table =
    case table of
        [] ->
            badFormat name "since this it is empty"

        firstRow :: otherRows ->
            D.map7 Row
                (findHeader "name" name firstRow)
                (findHeader "sets" name firstRow)
                (findHeader "reps" name firstRow)
                (findHeader "load" name firstRow)
                (findHeader "rest" name firstRow)
                (findHeader "progression rate" name firstRow)
                (findHeader "notes" name firstRow)
                |> D.andThen
                    (\headers ->
                        let
                            arrays =
                                List.map Array.fromList otherRows
                        in
                        case
                            arrays
                                |> List.map (parseMovement headers)
                                |> List.foldr (Result.map2 (::)) (Ok [])
                        of
                            Err reason ->
                                badFormat name reason

                            Ok movements ->
                                D.map3 Exercise
                                    (D.succeed name)
                                    (findTags name firstRow arrays)
                                    (D.succeed movements)
                    )


parseMovement : Headers -> Array String -> Result String Movement
parseMovement headers data =
    Result.map4
        (\sets reps rest progressionRate ->
            { sets = sets
            , reps = reps
            , rest = rest
            , progressionRate = progressionRate
            , name = findCell headers.name data
            , load = findCell headers.load data
            , notes = findCell headers.notes data
            }
        )
        (String.toInt <| findCell headers.sets data)
        (String.toInt <| findCell headers.reps data)
        (String.toInt <| findCell headers.rest data)
        (String.toInt <| findCell headers.progressionRate data)


findCell : Int -> Array String -> String
findCell index =
    Array.get index >> Maybe.withDefault ""


findHeader : String -> String -> List String -> D.Decoder Int
findHeader name sheet headers =
    findHeaderHelp name sheet headers 0


findHeaderHelp : String -> String -> List String -> Int -> D.Decoder Int
findHeaderHelp name sheet headers index =
    case headers of
        [] ->
            badFormat sheet <|
                "since it does not have a `"
                    ++ name
                    ++ "` column!"

        next :: rest ->
            if next == name then
                D.succeed index
            else
                findHeaderHelp name sheet rest (index + 1)


findTags : String -> List String -> List (Array String) -> D.Decoder (List String)
findTags name firstRow arrays =
    findHeader "tags" name firstRow
        |> D.map
            (\index ->
                List.map (findCell index) arrays
                    |> List.filter (not << String.isEmpty)
            )


badFormat : String -> String -> D.Decoder a
badFormat sheet because =
    D.fail <|
        "The sheet `"
            ++ sheet
            ++ "` is not formatted incorreclty, "
            ++ because
            ++ "!"
