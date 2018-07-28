module Exercise exposing (Exercise, Movement, fromSheet)

import Array exposing (Array)
import Json.Decode as D


type alias Exercise =
    { name : String
    , features : List String
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

        headers :: exercises ->
            D.map7 Row
                (findHeader "name" name headers)
                (findHeader "sets" name headers)
                (findHeader "reps" name headers)
                (findHeader "load" name headers)
                (findHeader "rest" name headers)
                (findHeader "progression rate" name headers)
                (findHeader "notes" name headers)
                |> D.andThen
                    (\headers ->
                        case
                            exercises
                                |> List.map (parseMovement headers << Array.fromList)
                                |> List.foldr (Result.map2 (::)) (Ok [])
                        of
                            Err reason ->
                                badFormat name reason

                            Ok movements ->
                                D.succeed <| Exercise name [{- TODO -}] movements
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


badFormat : String -> String -> D.Decoder a
badFormat sheet because =
    D.fail <|
        "The sheet `"
            ++ sheet
            ++ "` is not formatted incorreclty, "
            ++ because
            ++ "!"
