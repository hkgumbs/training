module Exercise.Field exposing (Field, int, isInvalid, nonEmptyString, string, view)


type Field a
    = Valid String a
    | Invalid String


int : String -> Field Int
int value =
    case String.toInt value of
        Err _ ->
            Invalid value

        Ok i ->
            Valid value i


nonEmptyString : String -> Field String
nonEmptyString value =
    if String.isEmpty value then
        Invalid value
    else
        Valid value value


string : String -> Field String
string value =
    Valid value value


view : Field a -> String
view value =
    case value of
        Valid raw _ ->
            raw

        Invalid raw ->
            raw


isInvalid : Field a -> Bool
isInvalid field =
    case field of
        Invalid _ ->
            True

        Valid _ _ ->
            False
