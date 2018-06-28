module Client.Data
    exposing
        ( Exercise
        , JointAction(..)
        , Load(..)
        , PlaneOfMotion(..)
        , Progression
        , Type(..)
        , dayString
        , days
        , exercise
        , jointActionString
        , planeOfMotionString
        , typeString
        )

import Date
import Json.Decode as D


type alias Exercise =
    { name : String
    , type_ : Type
    , planeOfMotion : PlaneOfMotion
    , jointAction : JointAction
    , progressions : List Progression
    }


type Type
    = HipDominent
    | KneeDominent
    | VerticalPush
    | VerticalPull
    | HorizontalPush
    | HorizontalPull


type PlaneOfMotion
    = Saggital
    | Frontal
    | Transverse


type JointAction
    = Multi


type alias Progression =
    { name : String
    , sets : Int
    , reps : Int
    , load : Maybe Load
    , rest : Int
    , progressionRate : Int
    , minimumFms : Maybe Int
    , option : Option
    }


type Load
    = Kgs Int
    | Lbs Int


type Option
    = Prep
    | ResistanceTraining


exercise : D.Decoder Exercise
exercise =
    D.map5 Exercise
        (D.field "name" D.string)
        (D.field "type" <| parse "TYPE" type_)
        (D.field "plane_of_motion" <| parse "PLANE OF MOTION" planeOfMotion)
        (D.field "joint_action" <| parse "JOINT ACTION" jointAction)
        (D.field "progressions" <| D.list progression)


type_ : String -> Maybe Type
type_ str =
    case str of
        "HIP_DOMINENT" ->
            Just HipDominent

        "KNEE_DOMINENT" ->
            Just KneeDominent

        "VERTICAL_PUSH" ->
            Just VerticalPush

        "VERTICAL_PULL" ->
            Just VerticalPull

        "HORIZONTAL_PUSH" ->
            Just HorizontalPush

        "HORIZONTAL_PULL" ->
            Just HorizontalPull

        _ ->
            Nothing


planeOfMotion : String -> Maybe PlaneOfMotion
planeOfMotion str =
    case str of
        "SAGGITAL" ->
            Just Saggital

        "FRONTAL" ->
            Just Frontal

        "TRANSVERSE" ->
            Just Transverse

        _ ->
            Nothing


jointAction : String -> Maybe JointAction
jointAction str =
    case str of
        "MULTI" ->
            Just Multi

        _ ->
            Nothing


progression : D.Decoder Progression
progression =
    D.map8 Progression
        (D.field "name" D.string)
        (D.field "sets" D.int)
        (D.field "reps" D.int)
        (D.field "load" <| D.nullable <| parse "LOAD" load)
        (D.field "rest" D.int)
        (D.field "progression_rate" D.int)
        (D.field "minimum_fms" <| D.nullable D.int)
        (D.field "option" <| parse "OPTION" option)


load : String -> Maybe Load
load str =
    case String.split " " str of
        [ n, "LBS" ] ->
            String.toInt n |> Result.toMaybe |> Maybe.map Lbs

        [ n, "KGS" ] ->
            String.toInt n |> Result.toMaybe |> Maybe.map Kgs

        _ ->
            Nothing


option : String -> Maybe Option
option str =
    case str of
        "MOVEMENT_PREP" ->
            Just Prep

        "RESISTANCE_TRAINING" ->
            Just ResistanceTraining

        _ ->
            Nothing


parse : String -> (String -> Maybe a) -> D.Decoder a
parse tag func =
    D.andThen
        (\str ->
            case func str of
                Just value ->
                    D.succeed value

                Nothing ->
                    D.fail <| "`" ++ str ++ "` is not a valid " ++ tag
        )
        D.string


days : D.Decoder (List Date.Day)
days =
    D.andThen (String.toList >> parseDay []) D.string


parseDay : List Date.Day -> List Char -> D.Decoder (List Date.Day)
parseDay acc chars =
    case chars of
        [] ->
            D.succeed (List.reverse acc)

        'M' :: rest ->
            parseDay (Date.Mon :: acc) rest

        'T' :: 'u' :: rest ->
            parseDay (Date.Tue :: acc) rest

        'W' :: rest ->
            parseDay (Date.Wed :: acc) rest

        'T' :: 'h' :: rest ->
            parseDay (Date.Thu :: acc) rest

        'F' :: rest ->
            parseDay (Date.Fri :: acc) rest

        'S' :: 'a' :: rest ->
            parseDay (Date.Sat :: acc) rest

        'S' :: 'u' :: rest ->
            parseDay (Date.Sun :: acc) rest

        _ ->
            D.fail <| "`" ++ String.fromList chars ++ "` is not a valid DAY"



-- TO STRING


typeString : Type -> String
typeString value =
    case value of
        HipDominent ->
            "hip-dominent"

        KneeDominent ->
            "knee-dominent"

        VerticalPush ->
            "vertical push"

        VerticalPull ->
            "vertical pull"

        HorizontalPull ->
            "horizontal pull"

        HorizontalPush ->
            "horizontal push"


planeOfMotionString : PlaneOfMotion -> String
planeOfMotionString value =
    case value of
        Saggital ->
            "saggital"

        Frontal ->
            "frontal"

        Transverse ->
            "transverse"


jointActionString : JointAction -> String
jointActionString value =
    case value of
        Multi ->
            "multi-joint"


dayString : Date.Day -> String
dayString day =
    case day of
        Date.Mon ->
            "M"

        Date.Tue ->
            "Tu"

        Date.Wed ->
            "W"

        Date.Thu ->
            "Th"

        Date.Fri ->
            "F"

        Date.Sat ->
            "Sa"

        Date.Sun ->
            "Su"
