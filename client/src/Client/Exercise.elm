module Client.Exercise
    exposing
        ( Exercise
        , JointAction(..)
        , PlaneOfMotion(..)
        , Type(..)
        , exercise
        , jointActionString
        , planeOfMotionString
        , typeString
        )

import Json.Decode as D


type alias Exercise =
    { name : String
    , type_ : Type
    , planeOfMotion : PlaneOfMotion
    , jointAction : JointAction
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


exercise : D.Decoder Exercise
exercise =
    D.map4 Exercise
        (D.field "name" D.string)
        (D.field "type" <| parse "TYPE" type_)
        (D.field "plane_of_motion" <| parse "PLANE OF MOTION" planeOfMotion)
        (D.field "joint_action" <| parse "JOINT ACTION" jointAction)


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
