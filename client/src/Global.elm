module Global
    exposing
        ( Error(..)
        , IdToken
        , Session
        , encodeToken
        , header
        , parseToken
        , session
        )

import Http
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type Error
    = RequiresAuth



-- AUTH


type alias Session =
    { idToken : Maybe IdToken }


{-| Auth0 Redirect URLs can have these variables:

  - access_token=XXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXX
  - expires_in=7200
  - token_type=Bearer
  - state=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  - id_token=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXX

-}
type IdToken
    = IdToken String


parseToken : String -> Result String IdToken
parseToken =
    String.split "&"
        >> List.filterMap findToken
        >> List.head
        >> Result.fromMaybe "couldn't find the `id_token` parameter"


findToken : String -> Maybe IdToken
findToken chunk =
    case String.split "=" chunk of
        [ "id_token", value ] ->
            Just (IdToken value)

        _ ->
            Nothing



-- JSON


session : Decoder IdToken
session =
    Json.Decode.map IdToken
        (Json.Decode.field "id_token" Json.Decode.string)


encodeToken : IdToken -> Value
encodeToken (IdToken raw) =
    Json.Encode.object
        [ ( "id_token", Json.Encode.string raw )
        ]


header : IdToken -> Http.Header
header (IdToken raw) =
    Http.header "Authorization" <| "Bearer " ++ raw
