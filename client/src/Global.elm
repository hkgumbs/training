module Global
    exposing
        ( Context
        , Error(..)
        , IdToken
        , authorize
        , context
        , encodeToken
        , parseToken
        )

import Http
import Json.Decode as D
import Json.Encode as E


type Error
    = RequiresAuth



-- AUTH


type alias Context =
    { dbapi : String
    , auth : Maybe IdToken
    }


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


authorize : Context -> Http.Header
authorize context =
    case context.auth of
        Nothing ->
            Http.header "" ""

        Just (IdToken raw) ->
            Http.header "Authorization" <| "Bearer " ++ raw



-- JSON


context : D.Decoder Context
context =
    D.map2 Context
        (D.field "dbapi" D.string)
        (D.maybe <| D.field "id_token" <| D.map IdToken D.string)


encodeToken : IdToken -> E.Value
encodeToken (IdToken raw) =
    E.object [ ( "id_token", E.string raw ) ]
