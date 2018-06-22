module Global
    exposing
        ( Context
        , Error(..)
        , Token
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
    , auth : Maybe Token
    }


{-| Auth0 Redirect URLs can have these variables:

  - access_token=XXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXX
  - expires_in=7200
  - token_type=Bearer
  - state=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  - id_token=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXX

-}
type Token
    = Token String


parseToken : String -> Result String Token
parseToken =
    String.split "&"
        >> List.filterMap findToken
        >> List.head
        >> Result.fromMaybe "couldn't find the `id_token` parameter"


findToken : String -> Maybe Token
findToken chunk =
    case String.split "=" chunk of
        [ "id_token", value ] ->
            Just (Token value)

        _ ->
            Nothing


authorize : Context -> Http.Header
authorize context =
    case context.auth of
        Nothing ->
            Http.header "" ""

        Just (Token raw) ->
            Http.header "Authorization" <| "Bearer " ++ raw



-- JSON


context : D.Decoder Context
context =
    D.map2 Context
        (D.field "dbapi" D.string)
        (D.maybe <| D.field "token" <| D.map Token D.string)


encodeToken : Token -> E.Value
encodeToken (Token raw) =
    E.string raw
