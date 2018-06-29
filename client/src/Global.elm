module Global
    exposing
        ( Context
        , Error(..)
        , context
        , encodeToken
        , httpError
        , parseToken
        )

import Http
import Json.Decode as D
import Json.Encode as E


type Error
    = RequiresAuth
    | AppError



-- AUTH


type alias Context =
    { dbapi : String
    , auth : Maybe String
    }


{-| Auth0 Redirect URLs can have these variables:

  - access_token=XXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXX
  - expires_in=7200
  - token_type=Bearer
  - state=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  - id_token=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-XXXXXXXXXXXXXXXXXXXX

-}
parseToken : String -> Result String String
parseToken =
    String.split "&"
        >> List.filterMap findToken
        >> List.head
        >> Result.fromMaybe "couldn't find the `id_token` parameter"


findToken : String -> Maybe String
findToken chunk =
    case String.split "=" chunk of
        [ "id_token", value ] ->
            Just value

        _ ->
            Nothing


httpError : Http.Error -> Error
httpError error =
    case error of
        Http.BadStatus { status } ->
            if status.code == 403 then
                RequiresAuth
            else
                AppError

        _ ->
            AppError



-- JSON


context : D.Decoder Context
context =
    D.map2 Context
        (D.field "dbapi" D.string)
        (D.maybe <| D.field "token" D.string)


encodeToken : String -> E.Value
encodeToken raw =
    E.string raw
