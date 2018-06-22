module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Global
import Html
import Html.Attributes
import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Dashboard
    | Client String
    | TokenRedirect Global.Token


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Dashboard
    else
        parseHash route location


route : Parser (Route -> a) a
route =
    oneOf
        [ map Dashboard <| s ""
        , map Client <| s "client" </> string
        , map TokenRedirect <| custom "ID TOKEN" Global.parseToken
        ]


modifyUrl : Route -> Cmd msg
modifyUrl =
    Navigation.modifyUrl << hash


href : Route -> Html.Attribute msg
href =
    Html.Attributes.href << hash


hash : Route -> String
hash route =
    case route of
        Dashboard ->
            "#/"

        Client id ->
            "#/client/" ++ id

        TokenRedirect _ ->
            "#/"
