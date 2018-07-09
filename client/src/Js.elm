port module Js exposing (getSheet, login, logout)

import Json.Encode as E


login : Cmd msg
login =
    outgoing <| tag "LOGIN" E.null


logout : Cmd msg
logout =
    outgoing <| tag "LOGOUT" E.null


getSheet : String -> Cmd msg
getSheet id =
    outgoing <| tag "GET-SHEET" <| E.string id


tag : String -> E.Value -> E.Value
tag name data =
    E.object
        [ ( "tag", E.string name )
        , ( "data", data )
        ]


port outgoing : E.Value -> Cmd msg
