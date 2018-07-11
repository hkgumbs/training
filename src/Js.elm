port module Js exposing (login, logout, pickSheet)

import Json.Encode as E


login : Cmd msg
login =
    outgoing <| tag "LOGIN" E.null


logout : Cmd msg
logout =
    outgoing <| tag "LOGOUT" E.null


pickSheet : String -> Cmd msg
pickSheet id =
    outgoing <| tag "PICK-SHEET" <| E.string id


tag : String -> E.Value -> E.Value
tag name data =
    E.object
        [ ( "tag", E.string name )
        , ( "data", data )
        ]


port outgoing : E.Value -> Cmd msg
