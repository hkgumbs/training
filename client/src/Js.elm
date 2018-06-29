port module Js exposing (login, logout, saveToken)

import Global
import Json.Encode exposing (Value)


saveToken : String -> Cmd msg
saveToken =
    outgoing << tag "SAVE-TOKEN" << Global.encodeToken


login : Cmd msg
login =
    outgoing <| tag "LOGIN" Json.Encode.null


logout : Cmd msg
logout =
    outgoing <| tag "LOGOUT" Json.Encode.null


tag : String -> Value -> Value
tag name data =
    Json.Encode.object
        [ ( "tag", Json.Encode.string name )
        , ( "data", data )
        ]


port outgoing : Value -> Cmd msg
