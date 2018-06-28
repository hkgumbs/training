module Dashboard.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Global
import Html.Events exposing (..)
import Http
import Js
import Task exposing (Task)


type alias Model =
    { authenticated : Bool
    }


init : Global.Context -> Task Global.Error Model
init context =
    Task.andThen
        (\_ ->
            Task.succeed
                { authenticated = context.auth /= Nothing
                }
        )
        (Http.request
            { method = "GET"
            , url = context.dbapi ++ "exercises"
            , headers = Global.authorize context
            , body = Http.emptyBody
            , expect = Http.expectStringResponse (always <| Ok ())
            , timeout = Nothing
            , withCredentials = False
            }
            |> Http.toTask
            |> Task.onError (\_ -> Task.succeed ())
        )


type Msg
    = NoOp
    | Login
    | Logout


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Login ->
            ( model, Js.login )

        Logout ->
            ( model, Js.logout )


view : Model -> Element a Msg
view model =
    if model.authenticated then
        button [ onClick Logout ] [ text "LOG OUT" ]
    else
        button [ onClick Login ] [ text "LOG IN" ]
