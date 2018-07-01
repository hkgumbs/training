module Dashboard.Page exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Events exposing (..)
import Js
import Task exposing (Task)
import Ui exposing (..)


type alias Model =
    { authenticated : Bool
    }


init : Global.Context -> Task Global.Error Model
init context =
    Task.succeed
        { authenticated = context.auth /= Nothing
        }


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


view : Model -> Element Msg
view model =
    if model.authenticated then
        html button [ onClick Logout ] [ Ui.text "LOG OUT" ]
    else
        html button [ onClick Login ] [ Ui.text "LOG IN" ]
