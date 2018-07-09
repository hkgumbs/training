module Settings.Page exposing (Model, Msg, init, update, view)

import Global
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
        button [ bulma.button, onClick Logout ] [ text "LOG OUT" ]
    else
        button [ bulma.button, onClick Login ] [ text "LOG IN" ]
