module Client.Page exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Task exposing (Task)


type alias Model =
    {}


init : Global.Session -> String -> Task Global.Error Model
init session id =
    Task.succeed {}


type Msg
    = NoOp


update : Global.Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    div [] [ text "CLIENT" ]
