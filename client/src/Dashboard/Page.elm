module Dashboard.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Global
import Task exposing (Task)


type alias Model =
    {}


init : Global.Context -> Task Global.Error Model
init context =
    Task.succeed {}


type Msg
    = NoOp


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Element msg
view model =
    box [] [ text "DASHBOARD" ]
