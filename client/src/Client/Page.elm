module Client.Page exposing (Model, Msg, init, update, view)

import Bulma exposing (..)
import Global
import Html.Events exposing (..)
import Task exposing (Task)


type alias Model =
    { clientName : String
    }


init : Global.Context -> String -> Task Global.Error Model
init context id =
    Task.succeed
        { clientName = "Mrs. Incredible"
        }


type Msg
    = NoOp


update : Global.Context -> Msg -> Model -> ( Model, Cmd Msg )
update context msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Element msg
view model =
    tile
        [ Tile [ width.is4 ]
            [ Children
                [ notification
                    [ background.light ]
                    [ title model.clientName ]
                ]
            ]
        , Tile [ width.is8 ] [ Children [ text "__SCHEDULE__" ] ]
        ]
