module Dashboard.Page exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)


type alias Model =
    {}


init : Global.Session -> Task Global.Error Model
init session =
    getState session
        |> Task.mapError (\_ -> Global.RequiresAuth)


getState : Global.Session -> Task Http.Error Model
getState session =
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
    div []
        [ div
            [ class "hero is-light" ]
            [ div
                [ class "hero-body" ]
                [ div
                    [ class "container" ]
                    [ h1 [ class "title" ] [ text "Progress" ]
                    , h2
                        [ class "subtitle" ]
                        [ text "Welcome to your Dashboard" ]
                    ]
                ]
            ]
        , section
            [ class "section" ]
            [ div
                [ class "container" ]
                [ text "__TODO__" ]
            ]
        ]
