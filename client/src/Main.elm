module Main exposing (..)

import Dashboard.Page
import Global
import Html
import Js
import Json.Decode exposing (Value)
import Navigation
import Route


type alias Model =
    { page : Page
    , session : Global.Session
    }


type Page
    = Blank
      -- | Client Client.Page.Model
    | Dashboard Dashboard.Page.Model


init : Value -> Navigation.Location -> ( Model, Cmd Msg )
init value location =
    let
        token =
            Json.Decode.decodeValue Global.session value
                |> Result.toMaybe
    in
    goTo (Route.fromLocation location)
        { page = Blank
        , session = Global.Session token
        }


type Msg
    = SetRoute (Maybe Route.Route)
    | Error Global.Error
    | Logout
    | Loaded Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SetRoute destination, _ ) ->
            goTo destination model

        ( Error Global.RequiresAuth, _ ) ->
            ( model, Js.login )

        ( Logout, _ ) ->
            ( model, Js.logout )

        ( Loaded page, _ ) ->
            ( { model | page = page }, Cmd.none )


goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo destination ({ session } as model) =
    case destination of
        Nothing ->
            ( model, Cmd.none )

        Just (Route.TokenRedirect idToken) ->
            ( { model | session = { session | idToken = Just idToken } }
            , Cmd.batch [ Js.saveToken idToken, Route.modifyUrl Route.Dashboard ]
            )

        _ ->
            Debug.crash {- TODO -} "__other pages__"


load : (a -> Page) -> Result Global.Error a -> Msg
load page result =
    case result of
        Err type_ ->
            Error type_

        Ok data ->
            Loaded (page data)


view : Model -> Html.Html Msg
view { page } =
    case page of
        Blank ->
            Html.text ""

        Dashboard model ->
            Dashboard.Page.view model


main : Program Value Model Msg
main =
    Navigation.programWithFlags
        (Route.fromLocation >> SetRoute)
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
