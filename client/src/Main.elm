module Main exposing (..)

import Client.Page
import Dashboard.Page
import Global
import Html
import Js
import Json.Decode exposing (Value)
import Navigation
import Route
import Task


type alias Model =
    { page : Page
    , context : Global.Context
    }


type Page
    = Blank
    | Client Client.Page.Model
    | Dashboard Dashboard.Page.Model


init : Value -> Navigation.Location -> ( Model, Cmd Msg )
init value location =
    case Json.Decode.decodeValue Global.context value of
        Err _ ->
            Debug.crash {- TODO -} "BAD CONTEXT"

        Ok context ->
            goTo (Route.fromLocation location)
                { page = Blank
                , context = context
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
goTo destination ({ context } as model) =
    case destination of
        Nothing ->
            ( model, Route.modifyUrl Route.Dashboard )

        Just Route.Dashboard ->
            ( model, Task.attempt (load Dashboard) <| Dashboard.Page.init context )

        Just (Route.Client id) ->
            ( model, Task.attempt (load Client) <| Client.Page.init context id )

        Just (Route.TokenRedirect idToken) ->
            ( { model | context = { context | auth = Just idToken } }
            , Cmd.batch [ Js.saveToken idToken, Route.modifyUrl Route.Dashboard ]
            )


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

        Client model ->
            Client.Page.view model


main : Program Value Model Msg
main =
    Navigation.programWithFlags
        (Route.fromLocation >> SetRoute)
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
