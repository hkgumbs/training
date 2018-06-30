module Main exposing (..)

import Bulma
import Client.Page
import Dashboard.Page
import Exercise.Page
import Global
import Html
import Js
import Json.Decode
import Navigation
import Route
import Task


type alias Model =
    { page : Page
    , context : Global.Context
    }


type Page
    = Blank
    | Dashboard Dashboard.Page.Model
    | Client Client.Page.Model
    | Exercise Exercise.Page.Model


init : Json.Decode.Value -> Navigation.Location -> ( Model, Cmd Msg )
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
    | PageMsg PageMsg


type PageMsg
    = DashboardMsg Dashboard.Page.Msg
    | ClientMsg Client.Page.Msg
    | ExerciseMsg Exercise.Page.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute destination ->
            goTo destination model

        Error Global.RequiresAuth ->
            ( model, Js.login )

        Error Global.AppError ->
            Debug.crash {- TODO -} ""

        Logout ->
            ( model, Js.logout )

        Loaded page ->
            ( { model | page = page }, Cmd.none )

        PageMsg pageMsg ->
            updatePage model.context pageMsg model.page
                |> mapBoth (\page -> { model | page = page }) (Cmd.map PageMsg)


updatePage : Global.Context -> PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage context msg page =
    case ( msg, page ) of
        ( DashboardMsg pageMsg, Dashboard model ) ->
            Dashboard.Page.update context pageMsg model
                |> mapBoth Dashboard (Cmd.map DashboardMsg)

        ( ClientMsg pageMsg, Client model ) ->
            Client.Page.update context pageMsg model
                |> mapBoth Client (Cmd.map ClientMsg)

        ( ExerciseMsg pageMsg, Exercise model ) ->
            Exercise.Page.update context pageMsg model
                |> mapBoth Exercise (Cmd.map ExerciseMsg)

        _ ->
            ( page, Cmd.none )


mapBoth : (a -> c) -> (b -> d) -> ( a, b ) -> ( c, d )
mapBoth f g =
    Tuple.mapFirst f >> Tuple.mapSecond g


goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo destination ({ context } as model) =
    case destination of
        Nothing ->
            ( model, Route.modifyUrl Route.Dashboard )

        Just Route.Dashboard ->
            ( model, Task.attempt (load Dashboard) <| Dashboard.Page.init context )

        Just (Route.Client id) ->
            ( model, Task.attempt (load Client) <| Client.Page.init context id )

        Just (Route.Exercise id) ->
            ( model, Task.attempt (load Exercise) <| Exercise.Page.init context id )

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
    Html.map PageMsg <|
        case page of
            Blank ->
                Html.text ""

            Dashboard model ->
                Bulma.toHtml DashboardMsg [ Dashboard.Page.view model ]

            Client model ->
                Bulma.toHtml ClientMsg [ Client.Page.view model ]

            Exercise model ->
                Bulma.toHtml ExerciseMsg [ Exercise.Page.view model ]


main : Program Json.Decode.Value Model Msg
main =
    Navigation.programWithFlags
        (Route.fromLocation >> SetRoute)
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
