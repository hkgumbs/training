module Main exposing (..)

import Global
import Html
import Js
import Json.Decode
import Navigation
import Plan.Page
import Route
import Settings.Page
import Task
import Ui


type alias Model =
    { page : Page
    , context : Global.Context
    }


type Page
    = Blank
    | Settings Settings.Page.Model
    | Plan Plan.Page.Model


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
    = SettingsMsg Settings.Page.Msg
    | PlanMsg Plan.Page.Msg


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
        ( SettingsMsg pageMsg, Settings model ) ->
            Settings.Page.update context pageMsg model
                |> mapBoth Settings (Cmd.map SettingsMsg)

        ( PlanMsg pageMsg, Plan model ) ->
            Plan.Page.update context pageMsg model
                |> mapBoth Plan (Cmd.map PlanMsg)

        _ ->
            ( page, Cmd.none )


mapBoth : (a -> c) -> (b -> d) -> ( a, b ) -> ( c, d )
mapBoth f g =
    Tuple.mapFirst f >> Tuple.mapSecond g


goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo destination ({ context } as model) =
    case destination of
        Nothing ->
            ( model, Route.modifyUrl Route.Plan )

        Just Route.Settings ->
            ( model, Task.attempt (load Settings) <| Settings.Page.init context )

        Just Route.Plan ->
            ( model, Task.attempt (load Plan) <| Plan.Page.init context )

        Just (Route.TokenRedirect idToken) ->
            ( { model | context = { context | auth = Just idToken } }
            , Cmd.batch [ Js.saveToken idToken, Route.modifyUrl Route.Plan ]
            )


load : (a -> Page) -> Result Global.Error a -> Msg
load page result =
    case result of
        Err type_ ->
            Error type_

        Ok data ->
            Loaded (page data)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PageMsg <|
        case model.page of
            Blank ->
                Sub.none

            Settings page ->
                Sub.none

            Plan page ->
                Sub.none


view : Model -> Html.Html Msg
view { page } =
    Html.map PageMsg <|
        case page of
            Blank ->
                Html.text ""

            Settings model ->
                Ui.toHtml SettingsMsg [ Settings.Page.view model ]

            Plan model ->
                Ui.toHtml PlanMsg [ Plan.Page.view model ]


main : Program Json.Decode.Value Model Msg
main =
    Navigation.programWithFlags
        (Route.fromLocation >> SetRoute)
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
