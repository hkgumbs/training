module Bulma
    exposing
        ( Element
        , Tile(..)
        , background
        , box
        , notification
        , text
        , tile
        , title
        , toHtml
        , width
        )

import Html
import Html.Attributes exposing (class)


type Element a
    = Text String
    | Element String (List (Html.Attribute a)) (List (Element a))


toHtml : List (Element msg) -> Html.Html msg
toHtml children =
    Html.div [] <| List.map topLevel children


topLevel : Element msg -> Html.Html msg
topLevel element =
    Html.section
        [ class "section" ]
        [ Html.div [ class "container" ] [ nested element ] ]


nested : Element msg -> Html.Html msg
nested element =
    case element of
        Text raw ->
            Html.text raw

        Element name attrs children ->
            Html.node name attrs <| List.map nested children



-- ELEMENTS


title : String -> Element msg
title name =
    Element "h1" [ class "title" ] [ text name ]


box : List (Html.Attribute msg) -> List (Element msg) -> Element msg
box attrs =
    Element "div" (class "box" :: attrs)


notification : List (Html.Attribute msg) -> List (Element msg) -> Element msg
notification attrs =
    Element "div" (class "notification" :: attrs)


text : String -> Element msg
text =
    Text



-- TILE — GRIDS AND COLUMNS


type Tile msg
    = Tile (List (Html.Attribute msg)) (List (Tile msg))
    | Children (List (Element msg))


tile : List (Tile msg) -> Element msg
tile tileTypes =
    Element "div" [ class "tile is-ancestor" ] <| List.map tileHelp tileTypes


tileHelp : Tile msg -> Element msg
tileHelp tileType =
    case tileType of
        Tile attrs children ->
            Element "div"
                (class "tile" :: attrs)
                (List.map tileHelp children)

        Children elements ->
            Element "div"
                [ class "tile is-parent" ]
                (List.map mergeChild elements)


mergeChild : Element msg -> Element msg
mergeChild element =
    case element of
        Element name attrs children ->
            Element name (class "tile is-child" :: attrs) children

        Text _ ->
            Element "div" [ class "tile is-child" ] [ element ]



-- ATTRIBUTES


width :
    { is1 : Html.Attribute msg
    , is2 : Html.Attribute msg
    , is3 : Html.Attribute msg
    , is4 : Html.Attribute msg
    , is5 : Html.Attribute msg
    , is6 : Html.Attribute msg
    , is7 : Html.Attribute msg
    , is8 : Html.Attribute msg
    , is9 : Html.Attribute msg
    , is10 : Html.Attribute msg
    , is11 : Html.Attribute msg
    , is12 : Html.Attribute msg
    }
width =
    { is1 = class "is-1"
    , is2 = class "is-2"
    , is3 = class "is-3"
    , is4 = class "is-4"
    , is5 = class "is-5"
    , is6 = class "is-6"
    , is7 = class "is-7"
    , is8 = class "is-8"
    , is9 = class "is-9"
    , is10 = class "is-10"
    , is11 = class "is-11"
    , is12 = class "is-12"
    }


background : { light : Html.Attribute msg }
background =
    { light = class "has-background-light"
    }
