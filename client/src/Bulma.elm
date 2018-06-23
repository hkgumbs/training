module Bulma
    exposing
        ( Attribute
        , Column(..)
        , Element
        , Tile(..)
        , ancestor
        , background
        , bold
        , box
        , br
        , checkbox
        , color
        , columns
        , field
        , form
        , label
        , multiline
        , notification
        , subtitle
        , text
        , title
        , toHtml
        , vertical
        , width
        )

import Html
import Html.Attributes as Attrs


type Element a
    = Text String
    | Element String (List (Attribute a)) (List (Element a))


type alias Attribute a =
    Html.Attribute a


toHtml : (a -> msg) -> List (Element a) -> Html.Html msg
toHtml f children =
    Html.map f <| Html.div [] <| List.map topLevel children


topLevel : Element msg -> Html.Html msg
topLevel element =
    Html.section
        [ Attrs.class "section" ]
        [ Html.div [ Attrs.class "container" ] [ nested element ] ]


nested : Element msg -> Html.Html msg
nested element =
    case element of
        Text raw ->
            Html.text raw

        Element name attrs children ->
            Html.node name attrs <| List.map nested children



-- ELEMENTS


title : String -> Element msg
title raw =
    Element "h1" [ Attrs.class "title" ] [ text raw ]


subtitle : String -> Element msg
subtitle raw =
    Element "h2" [ Attrs.class "subtitle" ] [ text raw ]


box : List (Attribute msg) -> List (Element msg) -> Element msg
box attrs =
    Element "div" (Attrs.class "box" :: attrs)


notification : List (Attribute msg) -> List (Element msg) -> Element msg
notification attrs =
    Element "div" (Attrs.class "notification" :: attrs)


bold : String -> Element msg
bold raw =
    Element "strong" [] [ Text raw ]


text : String -> Element msg
text =
    Text


br : Element msg
br =
    Element "br" [] []



-- FORMS


form :
    { addons : Attribute msg
    , addonsCentered : Attribute msg
    , expanded : Attribute msg
    }
form =
    { addons = Attrs.class "has-addons"
    , addonsCentered = Attrs.class "has-addons-centered"
    , expanded = Attrs.class "is-expanded"
    }


field : List (Attribute msg) -> List (Element msg) -> Element msg
field attrs =
    Element "div" (Attrs.class "field" :: attrs)


label : String -> Element msg
label name =
    Element "label" [ Attrs.class "label" ] [ Text name ]


checkbox : List (Attribute msg) -> Bool -> String -> Element msg
checkbox attrs value name =
    Element "div"
        [ Attrs.class "control" ]
        [ Element "label"
            (Attrs.class "checkbox" :: attrs)
            [ Element "input"
                [ Attrs.type_ "checkbox", Attrs.checked value ]
                []
            , Text <| " " ++ name
            ]
        ]



-- COLUMNS


type Column msg
    = Column (List (Attribute msg)) (List (Element msg))


columns : List (Column msg) -> Element msg
columns columnTypes =
    Element "div" [ Attrs.class "columns" ] (List.map fromColumn columnTypes)


fromColumn : Column msg -> Element msg
fromColumn (Column attrs children) =
    Element "div" (Attrs.class "column" :: attrs) children


multiline : Attribute msg
multiline =
    Attrs.class "multiline"



-- TILE


type Tile msg
    = Tile (List (Attribute msg)) (List (Tile msg))
    | Parent (List (Element msg))


vertical : Attribute msg
vertical =
    Attrs.class "is-vertical"


ancestor : List (Tile msg) -> Element msg
ancestor tileTypes =
    Element "div" [ Attrs.class "tile is-ancestor" ] <| List.map tileHelp tileTypes


tileHelp : Tile msg -> Element msg
tileHelp tileType =
    case tileType of
        Tile attrs children ->
            Element "div"
                (Attrs.class "tile" :: attrs)
                (List.map tileHelp children)

        Parent children ->
            Element "div"
                [ Attrs.class "tile is-parent" ]
                (List.map mergeChild children)


mergeChild : Element msg -> Element msg
mergeChild element =
    case element of
        Element name attrs children ->
            Element name (Attrs.class "tile is-child" :: attrs) children

        Text _ ->
            Element "div" [ Attrs.class "tile is-child" ] [ element ]



-- ATTRIBUTE HELPERS


width :
    { is1 : Attribute msg
    , is2 : Attribute msg
    , is3 : Attribute msg
    , is4 : Attribute msg
    , is5 : Attribute msg
    , is6 : Attribute msg
    , is7 : Attribute msg
    , is8 : Attribute msg
    , is9 : Attribute msg
    , is10 : Attribute msg
    , is11 : Attribute msg
    , is12 : Attribute msg
    }
width =
    { is1 = Attrs.class "is-1"
    , is2 = Attrs.class "is-2"
    , is3 = Attrs.class "is-3"
    , is4 = Attrs.class "is-4"
    , is5 = Attrs.class "is-5"
    , is6 = Attrs.class "is-6"
    , is7 = Attrs.class "is-7"
    , is8 = Attrs.class "is-8"
    , is9 = Attrs.class "is-9"
    , is10 = Attrs.class "is-10"
    , is11 = Attrs.class "is-11"
    , is12 = Attrs.class "is-12"
    }


color :
    { light : Attribute msg
    , primary : Attribute msg
    , white : Attribute msg
    }
color =
    { primary = Attrs.class "is-primary"
    , light = Attrs.class "is-light"
    , white = Attrs.class "is-white"
    }


background :
    { light : Attribute msg
    , primary : Attribute msg
    , white : Attribute msg
    }
background =
    { primary = Attrs.class "has-background-primary"
    , light = Attrs.class "has-background-light"
    , white = Attrs.class "has-background-white"
    }


noOp : Attribute msg
noOp =
    Attrs.class ""
