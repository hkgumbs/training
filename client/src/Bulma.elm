module Bulma
    exposing
        ( Attribute
        , Column(..)
        , Element
        , Icon
        , Tile(..)
        , ancestor
        , arrowLeft
        , arrowRight
        , background
        , bold
        , box
        , br
        , button
        , color
        , columns
        , concat
        , control
        , delete
        , empty
        , field
        , form
        , hr
        , icon
        , icons
        , label
        , level
        , levelLeftRight
        , multiline
        , nbsp
        , notification
        , outline
        , placeholder
        , rows
        , search
        , selected
        , subtitle
        , tag
        , tags
        , text
        , textInput
        , title
        , toHtml
        , toggle
        , user
        , vertical
        , width
        )

import Char
import Html
import Html.Attributes as Attrs


type Element a
    = Text String
    | Element String (List (Attribute a)) (List (Element a))
    | Concat (List (Element a))


type alias Attribute a =
    Html.Attribute a


toHtml : (a -> msg) -> List (Element a) -> Html.Html msg
toHtml f children =
    Html.map f <| Html.div [] <| List.map topLevel children


topLevel : Element msg -> Html.Html msg
topLevel element =
    Html.section
        [ Attrs.class "section" ]
        [ Html.div [ Attrs.class "container" ] (nested element) ]


nested : Element msg -> List (Html.Html msg)
nested element =
    case element of
        Text raw ->
            [ Html.text raw ]

        Element name attrs children ->
            [ Html.node name attrs <| List.concatMap nested children ]

        Concat children ->
            List.concatMap nested children



-- ELEMENTS


title : String -> Element msg
title raw =
    Element "h1" [ Attrs.class "title" ] [ text raw ]


subtitle : String -> Element msg
subtitle raw =
    Element "h2" [ Attrs.class "subtitle has-text-weight-semibold" ] [ text raw ]


box : List (Attribute msg) -> List (Element msg) -> Element msg
box attrs =
    Element "div" (Attrs.class "box" :: attrs)


level : List (List (Element msg)) -> Element msg
level =
    Element "div" [ Attrs.class "level" ]
        << List.map (Element "div" [ Attrs.class "level-item" ])


levelLeftRight : List (Element msg) -> List (Element msg) -> Element msg
levelLeftRight lefts rights =
    Element "div"
        [ Attrs.class "level" ]
        [ Element "div" [ Attrs.class "level-left" ] lefts
        , Element "div" [ Attrs.class "level-right" ] rights
        ]


notification : List (Attribute msg) -> List (Element msg) -> Element msg
notification attrs =
    Element "div" (Attrs.class "notification" :: attrs)


tag : List (Attribute msg) -> String -> Element msg
tag attrs name =
    Element "span" (Attrs.class "tag" :: attrs) [ text name ]


tags : List (Attribute msg) -> List (Element msg) -> Element msg
tags attrs =
    Element "div" (Attrs.class "tags" :: attrs)


bold : String -> Element msg
bold raw =
    Element "span" [ Attrs.class "has-text-weight-bold" ] [ Text raw ]


text : String -> Element msg
text =
    Text


nbsp : Element msg
nbsp =
    Text <| String.fromChar <| Char.fromCode 0xA0


br : Element msg
br =
    Element "br" [] []


hr : Element msg
hr =
    Element "hr" [] []


concat : List (Element msg) -> Element msg
concat =
    Concat


empty : Element msg
empty =
    Concat []


toggle : Bool -> List (Element msg) -> List (Element msg) -> Element msg
toggle condition whenVisible whenHidden =
    if condition then
        Element "show" [] whenVisible
    else
        Element "hide" [] whenHidden



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


control : List (Attribute msg) -> List (Element msg) -> Element msg
control attrs =
    Element "div" (Attrs.class "control" :: attrs)


textInput : List (Attribute msg) -> Element msg
textInput attrs =
    Element "input" (Attrs.class "input" :: attrs) []


button : List (Attribute msg) -> List (Element msg) -> Element msg
button attrs =
    Element "button" (Attrs.class "button" :: attrs)


placeholder : String -> Attribute msg
placeholder =
    Attrs.placeholder


selected : Bool -> Attribute msg
selected condition =
    if condition then
        Attrs.class "is-info is-selected"
    else
        Attrs.class ""


outline : Attribute msg
outline =
    Attrs.class "is-outlined"


icons : { left : Attribute msg, right : Attribute msg }
icons =
    { left = Attrs.class "has-icons-left"
    , right = Attrs.class "has-icons-right"
    }



-- COLUMNS


type Column msg
    = Column (List (Attribute msg)) (List (Element msg))


rows : List (Element msg) -> Element msg
rows =
    columns << List.singleton << Column []


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

        _ ->
            Element "div" [ Attrs.class "tile is-child" ] [ element ]



-- ICONS


type Icon
    = Icon String


icon : Icon -> Element msg
icon (Icon name) =
    Element "span"
        [ Attrs.class "icon" ]
        [ Element "i" [ Attrs.class <| "fas fa-" ++ name ] [] ]


user : Icon
user =
    Icon "user-circle"


arrowLeft : Icon
arrowLeft =
    Icon "long-arrow-alt-left"


arrowRight : Icon
arrowRight =
    Icon "long-arrow-alt-right"


delete : Icon
delete =
    Icon "minus-circle"


search : Icon
search =
    Icon "search"



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
    { primary : Attribute msg
    , info : Attribute msg
    , danger : Attribute msg
    , light : Attribute msg
    , white : Attribute msg
    }
color =
    { primary = Attrs.class "is-primary"
    , info = Attrs.class "is-info"
    , danger = Attrs.class "is-danger"
    , light = Attrs.class "is-light"
    , white = Attrs.class "is-white"
    }


background :
    { primary : Attribute msg
    , danger : Attribute msg
    , light : Attribute msg
    , white : Attribute msg
    }
background =
    { primary = Attrs.class "has-background-primary"
    , danger = Attrs.class "has-background-danger"
    , light = Attrs.class "has-background-light"
    , white = Attrs.class "has-background-white"
    }
