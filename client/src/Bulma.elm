module Bulma
    exposing
        ( Attribute
        , Column
        , Element
        , Icon
        , Tile
        , ancestor
        , arrowLeft
        , arrowRight
        , background
        , bold
        , box
        , br
        , button
        , color
        , column
        , columns
        , concat
        , control
        , delete
        , divider
        , dividerVertical
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
        , parent
        , placeholder
        , plus
        , rows
        , search
        , selected
        , subtitle
        , tag
        , tags
        , text
        , textInput
        , tile
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


type Element tag msg
    = Text String
    | Element String (List (Attribute msg)) (List (Element tag msg))
    | Concat (List (Element tag msg))


type alias Attribute a =
    Html.Attribute a


toHtml : (a -> msg) -> List (Element tag a) -> Html.Html msg
toHtml f children =
    Html.map f <| Html.div [] <| List.map topLevel children


topLevel : Element tag msg -> Html.Html msg
topLevel element =
    Html.section
        [ Attrs.class "section" ]
        [ Html.div [ Attrs.class "container" ] (nested element) ]


nested : Element tag msg -> List (Html.Html msg)
nested element =
    case element of
        Text raw ->
            [ Html.text raw ]

        Element name attrs children ->
            [ Html.node name attrs <| List.concatMap nested children ]

        Concat children ->
            List.concatMap nested children



-- ELEMENTS


title : List (Element tag msg) -> Element tag msg
title =
    Element "h1" [ Attrs.class "title" ]


subtitle : List (Element tag msg) -> Element tag msg
subtitle =
    Element "h2" [ Attrs.class "subtitle has-text-weight-semibold" ]


box : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
box attrs =
    Element "div" (Attrs.class "box" :: attrs)


level : List (List (Element tag msg)) -> Element tag msg
level =
    Element "div" [ Attrs.class "level" ]
        << List.map (Element "div" [ Attrs.class "level-item" ])


levelLeftRight : List (Element tag msg) -> List (Element tag msg) -> Element tag msg
levelLeftRight lefts rights =
    Element "div"
        [ Attrs.class "level" ]
        [ Element "div" [ Attrs.class "level-left" ] lefts
        , Element "div" [ Attrs.class "level-right" ] rights
        ]


notification : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
notification attrs =
    Element "div" (Attrs.class "notification" :: attrs)


tag : List (Attribute msg) -> String -> Element tag msg
tag attrs name =
    Element "span" (Attrs.class "tag" :: attrs) [ text name ]


tags : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
tags attrs =
    Element "div" (Attrs.class "tags" :: attrs)


bold : String -> Element tag msg
bold raw =
    Element "span" [ Attrs.class "has-text-weight-bold" ] [ Text raw ]


text : String -> Element tag msg
text =
    Text


nbsp : Element tag msg
nbsp =
    Text <| String.fromChar <| Char.fromCode 0xA0


br : Element tag msg
br =
    Element "br" [] []


hr : Element tag msg
hr =
    Element "hr" [] []


divider : String -> Element tag msg
divider content =
    Element "div"
        [ Attrs.class "is-divider"
        , Attrs.attribute "data-content" content
        ]
        []


dividerVertical : String -> Element tag msg
dividerVertical content =
    Element "div"
        [ Attrs.class "is-divider-vertical"
        , Attrs.attribute "data-content" content
        ]
        []


concat : List (Element tag msg) -> Element tag msg
concat =
    Concat


empty : Element tag msg
empty =
    Concat []


toggle : Bool -> List (Element tag msg) -> List (Element tag msg) -> Element tag msg
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


field : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
field attrs =
    Element "div" (Attrs.class "field" :: attrs)


label : String -> Element tag msg
label name =
    Element "label" [ Attrs.class "label" ] [ Text name ]


control : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
control attrs =
    Element "div" (Attrs.class "control" :: attrs)


textInput : List (Attribute msg) -> Element tag msg
textInput attrs =
    Element "input" (Attrs.class "input" :: attrs) []


button : List (Attribute msg) -> List (Element tag msg) -> Element tag msg
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


type Column
    = Column


rows : List (Element tag msg) -> Element tag msg
rows =
    columns << List.singleton << column []


column : List (Attribute msg) -> List (Element tag msg) -> Element Column msg
column attrs =
    eraseType << Element "div" (Attrs.class "column" :: attrs)


columns : List (Element Column msg) -> Element tag msg
columns =
    eraseType << Element "div" [ Attrs.class "columns" ]


multiline : Attribute msg
multiline =
    Attrs.class "multiline"



-- TILE


type Tile
    = Tile


ancestor : List (Element Tile msg) -> Element tag msg
ancestor =
    eraseType << Element "div" [ Attrs.class "tile is-ancestor" ]


tile : List (Attribute msg) -> List (Element Tile msg) -> Element Tile msg
tile attrs =
    Element "div" (Attrs.class "tile" :: attrs)


parent : List (Element Tile msg) -> Element Tile msg
parent children =
    Element "div" [ Attrs.class "tile is-parent" ] (List.map mergeChild children)


mergeChild : Element Tile msg -> Element Tile msg
mergeChild element =
    case element of
        Element name attrs children ->
            Element name (Attrs.class "tile is-child" :: attrs) children

        _ ->
            eraseType <| Element "div" [ Attrs.class "tile is-child" ] [ element ]


vertical : Attribute msg
vertical =
    Attrs.class "is-vertical"



-- ICONS


type Icon
    = Icon String


icon : Icon -> Element tag msg
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


plus : Icon
plus =
    Icon "plus-circle"



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


eraseType : Element a msg -> Element b msg
eraseType element =
    case element of
        Text raw ->
            Text raw

        Element name attrs children ->
            Element name attrs <| List.map eraseType children

        Concat children ->
            Concat <| List.map eraseType children
