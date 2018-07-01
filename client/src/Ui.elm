module Ui
    exposing
        ( Element
        , bulma
        , embed
        , empty
        , has
        , html
        , icon
        , is
        , nbsp
        , text
        , toHtml
        )

import Char
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)


{-| Thin wrapper around Html to keep things composable
-}
type Element msg
    = Text String
    | Embed (List (Element msg))
    | El (List (Attribute msg) -> List (Html msg) -> Html msg) (List (Attribute msg)) (List (Element msg))


toHtml : (a -> msg) -> List (Element a) -> Html.Html msg
toHtml f children =
    Html.map f <| Html.div [] <| List.map topLevel children


topLevel : Element msg -> Html.Html msg
topLevel element =
    Html.section
        [ class "section" ]
        [ Html.div [ class "container" ] (nested element) ]


nested : Element msg -> List (Html msg)
nested element =
    case element of
        Embed children ->
            List.concatMap nested children

        Text raw ->
            [ Html.text raw ]

        El toNode attrs children ->
            [ toNode attrs <| List.concatMap nested children ]



-- HELPERS


empty : Element msg
empty =
    Embed []


nbsp : Element msg
nbsp =
    Text <| String.fromChar <| Char.fromCode 0xA0


text : String -> Element msg
text =
    Text


html :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Element msg)
    -> Element msg
html =
    El


embed : List (Element msg) -> Element msg
embed =
    Embed


icon : String -> List (Attribute msg) -> Element msg
icon name attributes =
    El Html.span [ bulma.icon ] [ El Html.i [ class <| "fas fa-" ++ name ] [] ]



-- BULMA CLASSES


is :
    -- WIDTH
    { one : Attribute msg
    , two : Attribute msg
    , three : Attribute msg
    , four : Attribute msg
    , five : Attribute msg
    , six : Attribute msg
    , seven : Attribute msg
    , eight : Attribute msg
    , nine : Attribute msg
    , ten : Attribute msg
    , eleven : Attribute msg
    , twelve : Attribute msg

    -- COLOR
    , primary : Attribute msg
    , info : Attribute msg
    , danger : Attribute msg
    , light : Attribute msg
    , white : Attribute msg

    -- FORMS
    , expanded : Attribute msg
    , large : Attribute msg
    , medium : Attribute msg

    -- BUTTONS
    , selected : Attribute msg
    , static : Attribute msg
    , outlined : Attribute msg

    -- TILES
    , ancestor : Attribute msg
    , parent : Attribute msg
    , child : Attribute msg
    , vertical : Attribute msg
    }
is =
    { one = class "is-1"
    , two = class "is-2"
    , three = class "is-3"
    , four = class "is-4"
    , five = class "is-5"
    , six = class "is-6"
    , seven = class "is-7"
    , eight = class "is-8"
    , nine = class "is-9"
    , ten = class "is-10"
    , eleven = class "is-11"
    , twelve = class "is-12"
    , primary = class "is-primary"
    , info = class "is-info"
    , danger = class "is-danger"
    , light = class "is-light"
    , white = class "is-white"
    , expanded = class "is-expanded"
    , large = class "is-large"
    , medium = class "is-medium"
    , selected = class "is-selected"
    , static = class "is-static"
    , outlined = class "is-outlined"
    , ancestor = class "is-ancestor"
    , parent = class "is-parent"
    , child = class "is-child"
    , vertical = class "is-vertical"
    }


has :
    -- TEXT COLOR
    { textPrimary : Attribute msg
    , textInfo : Attribute msg
    , textDanger : Attribute msg
    , textLight : Attribute msg
    , textWhite : Attribute msg
    , textGrey : Attribute msg

    -- BACKGROUND COLOR
    , backgroundPrimary : Attribute msg
    , backgroundInfo : Attribute msg
    , backgroundDanger : Attribute msg
    , backgroundLight : Attribute msg
    , backgroundWhite : Attribute msg
    , backgroundGrey : Attribute msg

    -- TEXT WEIGHT
    , textWeightBold : Attribute msg

    -- FORMS
    , addons : Attribute msg
    , addonsCentered : Attribute msg

    -- ICONS
    , iconsLeft : Attribute msg
    , iconsRight : Attribute msg
    }
has =
    { textPrimary = class "has-text-primary"
    , textInfo = class "has-text-info"
    , textDanger = class "has-text-danger"
    , textLight = class "has-text-light"
    , textWhite = class "has-text-white"
    , textGrey = class "has-text-grey"
    , backgroundPrimary = class "has-background-primary"
    , backgroundInfo = class "has-background-info"
    , backgroundDanger = class "has-background-danger"
    , backgroundLight = class "has-background-light"
    , backgroundWhite = class "has-background-white"
    , backgroundGrey = class "has-background-grey"
    , textWeightBold = class "has-text-weight-bold"
    , addons = class "has-addons"
    , addonsCentered = class "has-addons-centered"
    , iconsLeft = class "has-icons-left"
    , iconsRight = class "has-icons-right"
    }


bulma :
    { title : Attribute msg
    , subtitle : Attribute msg
    , box : Attribute msg
    , notification : Attribute msg
    , tag : Attribute msg
    , tags : Attribute msg
    , level : Attribute msg
    , levelItem : Attribute msg
    , levelLeft : Attribute msg
    , levelRight : Attribute msg
    , field : Attribute msg
    , label : Attribute msg
    , control : Attribute msg
    , input : Attribute msg
    , checkbox : Attribute msg
    , textarea : Attribute msg
    , button : Attribute msg
    , columns : Attribute msg
    , column : Attribute msg
    , multiline : Attribute msg
    , tile : Attribute msg
    , icon : Attribute msg
    }
bulma =
    { title = class "title"
    , subtitle = class "subtitle"
    , box = class "box"
    , notification = class "notification"
    , tag = class "tag"
    , tags = class "tags"
    , level = class "level"
    , levelItem = class "level-item"
    , levelLeft = class "level-left"
    , levelRight = class "level-right"
    , field = class "field"
    , label = class "label"
    , control = class "control"
    , input = class "input"
    , checkbox = class "checkbox"
    , textarea = class "textarea"
    , button = class "button"
    , columns = class "columns"
    , column = class "column"
    , multiline = class "multiline"
    , tile = class "tile"
    , icon = class "icon"
    }
