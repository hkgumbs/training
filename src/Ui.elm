module Ui
    exposing
        ( Attribute
        , Element
        , a
        , br
        , bulma
        , button
        , concat
        , el
        , empty
        , h1
        , h2
        , has
        , hr
        , icon
        , input
        , is
        , label
        , li
        , nbsp
        , ol
        , option
        , select
        , stylesheet
        , text
        , toHtml
        , ul
        , when
        )

import Char
import Html exposing (Html)
import Html.Attributes exposing (class)


{-| Thin wrapper around Html to keep wiht the following goals:

1.  Allow arbitrary composition with `concat`
2.  Limit choice to semantic elements
3.  Mirror the Bulma API for learnability

-}
type Element msg
    = Text String
    | Concat (List (Element msg))
    | El String (List (Attribute msg)) (List (Element msg))


type alias Attribute msg =
    Html.Attribute msg


toHtml : (a -> msg) -> List (Element a) -> Html.Html msg
toHtml f children =
    Html.map f <| Html.div [] <| List.concatMap nested children


nested : Element msg -> List (Html msg)
nested element =
    case element of
        Concat children ->
            List.concatMap nested children

        Text raw ->
            [ Html.text raw ]

        El name attrs children ->
            [ Html.node name attrs <| List.concatMap nested children ]



-- SEMANTIC ELEMENTS


empty : Element msg
empty =
    Concat []


concat : List (Element msg) -> Element msg
concat =
    Concat


text : String -> Element msg
text =
    Text


nbsp : Element msg
nbsp =
    Text <| String.fromChar <| Char.fromCode 0xA0


hr : Element msg
hr =
    El "hr" [] []


br : Element msg
br =
    El "br" [] []


el : List (Attribute msg) -> List (Element msg) -> Element msg
el =
    El "div"


a : List (Attribute msg) -> List (Element msg) -> Element msg
a =
    El "a"


h1 : List (Attribute msg) -> List (Element msg) -> Element msg
h1 =
    El "h1"


h2 : List (Attribute msg) -> List (Element msg) -> Element msg
h2 =
    El "h2"


ul : List (Attribute msg) -> List (Element msg) -> Element msg
ul =
    El "ul"


ol : List (Attribute msg) -> List (Element msg) -> Element msg
ol =
    El "ol"


li : List (Attribute msg) -> List (Element msg) -> Element msg
li =
    El "li"


button : List (Attribute msg) -> List (Element msg) -> Element msg
button =
    El "button"


label : List (Attribute msg) -> List (Element msg) -> Element msg
label =
    El "label"


select : List (Attribute msg) -> List (Element msg) -> Element msg
select =
    El "select"


option : List (Attribute msg) -> List (Element msg) -> Element msg
option =
    El "option"


input : List (Attribute msg) -> Element msg
input attrs =
    El "input" attrs []


stylesheet : String -> Element msg
stylesheet styles =
    El "style"
        [ Html.Attributes.attribute "type" "text/css"
        , Html.Attributes.attribute "scoped" ""
        ]
        [ Text styles ]


icon : String -> Element msg
icon name =
    El "span" [ bulma.icon ] [ El "i" [ class <| "fas fa-" ++ name ] [] ]


when : Bool -> Attribute msg -> Attribute msg
when condition attr =
    if condition then
        attr
    else
        class ""



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
    , warning : Attribute msg
    , danger : Attribute msg
    , light : Attribute msg
    , white : Attribute msg
    , invisible : Attribute msg
    , bold : Attribute msg

    -- FORMS
    , expanded : Attribute msg
    , large : Attribute msg
    , medium : Attribute msg
    , hoverable : Attribute msg

    -- BUTTONS
    , selected : Attribute msg
    , static : Attribute msg
    , outlined : Attribute msg
    , inverted : Attribute msg
    , hovered : Attribute msg
    , rounded : Attribute msg

    -- LAYOUT
    , fullwidth : Attribute msg
    , fullheight : Attribute msg

    -- TILES
    , ancestor : Attribute msg
    , parent : Attribute msg
    , child : Attribute msg
    , vertical : Attribute msg

    -- TYPOGRAPHY
    , italic : Attribute msg
    , lowercase : Attribute msg
    , uppercase : Attribute msg
    , capitalized : Attribute msg
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
    , warning = class "is-warning"
    , danger = class "is-danger"
    , light = class "is-light"
    , white = class "is-white"
    , invisible = class "is-invisible"
    , bold = class "is-bold"
    , expanded = class "is-expanded"
    , large = class "is-large"
    , medium = class "is-medium"
    , hoverable = class "is-hoverable"
    , selected = class "is-selected"
    , static = class "is-static"
    , outlined = class "is-outlined"
    , inverted = class "is-inverted"
    , hovered = class "is-hovered"
    , rounded = class "is-rounded"
    , fullwidth = class "is-fullwidth"
    , fullheight = class "is-fullheight"
    , ancestor = class "is-ancestor"
    , parent = class "is-parent"
    , child = class "is-child"
    , vertical = class "is-vertical"
    , italic = class "is-italic"
    , uppercase = class "is-uppercase"
    , lowercase = class "is-lowercase"
    , capitalized = class "is-capitalized"
    }


has :
    -- TEXT COLOR
    { textPrimary : Attribute msg
    , textInfo : Attribute msg
    , textDanger : Attribute msg
    , textLight : Attribute msg
    , textWhite : Attribute msg
    , textGrey : Attribute msg
    , textCentered : Attribute msg

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
    , textCentered = class "has-text-centered"
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
    , hero : Attribute msg
    , heroBody : Attribute msg
    , container : Attribute msg
    , section : Attribute msg
    , box : Attribute msg
    , breadcrumb : Attribute msg
    , notification : Attribute msg
    , tag : Attribute msg
    , tags : Attribute msg
    , level : Attribute msg
    , levelItem : Attribute msg
    , levelLeft : Attribute msg
    , levelRight : Attribute msg
    , navbar : Attribute msg
    , navbarBrand : Attribute msg
    , navbarBurger : Attribute msg
    , navbarMenu : Attribute msg
    , navbarStart : Attribute msg
    , navbarEnd : Attribute msg
    , navbarItem : Attribute msg
    , navbarLink : Attribute msg
    , navbarDropdown : Attribute msg
    , navbarDivider : Attribute msg
    , field : Attribute msg
    , label : Attribute msg
    , control : Attribute msg
    , input : Attribute msg
    , select : Attribute msg
    , checkbox : Attribute msg
    , textarea : Attribute msg
    , button : Attribute msg
    , buttons : Attribute msg
    , delete : Attribute msg
    , dropdown : Attribute msg
    , dropdownTrigger : Attribute msg
    , dropdownMenu : Attribute msg
    , dropdownContent : Attribute msg
    , dropdownItem : Attribute msg
    , dropdownDivider : Attribute msg
    , media : Attribute msg
    , mediaContent : Attribute msg
    , mediaLeft : Attribute msg
    , mediaRight : Attribute msg
    , columns : Attribute msg
    , column : Attribute msg
    , multiline : Attribute msg
    , tile : Attribute msg
    , icon : Attribute msg
    }
bulma =
    { title = class "title"
    , subtitle = class "subtitle"
    , hero = class "hero"
    , heroBody = class "hero-body"
    , container = class "container"
    , section = class "section"
    , box = class "box"
    , breadcrumb = class "breadcrumb"
    , notification = class "notification"
    , tag = class "tag"
    , tags = class "tags"
    , level = class "level"
    , levelItem = class "level-item"
    , levelLeft = class "level-left"
    , levelRight = class "level-right"
    , navbar = class "navbar"
    , navbarBrand = class "navbar-brand"
    , navbarBurger = class "navbar-burger"
    , navbarMenu = class "navbbar-menu"
    , navbarStart = class "navbar-start"
    , navbarEnd = class "navbar-end"
    , navbarItem = class "navbar-item"
    , navbarLink = class "navbar-link"
    , navbarDropdown = class "navbar-dropdown"
    , navbarDivider = class "navbar-divider"
    , field = class "field"
    , label = class "label"
    , control = class "control"
    , input = class "input"
    , select = class "select"
    , checkbox = class "checkbox"
    , textarea = class "textarea"
    , button = class "button"
    , buttons = class "buttons"
    , delete = class "delete"
    , dropdown = class "dropdown"
    , dropdownTrigger = class "dropdown-triggerd"
    , dropdownMenu = class "dropdown-menu"
    , dropdownContent = class "dropdown-content"
    , dropdownItem = class "dropdown-item"
    , dropdownDivider = class "dropdown-divider"
    , media = class "media"
    , mediaContent = class "media-content"
    , mediaLeft = class "media-left"
    , mediaRight = class "media-right"
    , columns = class "columns"
    , column = class "column"
    , multiline = class "multiline"
    , tile = class "tile"
    , icon = class "icon"
    }
