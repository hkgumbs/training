module Bulma
    exposing
        ( Attribute
        , Html
        , background
        , box
        , column
        , columns
        , container
        , notification
        , section
        , text
        , title
        , width
        )

import Html
import Html.Attributes exposing (class)


type alias Html a =
    Html.Html a


type alias Attribute a =
    Html.Attribute a


container : List (Html msg) -> Html msg
container =
    Html.section [ class "container" ]


section : List (Html msg) -> Html msg
section =
    Html.section [ class "section" ]


columns : List (Html msg) -> Html msg
columns =
    Html.div [ class "columns" ]


column : List (Attribute msg) -> List (Html msg) -> Html msg
column attrs =
    Html.div (class "column" :: attrs)


title : String -> Html msg
title text =
    Html.h1 [ class "title" ] [ Html.text text ]


box : List (Attribute msg) -> List (Html msg) -> Html msg
box attrs =
    Html.div (class "box" :: attrs)


notification : List (Attribute msg) -> List (Html msg) -> Html msg
notification attrs =
    Html.div (class "notification" :: attrs)


text : String -> Html msg
text =
    Html.text



-- ATTRIBUTES


width :
    { auto : Attribute msg
    , threeQuarters : Attribute msg
    , twoThirds : Attribute msg
    , half : Attribute msg
    , oneThird : Attribute msg
    , oneQuarter : Attribute msg
    }
width =
    { auto = class ""
    , threeQuarters = class "is-three-quarters"
    , twoThirds = class "is-two-thirds"
    , half = class "is-half"
    , oneThird = class "is-one-third"
    , oneQuarter = class "is-one-quarter"
    }


background : { light : Html.Attribute msg }
background =
    { light = class "has-background-light"
    }
