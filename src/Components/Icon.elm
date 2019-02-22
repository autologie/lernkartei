module Components.Icon exposing
    ( add
    , close
    , copy
    , detail
    , edit
    , image
    , next
    , prev
    , shuffle
    , sound
    )

import Html exposing (i, text)
import Html.Attributes exposing (class)
import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, style, viewBox, width)


add : String -> Svg a
add theStyle =
    icon theStyle
        [ path [ d "M60,20 L60,100", stroke "white", fill "white", strokeWidth "12" ] []
        , path [ d "M20,60 L100,60", stroke "white", fill "white", strokeWidth "12" ] []
        ]


close : String -> String -> Svg a
close theStyle color =
    icon theStyle
        [ path [ d "M30,30 L90,90", stroke color, fill "transparent", strokeWidth "12" ] []
        , path [ d "M30,90 L90,30", stroke color, fill "transparent", strokeWidth "12" ] []
        ]


shuffle : String -> String -> Svg a
shuffle theStyle bgColor =
    icon theStyle
        [ path [ d "M10,30 C70,30 50,90 110,90", stroke "white", fill "transparent", strokeWidth "16" ] []
        , path [ d "M10,90 C70,90 50,30 110,30", stroke bgColor, fill "transparent", strokeWidth "48" ] []
        , path [ d "M10,90 C70,90 50,30 110,30", stroke "white", fill "transparent", strokeWidth "16" ] []
        , path [ d "M100,70 L120,90 L100,110 Z", fill "white" ] []
        , path [ d "M100,10 L120,30 L100,50 Z", fill "white" ] []
        ]


next : String -> Svg a
next theStyle =
    icon theStyle
        [ path [ d "M50,40 L70,60 L50,80", stroke "white", fill "transparent", strokeWidth "9" ] []
        ]


prev theStyle =
    icon theStyle
        [ path [ d "M70,40 L50,60 L70,80", stroke "white", fill "transparent", strokeWidth "9" ] []
        ]


image theStyle =
    i [ class "material-icons" ] [ text "photo" ]


sound theStyle =
    i [ class "material-icons" ] [ text "volume_up" ]


detail theStyle =
    i [ class "material-icons" ] [ text "list" ]


edit theStyle =
    i [ class "material-icons" ] [ text "edit" ]


copy theStyle =
    i [ class "material-icons" ] [ text "file_copy" ]


icon theStyle content =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        , style theStyle
        ]
        content
