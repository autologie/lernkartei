module Icon exposing (add)

import Svg exposing (Svg, path, svg)
import Svg.Attributes exposing (d, fill, height, stroke, strokeWidth, style, viewBox, width)


add : String -> Svg a
add theStyle =
    svg
        ([ width "120"
         , height "120"
         , viewBox "0 0 120 120"
         ]
            ++ [ style theStyle ]
        )
        [ path [ d "M60,20 L60,100", stroke "white", fill "white", strokeWidth "12" ] []
        , path [ d "M20,60 L100,60", stroke "white", fill "white", strokeWidth "12" ] []
        ]
