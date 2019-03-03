module Components.Tag exposing (view)

import Help
import Html exposing (Html, button, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


view : String -> Bool -> msg -> Html msg
view txt isSelected onClickTag =
    button
        [ onClick onClickTag
        , class "no-underline rounded inline-block mr-1 mb-2 py-1 px-2"
        , classList
            [ ( "text-white bg-blue", isSelected )
            , ( "text-grey-darker bg-grey-light", not isSelected )
            ]
        ]
        [ text txt ]
