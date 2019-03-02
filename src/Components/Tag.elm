module Components.Tag exposing (view)

import Help
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


view : String -> Bool -> msg -> Html msg
view txt isSelected onClickTag =
    button
        [ onClick onClickTag
        , Help.classNames
            ("no-underline rounded inline-block mr-1 mb-2 py-1 px-2"
                :: (if isSelected then
                        [ "text-white"
                        , "bg-blue"
                        ]

                    else
                        [ "text-grey-darker"
                        , "bg-grey-light"
                        ]
                   )
            )
        ]
        [ text txt ]
