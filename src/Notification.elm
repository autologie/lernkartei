module Notification exposing (Model, initialModel, view)

import Help
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Model =
    { message : String
    , isShown : Bool
    }


initialModel =
    { message = ""
    , isShown = False
    }


view : Model -> a -> Html a
view { isShown, message } onClickCloseButton =
    div
        [ Help.classNames
            [ "fixed"
            , "z-50"
            , "pin-b"
            , "pin-l"
            , "p-3"
            , "bg-black"
            , "text-white"
            , "text-xs"
            , "w-full"
            , "leading-loose"
            ]
        , style "transition" "margin-bottom .3s ease"
        , style "margin-bottom"
            (if isShown then
                "0"

             else
                "-6em"
            )
        ]
        [ text message
        , button
            [ onClick onClickCloseButton
            , Help.classNames
                [ "rounded"
                , "bg-grey-darker"
                , "text-grey-lighter"
                , "text-xs"
                , "px-2"
                , "py-1"
                , "mx-2"
                ]
            ]
            [ text "Entlassen" ]
        ]
