module Components.Dialog exposing (Dialog(..), view)

import Help
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Dialog a
    = YesNoDialog String a a


view : Dialog a -> Html a
view dialog =
    case dialog of
        YesNoDialog message onYes onNo ->
            div
                [ Help.classNames
                    [ "fixed"
                    , "w-full"
                    , "h-full"
                    , "flex"
                    , "justify-center"
                    , "items-center"
                    ]
                , style "background-color" "rgba(0,0,0,.3)"
                ]
                [ div
                    [ Help.classNames
                        [ "p-8"
                        , "bg-white"
                        , "shadow-lg"
                        , "w-full"
                        , "mb-8"
                        ]
                    ]
                    [ p [ Help.classNames [ "my-2" ] ] [ text message ]
                    , div [ Help.classNames [ "flex", "justify-center" ] ]
                        [ button [ Help.classNames btnClasses, onClick onYes ] [ text "Ja" ]
                        , button [ Help.classNames btnClasses, onClick onNo ] [ text "Nein" ]
                        ]
                    ]
                ]


btnClasses : List String
btnClasses =
    [ Help.L <| Help.btnClasses True False
    , Help.V "px-4"
    , Help.V "py-2"
    , Help.V "mx-2"
    ]
        |> Help.flatten
