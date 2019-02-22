module Components.Button exposing (addNewEntry, floatingGroup)

import Components.Icon as Icon
import Data.AppUrl as AppUrl
import Html exposing (Attribute, Html, a, div, text)
import Html.Attributes exposing (class, classList, href)


floatingGroup buttons =
    div
        [ class "fixed pin-r pin-b m-4 z-30 flex justify-center items-center flex-col" ]
        buttons


addNewEntry url =
    a
        [ class "rounded-full bg-green text-white text-xl p-2 w-16 h-16 flex justify-center items-center shadow-lg no-underline"
        , href (url |> AppUrl.toString)
        ]
        [ Icon.add "width: .6em; height: .6em;" ]
