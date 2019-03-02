module Components.Button exposing (addNewEntry, floatingGroup)

import Components.Icon as Icon
import Data.AppUrl as AppUrl
import Html exposing (a, div)
import Html.Attributes exposing (class, href)


floatingGroup : List (Html.Html msg) -> Html.Html msg
floatingGroup buttons =
    div
        [ class "fixed pin-r pin-b m-4 z-30 flex justify-center items-center flex-col" ]
        buttons


addNewEntry : AppUrl.AppUrl -> Html.Html msg
addNewEntry url =
    a
        [ class "rounded-full bg-green text-white text-xl p-2 w-16 h-16 flex justify-center items-center shadow-lg no-underline"
        , href (url |> AppUrl.toString)
        ]
        [ Icon.add "width: .6em; height: .6em;" ]
