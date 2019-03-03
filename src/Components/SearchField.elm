module Components.SearchField exposing (Msg(..), error, view)

import Array
import Components.Icon as Icon
import Data.Dictionary exposing (Dictionary)
import Data.Filter as Filter exposing (Filter(..))
import Help
import Html exposing (Html, button, div, input, p, text, ul)
import Html.Attributes exposing (class, classList, id, type_, value)
import Html.Events exposing (onClick, onFocus, onInput)


type Msg
    = SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | Focus


view : Dictionary -> String -> List Filter -> Bool -> Html Msg
view results searchInputBuffer filters needShadow =
    div [ class "relative" ]
        [ input
            [ type_ "text"
            , onInput SearchInput
            , onFocus Focus
            , id "search-input"
            , class "text-grey-darkest bg-grey-lighter w-full text-sm py-4 px-2 rounded"
            , classList [ ( "shadow-md", needShadow ) ]
            , value searchInputBuffer
            ]
            []
        , (error searchInputBuffer
            |> Maybe.map (text >> List.singleton >> p [ class "text-red my-4" ])
          )
            |> Maybe.withDefault (text "")
        , resultCountView results filters
        ]


resultCountView : Dictionary -> List Filter -> Html Msg
resultCountView results filters =
    let
        resultCount =
            results |> Array.length

        isFiltered =
            List.length filters > 0

        isClickable =
            isFiltered && resultCount > 0

        prefix =
            if isClickable then
                ""

            else
                "Alle "
    in
    ul
        [ class "list-reset text-xs m-2 absolute pin-r pin-t flex" ]
        [ button
            [ class "px-4 py-2 ml-px"
            , Help.groupedBtnClasses isClickable (not isClickable) True (not isFiltered)
            , onClick ToggleSearchResults
            ]
            [ text (prefix ++ (resultCount |> String.fromInt) ++ " Wörter") ]
        , if isFiltered then
            button
                [ class "px-2 py-2 ml-px"
                , Help.groupedBtnClasses True False (not isClickable) True
                , onClick ClearSearchText
                ]
                [ Icon.close "width: 1.2em; height: 1.2em" "white" ]

          else
            text ""
        ]


error : String -> Maybe String
error filterText =
    filterText
        |> Filter.parse
        |> List.foldl
            (\filter passed ->
                case ( passed, filter ) of
                    ( Just message, _ ) ->
                        Just message

                    ( _, NeverMatch message ) ->
                        Just message

                    _ ->
                        Nothing
            )
            Nothing
