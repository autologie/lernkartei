module Components.SearchField exposing (Msg(..), error, view)

import Array
import Components.Icon as Icon
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Filter as Filter exposing (Filter(..))
import Help
import Html exposing (Html, button, div, input, p, text, ul)
import Html.Attributes exposing (id, type_, value)
import Html.Events exposing (onClick, onInput)


type Msg
    = SearchInput String
    | ToggleSearchResults
    | ClearSearchText


view : Dictionary -> String -> List Filter -> Html Msg
view results searchInputBuffer filters =
    div [ Help.classNames [ "relative" ] ]
        (input
            [ type_ "text"
            , onInput SearchInput
            , id "search-input"
            , Help.classNames
                [ "text-grey-darkest"
                , "bg-transparent"
                , "w-full"
                , "text-sm"
                , "py-4"
                ]
            , value searchInputBuffer
            ]
            []
            :: (error searchInputBuffer
                    |> Maybe.map
                        (\errorMessage ->
                            [ p
                                [ Help.classNames
                                    [ "text-red"
                                    , "my-4"
                                    ]
                                ]
                                [ text errorMessage ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
            ++ [ resultCountView results filters ]
        )


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
        [ Help.classNames
            [ "list-reset"
            , "text-xs"
            , "my-2"
            , "absolute"
            , "pin-r"
            , "pin-t"
            , "flex"
            ]
        ]
        ([ button
            [ Help.classNames
                (Help.groupedBtnClasses isClickable
                    (not isClickable)
                    True
                    (not isFiltered)
                    ++ [ "px-4", "py-2", "ml-px" ]
                )
            , onClick ToggleSearchResults
            ]
            [ text (prefix ++ (resultCount |> String.fromInt) ++ " Wörter") ]
         ]
            ++ (if isFiltered then
                    [ button
                        [ Help.classNames
                            (Help.groupedBtnClasses True
                                False
                                (not isClickable)
                                True
                                ++ [ "px-2", "py-2", "ml-px" ]
                            )
                        , onClick ClearSearchText
                        ]
                        [ Icon.close "width: 1.2em; height: 1.2em" "white" ]
                    ]

                else
                    []
               )
        )


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
