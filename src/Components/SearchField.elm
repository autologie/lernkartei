module Components.SearchField exposing (Msg(..), error, view)

import Array
import Components.Icon as Icon
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Filter as Filter exposing (Filter(..))
import Help
import Html exposing (Html, button, div, input, p, text, ul)
import Html.Attributes exposing (id, type_, value)
import Html.Events exposing (onClick, onFocus, onInput)


type Msg
    = SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | Focus


view : Dictionary -> String -> List Filter -> Bool -> Html Msg
view results searchInputBuffer filters needShadow =
    div [ Help.classNames [ "relative" ] ]
        (Help.flatten
            [ Help.V <|
                input
                    [ type_ "text"
                    , onInput SearchInput
                    , onFocus Focus
                    , id "search-input"
                    , Help.classNames
                        (Help.flatten
                            [ Help.V "text-grey-darkest"
                            , Help.V "bg-grey-light"
                            , Help.V "w-full"
                            , Help.V "text-sm"
                            , Help.V "py-4"
                            , Help.V "px-2"
                            , Help.V "rounded"
                            , Help.O needShadow (\_ -> "shadow-md")
                            ]
                        )
                    , value searchInputBuffer
                    ]
                    []
            , Help.M <|
                (error searchInputBuffer
                    |> Maybe.map
                        (\errorMessage ->
                            p
                                [ Help.classNames
                                    [ "text-red"
                                    , "my-4"
                                    ]
                                ]
                                [ text errorMessage ]
                        )
                )
            , Help.V <| resultCountView results filters
            ]
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
            , "m-2"
            , "absolute"
            , "pin-r"
            , "pin-t"
            , "flex"
            ]
        ]
        (Help.flatten
            [ button
                [ Help.classNames
                    (List.concat
                        [ Help.groupedBtnClasses isClickable
                            (not isClickable)
                            True
                            (not isFiltered)
                        , [ "px-4", "py-2", "ml-px" ]
                        ]
                    )
                , onClick ToggleSearchResults
                ]
                [ text (prefix ++ (resultCount |> String.fromInt) ++ " Wörter") ]
                |> Help.V
            , Help.O isFiltered
                (\_ ->
                    button
                        [ Help.classNames
                            (List.concat
                                [ Help.groupedBtnClasses True
                                    False
                                    (not isClickable)
                                    True
                                , [ "px-2", "py-2", "ml-px" ]
                                ]
                            )
                        , onClick ClearSearchText
                        ]
                        [ Icon.close "width: 1.2em; height: 1.2em" "white" ]
                )
            ]
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
