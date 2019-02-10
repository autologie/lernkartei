module Pages.Search exposing (Model, Msg, initialModel, update, view)

import Array
import Browser.Navigation
import Components.Icon as Icon
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, input, li, p, span, text, ul)
import Html.Attributes exposing (href, id, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { session : Session
    , searchInputBuffer : String
    , expandSearchResults : Bool
    }


type Msg
    = SearchInput String
    | ToggleSearchResults
    | ClearSearchText


initialModel : Session -> Model
initialModel session =
    { session = session
    , searchInputBuffer =
        session.globalParams.filters |> Filter.toString
    , expandSearchResults = False
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SearchInput text ->
            case SearchField.error text of
                Just _ ->
                    ( { model | searchInputBuffer = text }, Cmd.none )

                _ ->
                    ( model
                    , Browser.Navigation.pushUrl model.session.navigationKey
                        (AppUrl.search model.session.globalParams
                            |> AppUrl.withFilters (Filter.parse text)
                            |> AppUrl.toString
                        )
                    )

        ToggleSearchResults ->
            ( { model
                | expandSearchResults =
                    not model.expandSearchResults
              }
            , Cmd.none
            )

        ClearSearchText ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.search model.session.globalParams
                    |> AppUrl.withoutFilters
                    |> AppUrl.toString
                )
            )


view : Model -> Html Msg
view model =
    let
        filters =
            model.session.globalParams.filters

        results =
            Filter.applied
                model.session.startTime
                model.session.dict
                filters
    in
    div
        [ Help.classNames
            [ "w-full"
            , "flex"
            , "justify-center"
            , "items-start"
            ]
        ]
        [ div
            [ Help.classNames
                [ "container"
                , "max-w-md"
                , "p-5"
                ]
            ]
            ([ SearchField.view
                results
                model.searchInputBuffer
                filters
                |> Html.map translateSearchFieldMsg
             , ul
                [ Help.classNames
                    [ "list-reset"
                    , "flex"
                    , "justify-center"
                    , "mb-2"
                    , "bg-grey-light"
                    , "rounded-full"
                    , "overflow-scroll"
                    , "p-2"
                    , "relative"
                    ]
                ]
                ([ 1, 2, 3, 4, 5, 6, 7 ]
                    |> List.reverse
                    |> List.map
                        (\n ->
                            li
                                []
                                [ a
                                    [ href
                                        (AppUrl.search model.session.globalParams
                                            |> AppUrl.withFilters
                                                (filters
                                                    |> List.filter
                                                        (\f ->
                                                            case f of
                                                                IsAddedIn _ ->
                                                                    False

                                                                _ ->
                                                                    True
                                                        )
                                                    |> (\fs -> fs ++ [ IsAddedIn (RelativeDays n 1) ])
                                                )
                                            |> AppUrl.toString
                                        )
                                    , Help.classNames
                                        [ "no-underline"
                                        , "block"
                                        , "text-white"
                                        , "rounded-full"
                                        , "p-2"
                                        , "mx-1"
                                        , "bg-blue"
                                        ]
                                    ]
                                    [ text ("-" ++ String.fromInt n ++ "d") ]
                                ]
                        )
                )
             ]
                ++ (if
                        True
                            || (model.expandSearchResults
                                    && (Array.length results > 0)
                                    && (List.length model.session.globalParams.filters > 0)
                               )
                    then
                        searchResultView results model.session.globalParams
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    else
                        []
                   )
            )
        ]


searchResultView : Dictionary -> GlobalQueryParams -> Maybe (Html Msg)
searchResultView results globalParams =
    case Array.length results of
        0 ->
            case globalParams.filters of
                [ Contains index ] ->
                    Just
                        (a
                            [ href (AppUrl.newEntry (Just index) globalParams |> AppUrl.toString)
                            , Help.classNames
                                (Help.btnClasses True False
                                    ++ [ "p-3"
                                       , "w-full"
                                       , "my-2"
                                       , "block"
                                       , "no-underline"
                                       ]
                                )
                            ]
                            [ text ("\"" ++ index ++ "\" hinzufügen") ]
                        )

                _ ->
                    Nothing

        _ ->
            Just
                (ul [ Help.classNames [ "list-reset", "py-3" ] ]
                    (results
                        |> Array.toList
                        |> List.sortBy Entry.toComparable
                        |> List.map (searchResultRow globalParams)
                    )
                )


searchResultRow : GlobalQueryParams -> Entry -> Html Msg
searchResultRow globalParams entry =
    li
        []
        [ a
            [ Help.classNames
                [ "py-3"
                , "block"
                , "text-left"
                , "rounded"
                , "cursor-pointer"
                , "text-black"
                , "hover:bg-grey-lighter"
                ]
            , href (AppUrl.card entry.de globalParams |> AppUrl.toString)
            ]
            [ div [ Help.classNames [ "inline-block", "mr-2" ] ] [ span [] [ text entry.de ] ]
            , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] [ span [] [ text entry.ja ] ]
            ]
        ]


translateSearchFieldMsg : SearchField.Msg -> Msg
translateSearchFieldMsg searchFieldMsg =
    case searchFieldMsg of
        SearchField.SearchInput txt ->
            SearchInput txt

        SearchField.ToggleSearchResults ->
            ToggleSearchResults

        SearchField.ClearSearchText ->
            ClearSearchText
