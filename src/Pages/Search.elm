module Pages.Search exposing (Model, Msg, initialModel, update, view)

import Array
import Browser.Navigation
import Components.Icon as Icon
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl, GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
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
    | CloseSearch
    | NoOp


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

        CloseSearch ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.nextCard model.session.globalParams
                    |> AppUrl.toString
                )
            )

        NoOp ->
            ( model
            , Cmd.none
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
                , "pt-12"
                ]
            ]
            ([ SearchField.view
                results
                model.searchInputBuffer
                filters
                |> Html.map translateSearchFieldMsg
             , filterViewByAddedIn model.session.globalParams
             , filterViewByPartOfSpeech model.session.globalParams
             , filterViewByTags model.session.dict model.session.globalParams
             ]
                ++ (if
                        model.expandSearchResults
                            && (List.length model.session.globalParams.filters > 0)
                    then
                        searchResultView results model.session.globalParams
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []

                    else
                        []
                   )
            )
        , button
            [ onClick CloseSearch
            , Help.classNames
                [ "fixed"
                , "pin-r"
                , "pin-t"
                , "m-4"
                ]
            ]
            [ Icon.close "width: 2em; height: 2em" "#3d4852" ]
        ]


filterViewByAddedIn globalParams =
    filterSection "Added in"
        [ ( IsAddedIn (RelativeDays 7 7), "-1w" )
        , ( IsAddedIn (RelativeDays 14 7), "-2w" )
        , ( IsAddedIn (RelativeDays 21 7), "-3w" )
        , ( IsAddedIn (RelativeDays 28 7), "-4w" )
        , ( IsAddedIn (RelativeDays 1 1), "-1d" )
        , ( IsAddedIn (RelativeDays 2 1), "-2d" )
        , ( IsAddedIn (RelativeDays 3 1), "-3d" )
        , ( IsAddedIn (RelativeDays 4 1), "-4d" )
        , ( IsAddedIn (RelativeDays 5 1), "-5d" )
        , ( IsAddedIn (RelativeDays 6 1), "-6d" )
        , ( IsAddedIn (RelativeDays 7 1), "-7d" )
        , ( IsAddedIn (RelativeDays 8 1), "-8d" )
        , ( IsAddedIn (RelativeDays 9 1), "-9d" )
        , ( IsAddedIn (RelativeDays 10 1), "-10d" )
        , ( IsAddedIn (RelativeDays 11 1), "-11d" )
        , ( IsAddedIn (RelativeDays 12 1), "-12d" )
        , ( IsAddedIn (RelativeDays 13 1), "-13d" )
        , ( IsAddedIn (RelativeDays 14 1), "-14d" )
        ]
        (\filter ->
            AppUrl.search globalParams
                |> AppUrl.withFilters
                    (globalParams.filters
                        |> List.filter
                            (\f ->
                                case f of
                                    IsAddedIn _ ->
                                        False

                                    _ ->
                                        True
                            )
                        |> (\fs -> fs ++ addIfNotExists filter globalParams.filters)
                    )
        )
        globalParams.filters


filterViewByPartOfSpeech globalParams =
    filterSection "Part of speech"
        (PartOfSpeech.items |> List.map (\pos -> ( PartOfSpeechIs pos, PartOfSpeech.toString pos )))
        (\filter ->
            AppUrl.search globalParams
                |> AppUrl.withFilters
                    (globalParams.filters
                        |> List.filter
                            (\f ->
                                case f of
                                    PartOfSpeechIs _ ->
                                        False

                                    _ ->
                                        True
                            )
                        |> (\fs -> fs ++ addIfNotExists filter globalParams.filters)
                    )
        )
        globalParams.filters


filterViewByTags dict globalParams =
    filterSection "Tags"
        (dict
            |> Dictionary.tags
            |> List.map (\tag -> ( HasTag tag, tag ))
        )
        (\filter ->
            AppUrl.search globalParams
                |> AppUrl.withFilters
                    (globalParams.filters
                        |> List.filter ((/=) filter)
                        |> (\fs -> fs ++ addIfNotExists filter globalParams.filters)
                    )
        )
        globalParams.filters


addIfNotExists : Filter -> List Filter -> List Filter
addIfNotExists filter filters =
    if List.member filter filters then
        []

    else
        [ filter ]


filterSection : String -> List ( Filter, String ) -> (Filter -> AppUrl) -> List Filter -> Html Msg
filterSection title filters createUrl currentFilters =
    section [ Help.classNames [ "my-4" ] ]
        [ h3 [ Help.classNames [ "text-grey-darker" ] ] [ text title ]
        , ul
            [ Help.classNames
                [ "list-reset"
                , "flex"
                , "justify-center"
                , "flex-wrap"
                , "my-4"
                ]
            ]
            (filters
                |> List.reverse
                |> List.map
                    (\( filter, txt ) ->
                        li
                            []
                            [ a
                                [ href (createUrl filter |> AppUrl.toString)
                                , Help.classNames
                                    ([ "no-underline"
                                     , "rounded"
                                     , "inline-block"
                                     , "mr-1"
                                     , "mb-2"
                                     , "py-1"
                                     , "px-2"
                                     ]
                                        ++ (if List.member filter currentFilters then
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
                            ]
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
            , href (AppUrl.card entry.index globalParams |> AppUrl.toString)
            ]
            [ div [ Help.classNames [ "inline-block", "mr-2" ] ] [ span [] [ text entry.index ] ]
            , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] [ span [] [ text entry.translation ] ]
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

        SearchField.Focus ->
            NoOp
