module Pages.Search exposing (Model, Msg, initialModel, subscriptions, update, view)

import Array
import Browser.Navigation
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
import Html.Keyed
import Ports


type alias Model =
    { session : Session
    , filters : List Filter
    , searchInputBuffer : String
    , expandSearchResults : Bool
    , results : Dictionary
    , isScrolled : Bool
    }


type Msg
    = SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | ApplyFilter
    | UpdateFilters (List Filter)
    | CloseSearch
    | OnScrollChange Int
    | NoOp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Ports.scrollChange OnScrollChange ]


initialModel : Session -> Model
initialModel session =
    let
        originalFilters =
            session.globalParams.filters
    in
    { session = session
    , filters = originalFilters
    , searchInputBuffer = originalFilters |> Filter.toString
    , expandSearchResults = False
    , results = Filter.applied session.startTime session.dict originalFilters
    , isScrolled = False
    }


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        SearchInput text ->
            let
                filters =
                    Filter.parse text
            in
            ( { model
                | searchInputBuffer = text
                , filters = filters
                , results = Filter.applied model.session.startTime model.session.dict filters
              }
            , Cmd.none
            )

        ToggleSearchResults ->
            ( { model
                | expandSearchResults =
                    not model.expandSearchResults
              }
            , Cmd.none
            )

        ClearSearchText ->
            update model (SearchInput "")

        UpdateFilters filters ->
            update model (SearchInput (Filter.toString filters))

        ApplyFilter ->
            let
                globalParams =
                    model.session.globalParams
            in
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.nextCard { globalParams | filters = model.filters }
                    |> AppUrl.toString
                )
            )

        CloseSearch ->
            ( model
            , Browser.Navigation.back model.session.navigationKey 1
            )

        OnScrollChange scrollY ->
            ( { model | isScrolled = scrollY /= 0 }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.Keyed.node "div"
        [ Help.classNames [ "container", "max-w-md" ] ]
        (Help.flatten
            [ Help.V <|
                ( "search"
                , div [ Help.classNames [ "fixed", "w-full", "p-5", "max-w-md" ] ]
                    [ SearchField.view
                        model.results
                        model.searchInputBuffer
                        model.filters
                        model.isScrolled
                        |> Html.map translateSearchFieldMsg
                    ]
                )
            , Help.V <|
                ( "body"
                , div [ Help.classNames [ "py-20", "p-5" ] ]
                    (Help.flatten
                        [ Help.V <| filterViewByAddedIn model.filters
                        , Help.V <| filterViewByPartOfSpeech model.filters
                        , Help.V <| filterViewByTags model.session.dict model.filters
                        , Help.Q (model.expandSearchResults && (List.length model.filters > 0))
                            (\_ -> searchResultView model.results model.session.globalParams)
                        ]
                    )
                )
            , Help.V <|
                ( "apply"
                , div
                    [ Help.classNames
                        [ "fixed"
                        , "pin-b"
                        , "text-md"
                        , "w-full"
                        , "p-5"
                        , "max-w-md"
                        ]
                    ]
                    [ button
                        [ onClick CloseSearch
                        , Help.classNames
                            [ "bg-grey-lighter"
                            , "text-grey-dark"
                            , "w-full"
                            , "p-4"
                            , "shadow-md"
                            , "mb-2"
                            , "rounded"
                            ]
                        ]
                        [ text "Abbrechen" ]
                    , button
                        [ onClick ApplyFilter
                        , Help.classNames
                            ([ "w-full"
                             , "p-4"
                             , "shadow-md"
                             ]
                                ++ Help.btnClasses True (Array.length model.results == 0)
                            )
                        ]
                        [ text "Verwerden" ]
                    ]
                )
            ]
        )


filterViewByAddedIn filters =
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
            filters
                |> List.filter
                    (\f ->
                        case f of
                            IsAddedIn _ ->
                                False

                            _ ->
                                True
                    )
                |> (\fs -> fs ++ addIfNotExists filter filters)
        )
        filters


filterViewByPartOfSpeech filters =
    filterSection "Part of speech"
        (PartOfSpeech.items |> List.map (\pos -> ( PartOfSpeechIs pos, PartOfSpeech.toString pos )))
        (\filter ->
            filters
                |> List.filter
                    (\f ->
                        case f of
                            PartOfSpeechIs _ ->
                                False

                            _ ->
                                True
                    )
                |> (\fs -> fs ++ addIfNotExists filter filters)
        )
        filters


filterViewByTags dict filters =
    filterSection "Tags"
        (dict
            |> Dictionary.tags
            |> List.map (\tag -> ( HasTag tag, tag ))
        )
        (\filter ->
            filters
                |> List.filter ((/=) filter)
                |> (\fs -> fs ++ addIfNotExists filter filters)
        )
        filters


addIfNotExists : Filter -> List Filter -> List Filter
addIfNotExists filter filters =
    if List.member filter filters then
        []

    else
        [ filter ]


filterSection : String -> List ( Filter, String ) -> (Filter -> List Filter) -> List Filter -> Html Msg
filterSection title filters createFilters currentFilters =
    section [ Help.classNames [ "my-4" ] ]
        [ h3 [ Help.classNames [ "text-grey-darker" ] ] [ text title ]
        , ul
            [ Help.classNames
                [ "list-reset"
                , "flex"
                , "justify-center"
                , "flex-wrap"
                , "my-6"
                ]
            ]
            (filters
                |> List.reverse
                |> List.map
                    (\( filter, txt ) ->
                        li
                            []
                            [ div
                                [ onClick (UpdateFilters (createFilters filter))
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
                (ul [ Help.classNames [ "list-reset" ] ]
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
