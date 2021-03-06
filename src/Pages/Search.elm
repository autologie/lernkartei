module Pages.Search exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Browser.Dom
import Browser.Navigation
import Components.SearchField as SearchField
import Components.Tag as Tag
import Data.AppUrl as AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h3, li, section, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Html.Keyed
import Ports
import Task


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


init : Session -> ( Model, Cmd Msg )
init session =
    let
        originalFilters =
            session.globalParams.filters
    in
    ( { session = session
      , filters = originalFilters
      , searchInputBuffer = originalFilters |> Filter.toString
      , expandSearchResults = False
      , results = Filter.applied session.startTime session.dict originalFilters
      , isScrolled = False
      }
    , Browser.Dom.focus "search-input" |> Task.attempt (\_ -> NoOp)
    )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    let
        globalParams =
            model.session.globalParams
    in
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
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.entries { globalParams | filters = model.filters } |> AppUrl.toString)
            )

        ClearSearchText ->
            update model (SearchInput "")

        UpdateFilters filters ->
            update model (SearchInput (Filter.toString filters))

        ApplyFilter ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.nextEntry { globalParams | filters = model.filters }
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
    let
        maybeKeyword =
            if Dictionary.nonEmpty model.results then
                Nothing

            else
                model.filters
                    |> List.filterMap Filter.keyword
                    |> List.head
    in
    Html.Keyed.node "div"
        [ class "container max-w-md" ]
        [ ( "search"
          , div [ class "fixed w-full p-5 max-w-md" ]
                [ SearchField.view
                    model.results
                    model.searchInputBuffer
                    model.filters
                    model.isScrolled
                    |> Html.map translateSearchFieldMsg
                ]
          )
        , ( "body"
          , div [ class "py-24 p-5" ]
                [ filterViewByAddedIn model.filters
                , filterViewByPartOfSpeech model.filters
                , filterViewByTags model.session.dict model.filters
                , filterViewByStar model.filters
                ]
          )
        , ( "apply"
          , div
                [ class "fixed pin-b text-md w-full p-5 flex flex-wrap max-w-md" ]
                [ maybeKeyword
                    |> Maybe.map (\keyword -> addButton keyword model.session.globalParams)
                    |> Maybe.withDefault (text "")
                , button
                    [ onClick CloseSearch
                    , class "bg-grey-lighter text-grey-dark p-4 shadow-md mr-1 rounded flex-1"
                    ]
                    [ text "Abbrechen" ]
                , button
                    [ onClick ApplyFilter
                    , class "ml-1 p-4 shadow-md flex-1"
                    , Help.btnClasses True (Dictionary.isEmpty model.results)
                    ]
                    [ text "Verwerden" ]
                ]
          )
        ]


addButton : String -> GlobalQueryParams -> Html msg
addButton keyword params =
    a
        [ class "w-full p-4 mb-2"
        , Help.btnClasses True False
        , href (AppUrl.createEntry (Just keyword) params |> AppUrl.toString)
        ]
        [ text ("\"" ++ keyword ++ "\" hinzufügen") ]


filterViewByAddedIn : List Filter -> Html Msg
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


filterViewByPartOfSpeech : List Filter -> Html Msg
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


filterViewByTags : Dictionary -> List Filter -> Html Msg
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


filterViewByStar : List Filter -> Html Msg
filterViewByStar filters =
    filterSection "Star"
        [ ( IsStarred, "Starred" ) ]
        (\_ -> filters |> Help.toggle IsStarred)
        filters


addIfNotExists : Filter -> List Filter -> List Filter
addIfNotExists filter filters =
    if List.member filter filters then
        []

    else
        [ filter ]


filterSection : String -> List ( Filter, String ) -> (Filter -> List Filter) -> List Filter -> Html Msg
filterSection title filters createFilters currentFilters =
    section [ class "my-4" ]
        [ h3 [ class "text-grey-darker" ] [ text title ]
        , ul
            [ class "list-reset flex justify-center flex-wrap my-6" ]
            (filters
                |> List.reverse
                |> List.map
                    (\( filter, txt ) ->
                        Tag.view
                            txt
                            (List.member filter currentFilters)
                            (UpdateFilters (createFilters filter))
                    )
                |> List.map (\tag -> li [] [ tag ])
            )
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
