module Pages.Entries exposing (Model, Msg, init, update, view)

import Array
import Browser.Navigation
import Components.Button as Button
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl, GlobalQueryParams)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, div, li, span, text, ul)
import Html.Attributes exposing (href)


type alias Model =
    { session : Session
    , isScrolled : Bool
    }


type Msg
    = NavigateTo AppUrl
    | NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, isScrolled = False }, Cmd.none )


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        NavigateTo url ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey (url |> AppUrl.toString)
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        globalParams =
            model.session.globalParams

        results =
            Filter.applied model.session.startTime
                model.session.dict
                globalParams.filters

        maybeKeyword =
            globalParams.filters
                |> List.filterMap Filter.keyword
                |> List.head
    in
    div [ Help.classNames [ "container", "max-w-md" ] ]
        (Help.flatten
            [ Help.V <|
                div [ Help.classNames [ "fixed", "w-full", "p-5", "max-w-md" ] ]
                    [ SearchField.view
                        results
                        (Filter.toString globalParams.filters)
                        globalParams.filters
                        model.isScrolled
                        |> Html.map (translateSearchFieldMsg globalParams)
                    ]
            , Help.M <|
                (resultsView globalParams results
                    |> Maybe.map
                        (\el ->
                            div
                                [ Help.classNames
                                    [ "py-24"
                                    , "p-5"
                                    ]
                                ]
                                [ el ]
                        )
                )
            , Help.V <|
                Button.floatingGroup
                    [ Button.addNewEntry (AppUrl.newEntry maybeKeyword globalParams)
                    ]
            ]
        )


resultsView : GlobalQueryParams -> Array.Array Entry -> Maybe (Html Msg)
resultsView globalParams results =
    case Array.length results of
        0 ->
            case globalParams.filters of
                [ Filter.Contains index ] ->
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
                [ "p-3"
                , "block"
                , "text-left"
                , "rounded"
                , "cursor-pointer"
                , "text-black"
                , "hover:bg-grey-lighter"
                ]
            , href (AppUrl.entry entry.index globalParams |> AppUrl.toString)
            ]
            [ div [ Help.classNames [ "inline-block", "mr-2" ] ] [ span [] [ text entry.index ] ]
            , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] [ span [] [ text entry.translation ] ]
            ]
        ]


translateSearchFieldMsg : GlobalQueryParams -> SearchField.Msg -> Msg
translateSearchFieldMsg globalParams searchFieldMsg =
    case searchFieldMsg of
        SearchField.ClearSearchText ->
            NavigateTo (AppUrl.search { globalParams | filters = [] })

        SearchField.Focus ->
            NavigateTo (AppUrl.search globalParams)

        _ ->
            NoOp
