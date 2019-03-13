module Pages.Entries exposing (Model, Msg, init, update, view)

import Array
import Browser.Navigation
import Components.Button as Button
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl, GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, div, li, span, text, ul)
import Html.Attributes exposing (class, href)


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
            model.session.progress.shown
                ++ model.session.progress.toBeShown
                |> List.concatMap
                    (\index ->
                        Dictionary.get index model.session.dict
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                    )
                |> List.sortBy Entry.toComparable

        maybeKeyword =
            globalParams.filters
                |> List.filterMap Filter.keyword
                |> List.head
    in
    div [ class "container max-w-md" ]
        [ div [ class "fixed w-full p-5 max-w-md" ]
            [ SearchField.view
                results
                (Filter.toString globalParams.filters)
                globalParams.filters
                model.isScrolled
                |> Html.map (translateSearchFieldMsg globalParams)
            ]
        , (resultsView globalParams results
            |> Maybe.map (\el -> div [ class "py-24 p-5" ] [ el ])
          )
            |> Maybe.withDefault (text "")
        , Button.floatingGroup
            [ Button.addNewEntry (AppUrl.createEntry maybeKeyword globalParams)
            ]
        ]


resultsView : GlobalQueryParams -> List Entry -> Maybe (Html Msg)
resultsView globalParams results =
    if List.isEmpty results then
        case globalParams.filters of
            [ Filter.Contains index ] ->
                Just
                    (a
                        [ href (AppUrl.createEntry (Just index) globalParams |> AppUrl.toString)
                        , class "p-3 w-full my-2 block no-underline"
                        , Help.btnClasses True False
                        ]
                        [ text ("\"" ++ index ++ "\" hinzufügen") ]
                    )

            _ ->
                Nothing

    else
        Just
            (ul [ class "list-reset" ]
                (results |> List.map (searchResultRow globalParams))
            )


searchResultRow : GlobalQueryParams -> Entry -> Html Msg
searchResultRow globalParams entry =
    li
        []
        [ a
            [ class "p-3 block text-left rounded cursor-pointer text-black hover:bg-grey-lighter"
            , href (AppUrl.entry entry.index globalParams |> AppUrl.toString)
            ]
            [ div [ class "inline-block mr-2" ] [ span [] [ text entry.index ] ]
            , div [ class "inline-block text-grey-dark" ] [ span [] [ text entry.translation ] ]
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
