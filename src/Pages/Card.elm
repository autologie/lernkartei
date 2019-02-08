module Pages.Card exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import Array
import Browser.Navigation
import Components.Icon as Icon exposing (add)
import Data.AppUrl as AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode as Decode
import Ports


type alias Model =
    { entry : Entry
    , textDisposition : Maybe ( Int, Int, Float )
    , expandSearchResults : Bool
    , session : Session
    , searchInputBuffer : String
    }


type Msg
    = Translate
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | ToggleStar


initialModel : Session -> String -> Model
initialModel session entryDe =
    { entry = Dictionary.get entryDe session.dict
    , textDisposition = Nothing
    , expandSearchResults = False
    , session = session
    , searchInputBuffer =
        session.globalParams.filters |> Filter.toString
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        Translate ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.card model.entry.de model.session.globalParams
                    |> AppUrl.toggleTranslate
                    |> AppUrl.toString
                )
            )

        TextDispositionChange value ->
            ( { model
                | textDisposition = Just value
              }
            , Cmd.none
            )

        SearchInput text ->
            case errorInFilterText text of
                Just _ ->
                    ( { model | searchInputBuffer = text }, Cmd.none )

                _ ->
                    ( model
                    , Browser.Navigation.pushUrl model.session.navigationKey
                        (AppUrl.card model.entry.de model.session.globalParams
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
                (AppUrl.card model.entry.de model.session.globalParams
                    |> AppUrl.withoutFilters
                    |> AppUrl.toString
                )
            )

        ToggleStar ->
            let
                entry =
                    model.entry

                updatedEntry =
                    { entry | starred = not entry.starred }
            in
            ( { model
                | entry = updatedEntry
                , session = model.session |> Session.withDict (model.session.dict |> Array.map (Help.replaceEntry entry updatedEntry))
              }
            , Ports.saveEntry ( model.session.userId, Entry.encode updatedEntry )
            )


view : Model -> Html Msg
view model =
    let
        results =
            Filter.applied model.session.startTime model.session.dict model.session.globalParams.filters
    in
    Html.Keyed.node "div"
        [ Help.classNames
            [ "container"
            , "max-w-md"
            , "p-5"
            ]
        ]
        ([ ( "dateLinks"
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
                                        (AppUrl.nextCard model.session.globalParams
                                            |> AppUrl.withFilters [ IsAddedIn (RelativeDays n 1) ]
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
           )
         , ( "search"
           , div [ Help.classNames [ "relative", "mb-5" ] ]
                (input
                    [ type_ "text"
                    , onInput SearchInput
                    , Help.classNames
                        [ "border-b"
                        , "text-grey-darkest"
                        , "bg-transparent"
                        , "w-full"
                        , "text-sm"
                        , "py-4"
                        ]
                    , value model.searchInputBuffer
                    ]
                    []
                    :: (errorInFilterText model.searchInputBuffer
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
                    ++ [ resultCountView results model ]
                )
           )
         ]
            ++ (case ( model.expandSearchResults || Array.length results == 0, model.session.globalParams.filters ) of
                    ( False, _ ) ->
                        []

                    ( _, [] ) ->
                        []

                    _ ->
                        searchResultView results
                            model.session.globalParams
                            |> Maybe.map (\el -> [ ( "searchResult", el ) ])
                            |> Maybe.withDefault []
               )
            ++ [ ( "card", cardView model results model.entry )
               , ( "addButton", addButton model.entry model.session.globalParams )
               ]
        )


resultCountView : Dictionary -> Model -> Html Msg
resultCountView results model =
    let
        resultCount =
            results |> Array.length

        isFiltered =
            List.length model.session.globalParams.filters > 0

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
                [ "p-3"
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


cardView : Model -> Dictionary -> Entry -> Html Msg
cardView model results entry =
    let
        textToShow =
            if model.session.globalParams.translate then
                entry.ja

            else
                entry.de

        simpleDe =
            Entry.withoutArticle entry

        hasNext =
            Array.length results > 1

        searchText =
            model.session.globalParams.filters
    in
    div []
        [ div
            [ Help.classNames
                [ "rounded"
                , "bg-white"
                , "shadow-lg"
                , "relative"
                , "mt-4"
                , "mb-8"
                ]
            ]
            [ cardBehindView 1.6 5 -1
            , cardBehindView 2 10 -2
            , div
                [ Help.classNames
                    [ "select-none"
                    , "h-64"
                    , "text-grey-darkest"
                    , "relative"
                    ]
                , onClick Translate
                ]
                [ div
                    ([ id "text", attribute "data-text" textToShow ]
                        ++ (case model.textDisposition of
                                Just ( x, y, scale ) ->
                                    [ Help.classNames
                                        [ "absolute"
                                        , "inline-block"
                                        ]
                                    , style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                                    , style "left" (String.fromInt x)
                                    , style "top" (String.fromInt y)
                                    ]

                                Nothing ->
                                    [ Help.classNames [ "inline-block text-transparent" ] ]
                           )
                    )
                    [ text textToShow ]
                , div
                    [ Help.classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-l"
                        , "m-2"
                        ]
                    ]
                    [ button
                        [ stopPropagationOn "click" (Decode.map (\msg -> ( msg, True )) (Decode.succeed ToggleStar))
                        , Help.classNames
                            ([ "text-lg"
                             , "text-black"
                             ]
                                ++ (if entry.starred then
                                        [ "text-orange" ]

                                    else
                                        [ "text-grey" ]
                                   )
                            )
                        , if entry.starred then
                            style "text-shadow" "0 0 .4em rgba(0,0,0,.1)"

                          else
                            style "" ""
                        ]
                        [ text
                            (if entry.starred then
                                "★"

                             else
                                "☆︎"
                            )
                        ]
                    ]
                , div
                    [ Help.classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-r"
                        , "m-2"
                        ]
                    ]
                    [ a
                        [ href ("https://de.wiktionary.org/wiki/" ++ simpleDe)
                        , target "_blank"
                        , Help.classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Untersuchen" ]
                    , a
                        [ href ("https://translate.google.co.jp/m/translate?hl=ja#view=home&op=translate&sl=de&tl=ja&text=" ++ simpleDe)
                        , target "_blank"
                        , Help.classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Hören" ]
                    , a
                        [ href
                            (AppUrl.editorFor entry.de model.session.globalParams
                                |> AppUrl.withFilters searchText
                                |> AppUrl.toString
                            )
                        , Help.classNames [ "text-blue", "no-underline" ]
                        ]
                        [ text "Edit" ]
                    ]
                ]
            ]
        , a
            [ Help.classNames
                (Help.btnClasses True (not hasNext)
                    ++ [ "my-5"
                       , "p-4"
                       , "text-lg"
                       , "w-full"
                       ]
                )
            , href (AppUrl.nextCard model.session.globalParams |> AppUrl.toString)
            ]
            [ text "Nächst" ]
        , entryDetailView entry
        ]


cardBehindView : Float -> Float -> Int -> Html Msg
cardBehindView rotateValue y zIndex =
    div
        [ Help.classNames
            [ "absolute"
            , "w-full"
            , "h-full"
            , "shadow"
            , "bg-white"
            , "rounded"
            ]
        , style "transform" ("rotate(" ++ String.fromFloat rotateValue ++ "deg) translateY(" ++ String.fromFloat y ++ "px)")
        , style "z-index" (String.fromInt zIndex)
        ]
        []


entryDetailView : Entry -> Html Msg
entryDetailView { pos, example, tags } =
    div
        [ Help.classNames
            [ "text-grey-dark"
            , "py-4"
            , "leading-normal"
            , "text-left"
            , "rounded-b"
            ]
        ]
        ([ section [ Help.classNames [ "mb-2" ] ]
            [ h3 [] [ text "Teil" ]
            , p [] [ text (PartOfSpeech.toString pos) ]
            ]
         ]
            ++ (example
                    |> Maybe.map
                        (\ex ->
                            [ section [ Help.classNames [ "mb-2" ] ]
                                [ h3 [] [ text "Beispiel" ]
                                , p [ Help.classNames [ "whitespace-prewrap" ] ] [ text (Entry.censorExample ex) ]
                                ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
            ++ [ section [ Help.classNames [ "mb-2" ] ]
                    [ h3 [] [ text "Etikett" ]
                    , p []
                        [ text
                            (if List.length tags > 0 then
                                tags |> String.join ", "

                             else
                                "-"
                            )
                        ]
                    ]
               ]
        )


addButton : Entry -> GlobalQueryParams -> Html Msg
addButton entry globalParams =
    div
        [ Help.classNames
            [ "fixed"
            , "pin-r"
            , "pin-b"
            , "m-4"
            , "z-30"
            , "flex"
            , "justify-center"
            , "items-center"
            , "flex-col"
            ]
        ]
        [ a
            [ Help.classNames
                [ "rounded-full"
                , "bg-grey"
                , "text-xl"
                , "shadow-md"
                , "flex"
                , "w-8"
                , "h-8"
                , "justify-center"
                , "items-center"
                , "mb-2"
                ]
            , href
                (AppUrl.card entry.de globalParams
                    |> AppUrl.toggleShuffle
                    |> AppUrl.toString
                )
            ]
            [ if globalParams.shuffle then
                Icon.shuffle "width: .4em; height: .4em;"

              else
                Icon.noShuffle "width: .4em; height: .4em;"
            ]
        , a
            [ Help.classNames
                [ "rounded-full"
                , "bg-green"
                , "text-white"
                , "text-xl"
                , "p-2"
                , "w-16"
                , "h-16"
                , "flex"
                , "justify-center"
                , "items-center"
                , "shadow-lg"
                , "no-underline"
                ]
            , href (AppUrl.newEntry Nothing globalParams |> AppUrl.toString)
            ]
            [ Icon.add "width: .6em; height: .6em;" ]
        ]


errorInFilterText : String -> Maybe String
errorInFilterText filterText =
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
