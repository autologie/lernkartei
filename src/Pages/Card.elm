module Pages.Card exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import AppUrl exposing (GlobalQueryParams)
import Array
import Browser.Navigation
import Components.Icon as Icon exposing (add)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.FilterCondition as FilterCondition
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode as Decode
import Ports
import Time


type alias Model =
    { isTranslated : Bool
    , entry : Entry
    , textDisposition : Maybe ( Int, Int, Float )
    , expandSearchResults : Bool
    , session : Session
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
    { isTranslated = False
    , entry = Dictionary.get entryDe session.dict
    , textDisposition = Nothing
    , expandSearchResults = False
    , session = session
    }


subscriptions _ =
    Sub.batch []


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        Translate ->
            ( { model
                | isTranslated = not model.isTranslated
                , textDisposition = Nothing
              }
            , Cmd.none
            )

        TextDispositionChange value ->
            ( { model
                | textDisposition = Just value
              }
            , Cmd.none
            )

        SearchInput text ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.card model.entry.de model.session.globalParams
                    |> AppUrl.withFilters text
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
            FilterCondition.applied model.session.startTime model.session.dict model.session.globalParams.filters
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
                                        (AppUrl.randomCard model.session.globalParams
                                            |> AppUrl.withFilters ("t:" ++ String.fromInt -n ++ "d+1d")
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
                [ input
                    [ type_ "text"
                    , onInput SearchInput
                    , Help.classNames
                        [ "border-b"
                        , "text-grey-darkest"
                        , "bg-transparent"
                        , "w-full"
                        , "text-lg"
                        , "py-2"
                        ]
                    , value (model.session.globalParams.filters |> Maybe.withDefault "")
                    ]
                    []
                , resultCountView model.session.startTime results model
                ]
           )
         ]
            ++ (case ( model.expandSearchResults || Array.length results == 0, model.session.globalParams.filters ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView results text model.session.globalParams ) ]

                    _ ->
                        []
               )
            ++ [ ( "card", cardView model results model.entry ) ]
            ++ [ ( "addButton", addButton model.session.globalParams ) ]
        )


resultCountView : Time.Posix -> Dictionary -> Model -> Html Msg
resultCountView startTime results model =
    let
        resultCount =
            results |> Array.length

        ( isFiltered, isClickable ) =
            model.session.globalParams.filters
                |> Maybe.map (\_ -> ( True, resultCount > 0 ))
                |> Maybe.withDefault ( False, False )

        prefix =
            if isClickable then
                ""

            else
                "Alle "

        extraBtnClasses =
            [ "px-4", "py-2", "ml-px" ]
    in
    ul
        [ Help.classNames
            [ "list-reset"
            , "text-sm"
            , "my-2"
            , "absolute"
            , "pin-r"
            , "pin-t"
            , "flex"
            ]
        ]
        ([ li []
            [ button
                [ Help.classNames
                    (Help.groupedBtnClasses isClickable
                        (not isClickable)
                        True
                        (not isFiltered)
                        ++ extraBtnClasses
                    )
                , onClick ToggleSearchResults
                ]
                [ text (prefix ++ (resultCount |> String.fromInt) ++ " Wörter") ]
            ]
         ]
            ++ (if isFiltered then
                    [ li []
                        [ button
                            [ Help.classNames
                                (Help.groupedBtnClasses True
                                    False
                                    (not isClickable)
                                    True
                                    ++ extraBtnClasses
                                )
                            , onClick ClearSearchText
                            ]
                            [ text "X" ]
                        ]
                    ]

                else
                    []
               )
        )


searchResultView : Dictionary -> String -> GlobalQueryParams -> Html Msg
searchResultView results searchText globalParams =
    case Array.length results of
        0 ->
            a
                [ href (AppUrl.newEntry (Just searchText) globalParams |> AppUrl.toString)
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
                [ text ("\"" ++ searchText ++ "\" hinzufügen") ]

        _ ->
            ul [ Help.classNames [ "list-reset", "py-3" ] ]
                (results
                    |> Array.toList
                    |> List.sortBy Entry.toComparable
                    |> List.map (searchResultRow searchText)
                )


searchResultRow searchText entry =
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
            , href
                (AppUrl.card entry.de AppUrl.emptyParams
                    |> AppUrl.withFilters searchText
                    |> AppUrl.toString
                )
            ]
            [ div [ Help.classNames [ "inline-block", "mr-2" ] ] (hilighted searchText entry.de)
            , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText entry.ja)
            ]
        ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView : Model -> Dictionary -> Entry -> Html Msg
cardView model results entry =
    let
        textToShow =
            if model.isTranslated then
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
            ([ cardBehindView 1.6 5 -1
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
                            (AppUrl.editorFor entry.de AppUrl.emptyParams
                                |> (searchText |> Maybe.map AppUrl.withFilters |> Maybe.withDefault AppUrl.withoutFilters)
                                |> AppUrl.toString
                            )
                        , Help.classNames [ "text-blue", "no-underline" ]
                        ]
                        [ text "Edit" ]
                    ]
                ]
             ]
                ++ (if model.isTranslated then
                        [ entryDetailView entry ]

                    else
                        []
                   )
            )
        , a
            [ Help.classNames
                (Help.btnClasses True (not hasNext)
                    ++ [ "my-5"
                       , "p-4"
                       , "text-lg"
                       , "w-full"
                       ]
                )
            , href
                (AppUrl.randomCard AppUrl.emptyParams
                    |> (searchText |> Maybe.map AppUrl.withFilters |> Maybe.withDefault AppUrl.withoutFilters)
                    |> AppUrl.toString
                )
            ]
            [ text "Nächst" ]
        ]


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


entryDetailView { de, pos, ja, example } =
    div
        [ Help.classNames
            [ "text-grey-dark"
            , "px-8"
            , "py-4"
            , "leading-normal"
            , "bg-grey-lighter"
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
                                , p [ Help.classNames [ "whitespace-pre" ] ] [ text (Entry.censorExample ex) ]
                                ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


addButton globalParams =
    a
        [ Help.classNames
            [ "fixed"
            , "pin-r"
            , "pin-b"
            , "rounded-full"
            , "bg-blue"
            , "text-white"
            , "text-xl"
            , "m-4"
            , "p-2"
            , "w-16"
            , "h-16"
            , "flex"
            , "justify-center"
            , "items-center"
            , "z-30"
            , "shadow-lg"
            , "no-underline"
            ]
        , href (AppUrl.newEntry Nothing globalParams |> AppUrl.toString)
        ]
        [ Icon.add "width: .6em; height: .6em;"
        ]
