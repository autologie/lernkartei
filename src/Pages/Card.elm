module Pages.Card exposing (Model, Msg(..), initialModel, update, view)

import Array
import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Entry exposing (Entry)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode as Decode
import PartOfSpeech
import Ports
import Time


type alias Model =
    { isTranslated : Bool
    , entry : Entry
    , textDisposition : Maybe ( Int, Int, Float )
    , expandSearchResults : Bool
    }


type Msg
    = Translate
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | ToggleStar


initialModel : Entry -> Model
initialModel entry =
    { isTranslated = False
    , entry = entry
    , textDisposition = Nothing
    , expandSearchResults = False
    }


update : Key -> String -> Dictionary -> Model -> Msg -> ( Model, Dictionary, Cmd Msg )
update navigationKey userId dict model msg =
    case msg of
        Translate ->
            ( { model
                | isTranslated = not model.isTranslated
                , textDisposition = Nothing
              }
            , dict
            , Cmd.none
            )

        TextDispositionChange value ->
            ( { model
                | textDisposition = Just value
              }
            , dict
            , Cmd.none
            )

        SearchInput text ->
            ( model
            , dict
            , Browser.Navigation.pushUrl navigationKey
                ("/entries/" ++ model.entry.de ++ "?filter=" ++ text)
            )

        ToggleSearchResults ->
            ( { model
                | expandSearchResults =
                    not model.expandSearchResults
              }
            , dict
            , Cmd.none
            )

        ClearSearchText ->
            ( model
            , dict
            , Browser.Navigation.pushUrl navigationKey ("/entries/" ++ model.entry.de)
            )

        ToggleStar ->
            let
                entry =
                    model.entry

                updatedEntry =
                    { entry | starred = not entry.starred }
            in
            ( { model | entry = updatedEntry }
            , dict |> Array.map (Help.replaceEntry entry updatedEntry)
            , Ports.saveEntry ( userId, Entry.encode updatedEntry )
            )


view : Time.Posix -> Maybe String -> Dictionary -> Model -> Html Msg
view startTime searchText results model =
    Html.Keyed.node "div"
        [ Help.classNames
            [ "container"
            , "max-w-md"
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
                                    [ href ("/entries/_random?filter=t:" ++ String.fromInt -n ++ "d+1d")
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
                    , value (searchText |> Maybe.withDefault "")
                    ]
                    []
                , resultCountView startTime searchText results model
                ]
           )
         ]
            ++ (case ( model.expandSearchResults || Array.length results == 0, searchText ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView results text ) ]

                    _ ->
                        []
               )
            ++ [ ( "card", cardView searchText model results model.entry ) ]
            ++ [ ( "addButton", addButton searchText ) ]
        )


resultCountView : Time.Posix -> Maybe String -> Dictionary -> Model -> Html Msg
resultCountView startTime searchText results model =
    let
        resultCount =
            results |> Array.length

        ( isFiltered, isClickable ) =
            searchText
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


searchResultView : Dictionary -> String -> Html Msg
searchResultView results searchText =
    case Array.length results of
        0 ->
            a
                [ href ("/entries/_new?de=" ++ searchText ++ "&filter=" ++ searchText)
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
            , href ("/entries/" ++ entry.de ++ ("?filter=" ++ searchText))
            ]
            [ div [ Help.classNames [ "inline-block", "mr-2" ] ] (hilighted searchText entry.de)
            , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText entry.ja)
            ]
        ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView : Maybe String -> Model -> Dictionary -> Entry -> Html Msg
cardView searchText model results entry =
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
    in
    div []
        [ div
            [ Help.classNames
                [ "rounded"
                , "bg-white"
                , "shadow-lg"
                ]
            ]
            ([ div
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
                            ("/entries/"
                                ++ entry.de
                                ++ "/_edit"
                                ++ (searchText
                                        |> Maybe.map (\st -> "?filter=" ++ st)
                                        |> Maybe.withDefault ""
                                   )
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
                ("/entries/_random"
                    ++ (searchText
                            |> Maybe.map (\st -> "?filter=" ++ st)
                            |> Maybe.withDefault ""
                       )
                )
            ]
            [ text "Nächst" ]
        ]


entryDetailView { de, pos, ja, example } =
    div
        [ Help.classNames
            [ "text-grey-dark"
            , "px-8"
            , "py-4"
            , "leading-normal"
            , "bg-grey-lighter"
            , "text-left"
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
                                , p [] [ text (Entry.censorExample ex) ]
                                ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


addButton searchText =
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
        , href
            ("/entries/_new"
                ++ (searchText
                        |> Maybe.map (\st -> "?filter=" ++ st)
                        |> Maybe.withDefault ""
                   )
            )
        ]
        [ div [ style "margin-top" "-8px" ] [ text "+" ] ]