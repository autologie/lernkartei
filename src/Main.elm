port module Main exposing (Direction(..), Entry(..), Model, Msg(..), addButton, btnClasses, cardView, censorExample, classNames, groupedBtnClasses, hilighted, initialModel, isMatchedTo, main, parseDict, parseLine, resultCountView, searchResultRow, searchResultView, searchResults, textDisposition, update, view)

import Array exposing (Array)
import Browser
import Html exposing (a, button, div, h1, input, label, li, p, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Http
import Random
import Regex


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


main : Program ( String, Int ) Model Msg
main =
    Browser.document
        { init =
            \( dictUrl, randomSeed ) ->
                ( initialModel randomSeed
                , Http.get
                    { url = dictUrl
                    , expect = Http.expectString ReceiveDict
                    }
                )
        , subscriptions = \_ -> Sub.batch [ textDisposition TextDispositionChange ]
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        }


type Entry
    = Entry String String (Maybe String)


type Direction
    = DeToJa
    | JaToDe


type alias Model =
    { dict : Array Entry
    , showing : Maybe ( Bool, Entry )
    , seed : Random.Seed
    , direction : Direction
    , textDisposition : Maybe ( Int, Int, Float )
    , searchText : Maybe String
    , expandSearchResults : Bool
    , appMode : AppMode
    }


type AppMode
    = ShowCard
    | AddWord Entry


initialModel randomSeed =
    { dict = Array.empty
    , showing = Nothing
    , seed = Random.initialSeed randomSeed
    , direction = DeToJa
    , textDisposition = Nothing
    , searchText = Nothing
    , expandSearchResults = False
    , appMode = ShowCard
    }


type Msg
    = NextRandomWord
    | Translate
    | ReceiveDict (Result Http.Error String)
    | DirectionChange
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ClickSearchResult Entry
    | ToggleSearchResults
    | ClearSearchText
    | AddButtonClicked
    | CloseEditor
    | SaveAndCloseEditor
    | WordChange Entry


update msg model =
    case msg of
        NextRandomWord ->
            let
                entries =
                    searchResults model

                ( index, nextSeed ) =
                    Random.step
                        (Random.int 0 (Array.length entries - 1))
                        model.seed
            in
            ( { model
                | showing =
                    entries
                        |> Array.get index
                        |> Maybe.map (\entry -> ( False, entry ))
                , seed = nextSeed
                , textDisposition = Nothing
              }
            , Cmd.none
            )

        ReceiveDict (Ok str) ->
            update NextRandomWord { model | dict = parseDict str }

        ReceiveDict (Err _) ->
            ( model, Cmd.none )

        Translate ->
            ( { model
                | showing =
                    model.showing
                        |> Maybe.map
                            (\( showTranslation, entry ) ->
                                ( not showTranslation, entry )
                            )
                , textDisposition = Nothing
              }
            , Cmd.none
            )

        DirectionChange ->
            ( { model
                | direction =
                    case model.direction of
                        DeToJa ->
                            JaToDe

                        JaToDe ->
                            DeToJa
                , textDisposition = Nothing
              }
            , Cmd.none
            )

        TextDispositionChange value ->
            ( { model | textDisposition = Just value }, Cmd.none )

        SearchInput text ->
            ( { model
                | searchText =
                    if text == "" then
                        Nothing

                    else
                        Just text
              }
            , Cmd.none
            )

        ClickSearchResult entry ->
            ( { model
                | searchText = Nothing
                , showing = Just ( False, entry )
                , textDisposition = Nothing
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
            ( { model
                | searchText = Nothing
              }
            , Cmd.none
            )

        AddButtonClicked ->
            ( { model | appMode = AddWord (Entry "" "" Nothing) }, Cmd.none )

        CloseEditor ->
            ( { model | appMode = ShowCard }, Cmd.none )

        SaveAndCloseEditor ->
            case model.appMode of
                AddWord entry ->
                    ( { model
                        | appMode = ShowCard
                        , dict = Array.append (Array.fromList [ entry ]) model.dict
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        WordChange updatedEntry ->
            ( { model | appMode = AddWord updatedEntry }, Cmd.none )


parseDict : String -> Array Entry
parseDict dict =
    dict
        |> String.split "\n"
        |> List.filterMap parseLine
        |> Array.fromList


parseLine : String -> Maybe Entry
parseLine line =
    if line == "" || (line |> String.slice 0 1) == "#" then
        Nothing

    else
        case line |> String.split "\t" of
            [ de, ja ] ->
                Just (Entry de ja Nothing)

            [ de, ja, example ] ->
                Just (Entry de ja (Just example))

            _ ->
                Nothing


view model =
    div
        [ classNames
            [ "w-screen"
            , "p-5"
            , "flex-row"
            , "flex"
            , "text-center"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ Html.Keyed.node "div"
            [ classNames
                [ "container"
                , "max-w-md"
                ]
            ]
            ([ ( "header"
               , h1
                    [ classNames
                        [ "text-center"
                        , "py-2"
                        , "lg:py-8"
                        , "text-grey-dark"
                        , "text-sm"
                        , "lg:text-lg"
                        ]
                    ]
                    [ text "Wortkarten" ]
               )
             , ( "search"
               , div [ classNames [ "relative" ] ]
                    [ input
                        [ type_ "text"
                        , onInput SearchInput
                        , classNames
                            [ "border-b"
                            , "text-grey-darkest"
                            , "bg-transparent"
                            , "w-full"
                            , "text-lg"
                            , "py-2"
                            ]
                        , placeholder "Filter"
                        , value (model.searchText |> Maybe.withDefault "")
                        ]
                        []
                    , resultCountView model
                    ]
               )
             ]
                ++ (case ( model.expandSearchResults, model.searchText ) of
                        ( True, Just text ) ->
                            [ ( "searchResult", searchResultView model text ) ]

                        _ ->
                            []
                   )
                ++ [ ( "card", cardView model ) ]
            )
        , case model.appMode of
            ShowCard ->
                addButton

            AddWord entry ->
                editorView model entry
        ]


resultCountView model =
    let
        resultCount =
            model
                |> searchResults
                |> Array.length

        ( isFiltered, isClickable ) =
            model.searchText
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
        [ classNames
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
                [ classNames
                    (groupedBtnClasses isClickable
                        (not isClickable)
                        True
                        (not isFiltered)
                        ++ extraBtnClasses
                    )
                , onClick ToggleSearchResults
                ]
                [ text (prefix ++ (resultCount |> String.fromInt) ++ " Worten") ]
            ]
         ]
            ++ (if isFiltered then
                    [ li []
                        [ button
                            [ classNames
                                (groupedBtnClasses True
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


searchResults model =
    model.searchText
        |> Maybe.map
            (\searchText ->
                model.dict
                    |> Array.filter (isMatchedTo searchText)
            )
        |> Maybe.withDefault model.dict


searchResultView model searchText =
    ul [ classNames [ "list-reset", "py-3" ] ]
        (model
            |> searchResults
            |> Array.map (searchResultRow searchText)
            |> Array.toList
        )


isMatchedTo searchText (Entry de ja _) =
    let
        ( test, search ) =
            if String.startsWith "^" searchText then
                ( String.startsWith, String.dropLeft 1 searchText )

            else if String.endsWith "$" searchText then
                ( String.endsWith, String.dropRight 1 searchText )

            else
                ( String.contains, searchText )

        lowerSearchText =
            String.toLower search
    in
    test lowerSearchText de || test lowerSearchText ja


searchResultRow searchText entry =
    let
        (Entry de ja _) =
            entry
    in
    li
        [ classNames
            [ "p-3"
            , "text-left"
            , "rounded"
            , "cursor-pointer"
            , "hover:bg-grey-lighter"
            ]
        , onClick (ClickSearchResult entry)
        ]
        [ div [ classNames [ "inline-block", "mr-2" ] ] (hilighted searchText de)
        , div [ classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText ja)
        ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView model =
    let
        ( textToShow, maybeExample ) =
            model.showing
                |> Maybe.map
                    (\( showTranslation, Entry de ja ex ) ->
                        case ( showTranslation, model.direction ) of
                            ( False, DeToJa ) ->
                                ( de, Nothing )

                            ( False, JaToDe ) ->
                                ( ja, Nothing )

                            ( True, DeToJa ) ->
                                ( ja, ex )

                            ( True, JaToDe ) ->
                                ( de, ex )
                    )
                |> Maybe.withDefault ( "", Nothing )
    in
    div []
        [ ul
            [ classNames
                [ "py-4"
                , "list-reset"
                , "flex"
                ]
            ]
            [ li [ classNames [ "flex-1", "mr-2" ] ]
                [ button
                    [ onClick DirectionChange
                    , classNames (btnClasses (model.direction == DeToJa) False ++ [ "p-3", "w-full" ])
                    ]
                    [ text "De → Ja" ]
                ]
            , li [ classNames [ "flex-1" ] ]
                [ button
                    [ onClick DirectionChange
                    , classNames (btnClasses (model.direction == JaToDe) False ++ [ "p-3", "w-full" ])
                    ]
                    [ text "Ja → De" ]
                ]
            ]
        , div
            [ classNames
                [ "rounded"
                , "bg-white"
                , "shadow-lg"
                ]
            ]
            ([ div
                [ classNames
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
                                    [ classNames
                                        [ "absolute"
                                        , "inline-block"
                                        ]
                                    , style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                                    , style "left" (String.fromInt x)
                                    , style "top" (String.fromInt y)
                                    ]

                                Nothing ->
                                    [ classNames [ "inline-block text-transparent" ] ]
                           )
                    )
                    [ text textToShow ]
                , a
                    [ href ("https://translate.google.co.jp/m/translate?hl=ja#view=home&op=translate&sl=de&tl=ja&text=" ++ textToShow)
                    , target "_blank"
                    , classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-r"
                        , "m-2"
                        , "text-blue"
                        , "no-underline"
                        ]
                    ]
                    [ text "Hören" ]
                ]
             ]
                ++ (maybeExample
                        |> Maybe.map
                            (\example ->
                                [ p
                                    [ classNames
                                        [ "text-grey-dark"
                                        , "p-8"
                                        , "leading-normal"
                                        , "bg-grey-lighter"
                                        ]
                                    ]
                                    [ text (censorExample example) ]
                                ]
                            )
                        |> Maybe.withDefault []
                   )
            )
        , button
            [ classNames
                (btnClasses True False
                    ++ [ "my-5"
                       , "p-4"
                       , "text-lg"
                       , "w-full"
                       ]
                )
            , onClick NextRandomWord
            ]
            [ text "Nächst" ]
        ]


addButton =
    button
        [ classNames
            [ "fixed"
            , "pin-r"
            , "pin-b"
            , "rounded-full"
            , "bg-blue"
            , "text-white"
            , "text-xl"
            , "m-8"
            , "p-2"
            , "w-16"
            , "h-16"
            , "flex"
            , "justify-center"
            , "items-center"
            , "z-50"
            ]
        , onClick AddButtonClicked
        ]
        [ div [ style "line-height" "0" ] [ text "+" ] ]


editorView model (Entry de ja maybeExample) =
    div
        [ classNames
            [ "fixed"
            , "pin-l"
            , "pin-t"
            , "w-full"
            , "h-full"
            , "bg-white"
            , "p-4"
            ]
        ]
        [ textInputView "De"
            de
            False
            (\value -> WordChange (Entry value ja maybeExample))
        , textInputView "Ja"
            ja
            False
            (\value -> WordChange (Entry de value maybeExample))
        , textInputView "Example"
            (maybeExample |> Maybe.withDefault "")
            True
            (\value ->
                WordChange
                    (Entry de
                        ja
                        (if value == "" then
                            Nothing

                         else
                            Just value
                        )
                    )
            )
        , button
            [ onClick CloseEditor
            , classNames
                [ "absolute"
                , "pin-t"
                , "pin-r"
                , "text-lg"
                , "p-2"
                ]
            ]
            [ text "x" ]
        , button
            [ onClick SaveAndCloseEditor
            , classNames
                (btnClasses True False
                    ++ [ "w-full"
                       , "p-4"
                       , "text-lg"
                       ]
                )
            ]
            [ text "Save" ]
        ]


textInputView fieldName inputValue multiline handleInput =
    let
        formClasses =
            [ "bg-grey-lighter"
            , "rounded"
            , "p-4"
            , "text-grey-darkest"
            , "w-full"
            ]

        formView =
            if multiline then
                textarea
                    [ classNames
                        ([ "text-base"
                         , "leading-normal"
                         , "resize-none"
                         ]
                            ++ formClasses
                        )
                    , rows 3
                    , value inputValue
                    , onInput handleInput
                    ]
                    []

            else
                input
                    [ type_ "text"
                    , classNames ([ "text-lg" ] ++ formClasses)
                    , value inputValue
                    , onInput handleInput
                    ]
                    []
    in
    div [ classNames [ "mb-8", "w-full" ] ]
        [ label [ classNames [ "w-full" ] ]
            [ div
                [ classNames
                    [ "mr-2"
                    , "text-left"
                    , "text-sm"
                    , "my-2"
                    , "w-full"
                    , "text-grey-dark"
                    ]
                ]
                [ text fieldName ]
            , formView
            ]
        ]


censorExample text =
    let
        regex =
            Regex.fromString "\\[[^\\]]+\\]"
                |> Maybe.withDefault Regex.never

        replacer =
            \_ -> "(...)"
    in
    Regex.replace regex replacer text


btnClasses selected disabled =
    groupedBtnClasses selected disabled True True


groupedBtnClasses selected disabled isFirst isLast =
    [ ( "rounded-l", isFirst )
    , ( "rounded-r", isLast )
    , ( "bg-blue", selected )
    , ( "text-white", selected )
    , ( "text-grey", disabled )
    , ( "text-blue", not selected && not disabled )
    , ( "shadow", selected )
    , ( "cursor-default", disabled )
    ]
        |> List.filter (\( _, isIncluded ) -> isIncluded)
        |> List.map Tuple.first


classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList
