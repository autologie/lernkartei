port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Dictionary exposing (Dictionary)
import Entry exposing (Entry(..))
import Html exposing (a, button, div, h1, input, label, li, p, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Http
import Random
import Regex
import Task


port persistDictionary : ( String, String ) -> Cmd msg


port persistDictionaryDone : (() -> msg) -> Sub msg


port signIn : () -> Cmd msg


port signInDone : (String -> msg) -> Sub msg


port fetchDict : () -> Cmd msg


port getDictUrl : String -> Cmd msg


port getDictUrlDone : (String -> msg) -> Sub msg


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


main : Program Int Model Msg
main =
    Browser.document
        { init =
            \randomSeed ->
                ( initialModel randomSeed
                , signIn ()
                )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ textDisposition TextDispositionChange
                    , signInDone SignInDone
                    , persistDictionaryDone PersistDictionaryDone
                    , getDictUrlDone GetDictUrlDone
                    ]
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        }


type Direction
    = DeToJa
    | JaToDe


type alias Model =
    { dict : Dictionary
    , showing : Maybe ( Bool, Entry )
    , seed : Random.Seed
    , direction : Direction
    , textDisposition : Maybe ( Int, Int, Float )
    , searchText : Maybe String
    , expandSearchResults : Bool
    , appMode : AppMode
    , userId : Maybe String
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
    , userId = Nothing
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
    | SignInDone String
    | PersistDictionaryDone ()
    | GetDictUrlDone String
    | NoOp


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
            update NextRandomWord { model | dict = Dictionary.parse str }

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
            ( { model | appMode = AddWord (Entry "" "" Nothing) }
            , Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)
            )

        CloseEditor ->
            ( { model | appMode = ShowCard }, Cmd.none )

        SaveAndCloseEditor ->
            case ( model.appMode, model.userId ) of
                ( AddWord entry, Just userId ) ->
                    let
                        updatedDict =
                            Array.append (Array.fromList [ entry ]) model.dict
                    in
                    ( { model
                        | appMode = ShowCard
                        , dict = updatedDict
                      }
                    , persistDictionary ( Dictionary.serialize updatedDict, userId )
                    )

                _ ->
                    ( model, Cmd.none )

        PersistDictionaryDone _ ->
            ( model, Cmd.none )

        WordChange updatedEntry ->
            ( { model | appMode = AddWord updatedEntry }, Cmd.none )

        SignInDone uid ->
            ( { model | userId = Just uid }, getDictUrl uid )

        GetDictUrlDone url ->
            ( model, Http.get { url = url, expect = Http.expectString ReceiveDict } )

        NoOp ->
            ( model, Cmd.none )


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
            , "pin-l"
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
            , "z-50"
            , "shadow-lg"
            ]
        , onClick AddButtonClicked
        ]
        [ div [ style "margin-top" "-8px" ] [ text "+" ] ]


editorView model ((Entry de ja maybeExample) as entry) =
    let
        hasError =
            not (isValid model.dict entry)
    in
    div
        [ classNames
            [ "fixed"
            , "pin-l"
            , "pin-t"
            , "w-full"
            , "h-full"
            , "bg-white"
            , "p-4"
            , "flex"
            , "justify-center"
            , "items-start"
            ]
        ]
        [ div
            [ classNames
                [ "container"
                , "max-w-md"
                ]
            ]
            [ textInputView "De"
                (Just "editor-input-de")
                de
                False
                (\value -> WordChange (Entry value ja maybeExample))
            , textInputView "Ja"
                Nothing
                ja
                False
                (\value -> WordChange (Entry de value maybeExample))
            , textInputView "Example"
                Nothing
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
                    , "text-grey-darkest"
                    ]
                ]
                [ text "X" ]
            , button
                [ onClick SaveAndCloseEditor
                , classNames
                    (btnClasses True hasError
                        ++ [ "w-full"
                           , "p-4"
                           , "text-lg"
                           ]
                    )
                , disabled hasError
                ]
                [ text "Save" ]
            ]
        ]


isValid dict (Entry de ja maybeExample) =
    (de /= "")
        && (ja /= "")
        && (dict |> Array.filter (\(Entry dictDe _ _) -> dictDe == de) |> Array.isEmpty)


textInputView fieldName maybeInputId inputValue multiline handleInput =
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
                    ([ type_ "text"
                     , classNames ([ "text-lg" ] ++ formClasses)
                     , value inputValue
                     , onInput handleInput
                     ]
                        ++ (maybeInputId
                                |> Maybe.map (\value -> [ id value ])
                                |> Maybe.withDefault []
                           )
                    )
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
    , ( "bg-blue", selected && not disabled )
    , ( "text-white", selected && not disabled )
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
