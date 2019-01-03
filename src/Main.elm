port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Dictionary exposing (Dictionary)
import Entry exposing (Entry(..))
import Html exposing (Html, a, button, div, h1, input, label, li, p, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Http
import Process
import Random
import Regex
import Task


port persistDictionary : ( String, String ) -> Cmd msg


port persistDictionaryDone : (() -> msg) -> Sub msg


port signInDone : (String -> msg) -> Sub msg


port getDictUrl : String -> Cmd msg


port getDictUrlDone : (String -> msg) -> Sub msg


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


port syncLocalStorage : String -> Cmd msg


main : Program ( Bool, Int, String ) Model Msg
main =
    Browser.document
        { init = \( onLine, randomSeed, localDict ) -> ( initialModel onLine randomSeed localDict, Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ textDisposition (TextDispositionChange >> HomeMsg)
                    , signInDone SignInDone
                    , persistDictionaryDone PersistDictionaryDone
                    , getDictUrlDone GetDictUrlDone
                    ]
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        }


type alias Model =
    { dict : Dictionary
    , seed : Random.Seed
    , route : Route
    , userId : Maybe String
    , onLine : Bool
    , notification : ( Bool, String )
    }


type alias HomeModel =
    { isTranslated : Bool
    , entry : Maybe Entry
    , direction : Direction
    , textDisposition : Maybe ( Int, Int, Float )
    , searchText : Maybe String
    , expandSearchResults : Bool
    }


type Direction
    = DeToJa
    | JaToDe


type Route
    = ShowCard HomeModel
    | AddWord Entry
    | EditWord Entry Entry


initialModel : Bool -> Int -> String -> Model
initialModel onLine randomSeed localDict =
    let
        dict =
            Dictionary.parse localDict

        ( maybeEntry, nextSeed ) =
            randomEntry (Random.initialSeed randomSeed) dict
    in
    { dict = dict
    , seed = nextSeed
    , route = ShowCard (initialHomeModel maybeEntry)
    , userId = Nothing
    , onLine = onLine
    , notification =
        if onLine then
            ( False, "" )

        else
            ( True, "Die App befindet sich im Offline-Modus." )
    }


initialHomeModel maybeEntry =
    { isTranslated = False
    , direction = DeToJa
    , entry = maybeEntry
    , textDisposition = Nothing
    , searchText = Nothing
    , expandSearchResults = False
    }


type Msg
    = HomeMsg HomeMsg
    | EditorMsg EditorMsg
    | GetDictUrlDone String
    | ReceiveDict (Result Http.Error String)
    | SignInDone String
    | PersistDictionaryDone ()
    | CloseNotification
    | NoOp


type HomeMsg
    = NextRandomWord
    | ClickSearchResult Entry
    | Translate
    | DirectionChange
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | AddButtonClicked
    | StartEdit Entry


type EditorMsg
    = CloseEditor
    | SaveAndCloseEditor
    | WordChange Entry
    | DeleteEntry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeMsg homeMsg ->
            case model.route of
                ShowCard homeModel ->
                    case homeMsg of
                        NextRandomWord ->
                            let
                                ( maybeEntry, nextSeed ) =
                                    randomEntry model.seed
                                        (searchResults model.dict homeModel.searchText)
                            in
                            ( { model
                                | seed = nextSeed
                                , route =
                                    ShowCard
                                        { homeModel
                                            | textDisposition = Nothing
                                            , entry = maybeEntry
                                        }
                              }
                            , Cmd.none
                            )

                        ClickSearchResult entry ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | expandSearchResults = False
                                            , textDisposition = Nothing
                                            , isTranslated = False
                                            , entry = Just entry
                                        }
                              }
                            , Cmd.none
                            )

                        Translate ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | isTranslated = not homeModel.isTranslated
                                            , textDisposition = Nothing
                                        }
                              }
                            , Cmd.none
                            )

                        DirectionChange ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | direction =
                                                case homeModel.direction of
                                                    DeToJa ->
                                                        JaToDe

                                                    JaToDe ->
                                                        DeToJa
                                            , textDisposition = Nothing
                                        }
                              }
                            , Cmd.none
                            )

                        TextDispositionChange value ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | textDisposition = Just value
                                        }
                              }
                            , Cmd.none
                            )

                        SearchInput text ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | searchText =
                                                if text == "" then
                                                    Nothing

                                                else
                                                    Just text
                                        }
                              }
                            , Cmd.none
                            )

                        ToggleSearchResults ->
                            ( { model
                                | route =
                                    ShowCard
                                        { homeModel
                                            | expandSearchResults =
                                                not homeModel.expandSearchResults
                                        }
                              }
                            , Cmd.none
                            )

                        ClearSearchText ->
                            ( { model
                                | route = ShowCard { homeModel | searchText = Nothing }
                              }
                            , Cmd.none
                            )

                        AddButtonClicked ->
                            ( { model | route = AddWord (Entry "" "" Nothing) }
                            , Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)
                            )

                        StartEdit entry ->
                            ( { model | route = EditWord entry entry }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditorMsg editorMsg ->
            case ( model.route, editorMsg ) of
                ( AddWord _, CloseEditor ) ->
                    let
                        ( maybeEntry, updatedSeed ) =
                            randomEntry model.seed (searchResults model.dict Nothing)
                    in
                    ( { model
                        | route = ShowCard (initialHomeModel maybeEntry)
                        , seed = updatedSeed
                      }
                    , Cmd.none
                    )

                ( EditWord _ originalEntry, CloseEditor ) ->
                    ( { model | route = ShowCard (initialHomeModel (Just originalEntry)) }
                    , Cmd.none
                    )

                ( EditWord entry originalEntry, SaveAndCloseEditor ) ->
                    updateDict
                        (Array.map
                            (\e ->
                                if e == originalEntry then
                                    entry

                                else
                                    e
                            )
                            model.dict
                        )
                        model

                ( AddWord entry, SaveAndCloseEditor ) ->
                    updateDict
                        (model.dict |> Array.append (Array.fromList [ entry ]))
                        model

                ( EditWord _ entry, DeleteEntry ) ->
                    updateDict (model.dict |> Array.filter ((/=) entry)) model

                ( EditWord _ originalEntry, WordChange entry ) ->
                    ( { model | route = EditWord entry originalEntry }, Cmd.none )

                ( AddWord _, WordChange entry ) ->
                    ( { model | route = AddWord entry }, Cmd.none )

                ( AddWord _, DeleteEntry ) ->
                    ( model, Cmd.none )

                ( ShowCard _, _ ) ->
                    ( model, Cmd.none )

        ReceiveDict (Ok str) ->
            let
                updatedModel =
                    let
                        newDict =
                            Dictionary.parse str

                        modelWithNewDict =
                            { model | dict = newDict }
                    in
                    case model.route of
                        ShowCard homeModel ->
                            case homeModel.entry of
                                Just _ ->
                                    modelWithNewDict

                                Nothing ->
                                    let
                                        ( maybeEntry, updatedSeed ) =
                                            randomEntry model.seed
                                                (searchResults newDict homeModel.searchText)
                                    in
                                    { modelWithNewDict
                                        | seed = updatedSeed
                                        , route =
                                            ShowCard
                                                { homeModel
                                                    | entry = maybeEntry
                                                    , isTranslated = False
                                                    , textDisposition = Nothing
                                                }
                                    }

                        _ ->
                            modelWithNewDict
            in
            ( updatedModel, syncLocalStorage str )

        ReceiveDict (Err _) ->
            ( { model | notification = ( True, "Failed to load the remote dictionary." ) }, Cmd.none )

        PersistDictionaryDone _ ->
            ( { model | notification = ( True, "Changes were synchronized." ) }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
            )

        CloseNotification ->
            ( { model | notification = ( False, model.notification |> Tuple.second ) }, Cmd.none )

        SignInDone uid ->
            ( { model | userId = Just uid }, getDictUrl uid )

        GetDictUrlDone url ->
            ( model, Http.get { url = url, expect = Http.expectString ReceiveDict } )

        NoOp ->
            ( model, Cmd.none )


updateDict : Dictionary -> Model -> ( Model, Cmd Msg )
updateDict dict model =
    Maybe.map2
        (\entry userId ->
            let
                serializedDict =
                    Dictionary.serialize dict
            in
            ( { model
                | route = ShowCard (initialHomeModel (Just entry))
                , dict = dict
              }
            , Cmd.batch
                [ persistDictionary ( serializedDict, userId )
                , syncLocalStorage serializedDict
                ]
            )
        )
        (case model.route of
            AddWord entry ->
                Just entry

            EditWord entry _ ->
                Just entry

            ShowCard _ ->
                Nothing
        )
        model.userId
        |> Maybe.withDefault ( model, Cmd.none )


randomEntry seed entries =
    let
        ( index, nextSeed ) =
            Random.step
                (Random.int 0 (Array.length entries - 1))
                seed
    in
    ( entries |> Array.get index
    , nextSeed
    )


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
        [ case model.route of
            ShowCard entry ->
                homeView model.onLine model.dict entry |> Html.map HomeMsg

            AddWord entry ->
                editorView model True entry |> Html.map EditorMsg

            EditWord entry originalEntry ->
                editorView model False entry |> Html.map EditorMsg
        , notificationView model.notification
        ]


notificationView ( isShown, message ) =
    div
        [ classNames
            [ "fixed"
            , "z-50"
            , "pin-b"
            , "pin-l"
            , "p-3"
            , "bg-black"
            , "text-white"
            , "text-xs"
            , "w-full"
            , "leading-loose"
            ]
        , style "transition" "margin-bottom .3s ease"
        , style "margin-bottom"
            (if isShown then
                "0"

             else
                "-4em"
            )
        ]
        [ text message
        , button
            [ onClick CloseNotification
            , classNames
                [ "rounded"
                , "bg-grey-darker"
                , "text-grey-lighter"
                , "text-xs"
                , "px-2"
                , "py-1"
                , "mx-2"
                ]
            ]
            [ text "Entlassen" ]
        ]


homeView : Bool -> Dictionary -> HomeModel -> Html HomeMsg
homeView onLine dict homeModel =
    Html.Keyed.node "div"
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
                    , value (homeModel.searchText |> Maybe.withDefault "")
                    ]
                    []
                , resultCountView dict homeModel
                ]
           )
         ]
            ++ (case ( homeModel.expandSearchResults, homeModel.searchText ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView dict text ) ]

                    _ ->
                        []
               )
            ++ (homeModel.entry
                    |> Maybe.map (cardView homeModel)
                    |> Maybe.map (\v -> [ ( "card", v ) ])
                    |> Maybe.withDefault []
               )
            ++ (if onLine then
                    [ ( "addButton", addButton ) ]

                else
                    []
               )
        )


resultCountView : Dictionary -> HomeModel -> Html HomeMsg
resultCountView dict homeModel =
    let
        resultCount =
            searchResults dict homeModel.searchText |> Array.length

        ( isFiltered, isClickable ) =
            homeModel.searchText
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


searchResults : Dictionary -> Maybe String -> Array Entry
searchResults dict maybeSearchText =
    maybeSearchText
        |> Maybe.map
            (\searchText ->
                dict
                    |> Array.filter (isMatchedTo searchText)
            )
        |> Maybe.withDefault dict


searchResultView : Dictionary -> String -> Html HomeMsg
searchResultView dict searchText =
    ul [ classNames [ "list-reset", "py-3" ] ]
        (searchResults dict (Just searchText)
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


cardView : HomeModel -> Entry -> Html HomeMsg
cardView model ((Entry de ja ex) as entry) =
    let
        ( textToShow, maybeExample ) =
            case ( model.direction, model.isTranslated ) of
                ( DeToJa, False ) ->
                    ( de, Nothing )

                ( JaToDe, False ) ->
                    ( ja, Nothing )

                ( DeToJa, True ) ->
                    ( ja, ex )

                ( JaToDe, True ) ->
                    ( de, ex )
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
                , div
                    [ classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-r"
                        , "m-2"
                        ]
                    ]
                    [ a
                        [ href ("https://translate.google.co.jp/m/translate?hl=ja#view=home&op=translate&sl=de&tl=ja&text=" ++ textToShow)
                        , target "_blank"
                        , classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Hören" ]
                    , button
                        [ onClick (StartEdit entry)
                        , classNames [ "text-blue" ]
                        ]
                        [ text "Edit" ]
                    ]
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
            , "m-4"
            , "p-2"
            , "w-16"
            , "h-16"
            , "flex"
            , "justify-center"
            , "items-center"
            , "z-30"
            , "shadow-lg"
            ]
        , onClick AddButtonClicked
        ]
        [ div [ style "margin-top" "-8px" ] [ text "+" ] ]


editorView model isNew ((Entry de ja maybeExample) as entry) =
    let
        hasError =
            not (isValid entry)
    in
    div
        [ classNames
            [ "w-full"
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
            [ textInputView "Deutsch"
                (Just "editor-input-de")
                de
                False
                (\value -> WordChange (Entry value ja maybeExample))
            , textInputView "Japanisch"
                Nothing
                ja
                False
                (\value -> WordChange (Entry de value maybeExample))
            , textInputView "Beispiel"
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
                [ onClick SaveAndCloseEditor
                , classNames
                    (btnClasses True hasError
                        ++ [ "w-full"
                           , "p-3"
                           , "text-base"
                           , "mb-2"
                           ]
                    )
                , disabled hasError
                ]
                [ text "Sparen" ]
            , button
                [ onClick DeleteEntry
                , style "display"
                    (if isNew then
                        "none"

                     else
                        "inline"
                    )
                , classNames
                    ((btnClasses True False |> List.filter (\c -> c /= "bg-blue"))
                        ++ [ "w-full"
                           , "p-3"
                           , "text-base"
                           , "bg-red"
                           ]
                    )
                , disabled hasError
                ]
                [ text "Löschen" ]
            , button
                [ onClick CloseEditor
                , classNames
                    ((btnClasses True False |> List.filter (\c -> c /= "bg-blue" && c /= "text-white"))
                        ++ [ "w-full"
                           , "p-3"
                           , "text-base"
                           , "mb-2"
                           , "bg-grey-lighter"
                           , "text-grey-darker"
                           ]
                    )
                ]
                [ text "Abbrechen" ]
            ]
        ]


isValid (Entry de ja maybeExample) =
    (de /= "") && (ja /= "")


textInputView fieldName maybeInputId inputValue multiline handleInput =
    let
        formClasses =
            [ "bg-grey-lighter"
            , "rounded"
            , "p-3"
            , "text-grey-darkest"
            , "w-full"
            ]

        formView =
            if multiline then
                textarea
                    [ classNames
                        ([ "text-sm"
                         , "leading-normal"
                         , "resize-none"
                         ]
                            ++ formClasses
                        )
                    , rows 5
                    , value inputValue
                    , onInput handleInput
                    ]
                    []

            else
                input
                    ([ type_ "text"
                     , classNames ([ "text-base" ] ++ formClasses)
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
    div [ classNames [ "mb-6", "w-full" ] ]
        [ label [ classNames [ "w-full" ] ]
            [ div
                [ classNames
                    [ "mr-2"
                    , "text-left"
                    , "text-xs"
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
