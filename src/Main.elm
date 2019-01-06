port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Entry exposing (Entry(..))
import FilterCondition
import Html exposing (Html, a, button, div, h1, h3, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import PartOfSpeech exposing (PartOfSpeech(..))
import Process
import Random
import Task
import Url exposing (Protocol(..), Url)


port saveEntry : ( String, Encode.Value ) -> Cmd msg


port deleteEntry : ( String, String ) -> Cmd msg


port syncEntryDone : (() -> msg) -> Sub msg


port signInDone : (String -> msg) -> Sub msg


port dictionaryLoaded : (List Encode.Value -> msg) -> Sub msg


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


main : Program Int Model Msg
main =
    Browser.application
        { init =
            \randomSeed url key ->
                initialModel url key randomSeed
        , subscriptions =
            \_ ->
                Sub.batch
                    [ textDisposition (TextDispositionChange >> HomeMsg)
                    , signInDone SignInDone
                    , syncEntryDone SyncEntryDone
                    , dictionaryLoaded
                        (List.map (Decode.decodeValue Entry.decode)
                            >> reduceError
                            >> ReceiveDict
                        )
                    ]
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        , onUrlRequest = RouteChanged
        , onUrlChange = \_ -> NoOp
        }


type alias Model =
    { dict : Dictionary
    , seed : Random.Seed
    , route : Route
    , userId : Maybe String
    , notification : ( Bool, String )
    , key : Key
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


route : Url -> ( Route, Cmd Msg )
route { path } =
    case String.split "/" path |> List.drop 1 of
        [ "entries", "_new" ] ->
            ( AddWord (Entry "" Verb "" Nothing), Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp) )

        _ ->
            ( ShowCard (initialHomeModel Nothing), Cmd.none )


initialModel : Url -> Key -> Int -> ( Model, Cmd Msg )
initialModel url key randomSeed =
    let
        ( theRoute, cmd ) =
            route url
    in
    ( { dict = Array.empty
      , seed = Random.initialSeed randomSeed
      , route = theRoute
      , userId = Nothing
      , notification = ( False, "" )
      , key = key
      }
    , cmd
    )


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
    | ReceiveDict (Result Decode.Error (List Entry))
    | SignInDone String
    | SyncEntryDone ()
    | CloseNotification
    | ClickedLink UrlRequest
    | RouteChanged UrlRequest
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
                                            , isTranslated = False
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
                    ( updateDict
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
                    , model.userId
                        |> Maybe.map (\userId -> saveEntry ( userId, Entry.encode entry ))
                        |> Maybe.withDefault Cmd.none
                    )

                ( AddWord entry, SaveAndCloseEditor ) ->
                    ( updateDict
                        (model.dict |> Array.append (Array.fromList [ entry ]))
                        model
                    , model.userId
                        |> Maybe.map (\userId -> saveEntry ( userId, Entry.encode entry ))
                        |> Maybe.withDefault Cmd.none
                    )

                ( EditWord _ ((Entry de _ _ _) as entry), DeleteEntry ) ->
                    ( updateDict (model.dict |> Array.filter ((/=) entry)) model
                    , model.userId
                        |> Maybe.map (\userId -> deleteEntry ( userId, de ))
                        |> Maybe.withDefault Cmd.none
                    )

                ( EditWord _ originalEntry, WordChange entry ) ->
                    ( { model | route = EditWord entry originalEntry }
                    , Cmd.none
                    )

                ( AddWord _, WordChange entry ) ->
                    ( { model | route = AddWord entry }, Cmd.none )

                ( AddWord _, DeleteEntry ) ->
                    ( model, Cmd.none )

                ( ShowCard _, _ ) ->
                    ( model, Cmd.none )

        ReceiveDict (Ok dict) ->
            let
                updatedModel =
                    let
                        newDict =
                            Array.fromList dict

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
            ( updatedModel, Cmd.none )

        ReceiveDict (Err _) ->
            ( { model | notification = ( True, "Failed to load the dictionary." ) }, Cmd.none )

        SyncEntryDone _ ->
            ( { model | notification = ( True, "Changes were synchronized." ) }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
            )

        CloseNotification ->
            ( { model | notification = ( False, model.notification |> Tuple.second ) }, Cmd.none )

        SignInDone uid ->
            ( { model | userId = Just uid }, Cmd.none )

        ClickedLink urlRequest ->
            update (RouteChanged urlRequest) model

        RouteChanged urlRequest ->
            case urlRequest of
                Internal url ->
                    let
                        ( theRoute, cmd ) =
                            route url
                    in
                    ( { model | route = theRoute }
                    , Cmd.batch [ cmd, Browser.Navigation.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        NoOp ->
            ( model, Cmd.none )


updateDict : Dictionary -> Model -> Model
updateDict dict model =
    Maybe.map
        (\entry ->
            { model
                | route = ShowCard (initialHomeModel (Just entry))
                , dict = dict
            }
        )
        (case model.route of
            AddWord entry ->
                Just entry

            EditWord entry _ ->
                Just entry

            ShowCard _ ->
                Nothing
        )
        |> Maybe.withDefault model


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
                homeView model.dict entry

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


homeView : Dictionary -> HomeModel -> Html Msg
homeView dict homeModel =
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
                    , onInput (SearchInput >> HomeMsg)
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
                , resultCountView dict homeModel |> Html.map HomeMsg
                ]
           )
         ]
            ++ (case ( homeModel.expandSearchResults, homeModel.searchText ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView dict text |> Html.map HomeMsg ) ]

                    _ ->
                        []
               )
            ++ (homeModel.entry
                    |> Maybe.map (cardView homeModel >> Html.map HomeMsg)
                    |> Maybe.map (\v -> [ ( "card", v ) ])
                    |> Maybe.withDefault []
               )
            ++ [ ( "addButton", addButton ) ]
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
                    |> Array.filter (FilterCondition.isMatchedTo searchText)
            )
        |> Maybe.withDefault dict


searchResultView : Dictionary -> String -> Html HomeMsg
searchResultView dict searchText =
    ul [ classNames [ "list-reset", "py-3" ] ]
        (searchResults dict (Just searchText)
            |> Array.toList
            |> List.sortBy Entry.toComparable
            |> List.map (searchResultRow searchText)
        )


searchResultRow searchText entry =
    let
        (Entry de _ ja _) =
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
cardView model ((Entry de _ ja ex) as entry) =
    let
        textToShow =
            case ( model.direction, model.isTranslated ) of
                ( DeToJa, False ) ->
                    de

                ( JaToDe, False ) ->
                    ja

                ( DeToJa, True ) ->
                    ja

                ( JaToDe, True ) ->
                    de
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
                        [ href ("https://de.wiktionary.org/wiki/" ++ de)
                        , target "_blank"
                        , classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Untersuchen" ]
                    , a
                        [ href ("https://translate.google.co.jp/m/translate?hl=ja#view=home&op=translate&sl=de&tl=ja&text=" ++ de)
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
                ++ (if model.isTranslated then
                        [ entryDetailView entry ]

                    else
                        []
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


entryDetailView (Entry de pos ja maybeExample) =
    div
        [ classNames
            [ "text-grey-dark"
            , "px-8"
            , "py-4"
            , "leading-normal"
            , "bg-grey-lighter"
            , "text-left"
            ]
        ]
        ([ section [ classNames [ "mb-2" ] ]
            [ h3 [] [ text "Teil" ]
            , p [] [ text (PartOfSpeech.toString pos) ]
            ]
         ]
            ++ (maybeExample
                    |> Maybe.map
                        (\example ->
                            [ section [ classNames [ "mb-2" ] ]
                                [ h3 [] [ text "Beispiel" ]
                                , p [] [ text (Entry.censorExample example) ]
                                ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


addButton =
    a
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
        , href "/entries/_new"
        ]
        [ div [ style "margin-top" "-8px" ] [ text "+" ] ]


editorView model isNew ((Entry de pos ja maybeExample) as entry) =
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
            [ inputRowView "Deutsch"
                (textInputView (Just "editor-input-de")
                    de
                    False
                    (\value -> WordChange (Entry value pos ja maybeExample))
                )
            , inputRowView "Teil"
                (selectInputView
                    pos
                    (\value ->
                        WordChange
                            (Entry de
                                (value |> PartOfSpeech.fromString |> Result.withDefault pos)
                                ja
                                maybeExample
                            )
                    )
                    (PartOfSpeech.items
                        |> List.map PartOfSpeech.toString
                        |> List.map (\v -> ( v, v ))
                    )
                )
            , inputRowView "Japanisch"
                (textInputView Nothing
                    ja
                    False
                    (\value -> WordChange (Entry de pos value maybeExample))
                )
            , inputRowView "Beispiel"
                (textInputView Nothing
                    (maybeExample |> Maybe.withDefault "")
                    True
                    (\value ->
                        WordChange
                            (Entry de
                                pos
                                ja
                                (if value == "" then
                                    Nothing

                                 else
                                    Just value
                                )
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


isValid (Entry de _ ja maybeExample) =
    (de /= "") && (ja /= "")


inputRowView : String -> (List String -> Html msg) -> Html msg
inputRowView fieldName inputView =
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
            , inputView
                [ "bg-grey-lighter"
                , "rounded"
                , "p-3"
                , "text-grey-darkest"
                , "w-full"
                ]
            ]
        ]


textInputView maybeInputId inputValue multiline handleInput formClasses =
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


selectInputView inputValue handleInput options formClasses =
    select
        [ classNames ([ "text-base" ] ++ formClasses)
        , style "-webkit-appearance" "none"
        , onInput handleInput
        ]
        (options |> List.map (\( v, label ) -> option [ value v ] [ text label ]))


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


reduceError : List (Result a b) -> Result a (List b)
reduceError results =
    results
        |> List.foldl
            (\result passed ->
                case ( passed, result ) of
                    ( Err e, _ ) ->
                        Err e

                    ( _, Err e ) ->
                        Err e

                    ( Ok r, Ok rr ) ->
                        Ok (r ++ [ rr ])
            )
            (Ok [])
