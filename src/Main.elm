port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Entry exposing (Entry)
import FilterCondition
import Help
import Html exposing (Html, a, button, div, h1, h3, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Editor
import PartOfSpeech exposing (PartOfSpeech(..))
import Ports
import Process
import Random
import Task
import Time exposing (Month(..), Zone, ZoneName(..))
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


port syncEntryDone : (() -> msg) -> Sub msg


port signInDone : (String -> msg) -> Sub msg


port dictionaryLoaded : (List Encode.Value -> msg) -> Sub msg


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


main : Program Int Model Msg
main =
    Browser.application
        { init =
            \startTimeMillis url key ->
                initialModel startTimeMillis url key startTimeMillis
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
        , view = \model -> { title = "Wortkarten", body = [ Html.Lazy.lazy view model ] }
        , onUrlRequest = NewUrlRequested
        , onUrlChange = RouteChanged
        }


type alias Model =
    { dict : Dictionary
    , seed : Random.Seed
    , route : Route
    , userId : Maybe String
    , notification : ( Bool, String )
    , key : Key
    , zone : Zone
    , zoneName : ZoneName
    , searchText : Maybe String
    , startTime : Time.Posix
    }


type alias HomeModel =
    { isTranslated : Bool
    , entry : Entry
    , textDisposition : Maybe ( Int, Int, Float )
    , expandSearchResults : Bool
    }


type Route
    = Initializing (Maybe Url)
    | ShowCard HomeModel
    | EditWord Pages.Editor.Model


routeParser : Dictionary -> Url.Parser.Parser (( Result () Route, Maybe String ) -> a) a
routeParser dict =
    let
        emptyEntry =
            Entry.empty
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (\filter -> ( Err (), filter ))
            (Url.Parser.s "entries"
                </> Url.Parser.s "_random"
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\filter -> ( Err (), filter ))
            (Url.Parser.top
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                ( Ok
                    (EditWord
                        { entry = { emptyEntry | de = Maybe.withDefault "" de }
                        , originalEntry = Nothing
                        }
                    )
                , filter
                )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.s "_new"
                <?> Url.Parser.Query.string "de"
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                let
                    homeModel =
                        initialHomeModel (entryOf dict de)
                in
                ( Ok (ShowCard homeModel), filter )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.string
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                entryOf dict de
                    |> (\entry ->
                            ( Ok
                                (EditWord
                                    { entry = entry
                                    , originalEntry = Just entry
                                    }
                                )
                            , filter
                            )
                       )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.string
                </> Url.Parser.s "_edit"
                <?> Url.Parser.Query.string "filter"
            )
        ]


entryOf : Dictionary -> String -> Entry
entryOf dict de =
    let
        emptyEntry =
            Entry.empty

        decoded =
            Url.percentDecode de |> Maybe.withDefault de
    in
    dict
        |> Array.filter (\e -> e.de == decoded)
        |> Array.toList
        |> List.head
        |> Maybe.withDefault { emptyEntry | de = decoded }


initialModel : Int -> Url -> Key -> Int -> ( Model, Cmd Msg )
initialModel startTimeMillis url key randomSeed =
    ( { dict = Array.empty
      , seed = Random.initialSeed randomSeed
      , route = Initializing (Just url)
      , userId = Nothing
      , notification = ( False, "" )
      , key = key
      , zone = Time.utc
      , zoneName = Offset 0
      , searchText = Nothing
      , startTime = startTimeMillis |> Time.millisToPosix
      }
    , Cmd.batch
        [ Time.here |> Task.attempt ZoneResolved
        , Time.getZoneName |> Task.attempt ZoneNameResolved
        ]
    )


initialHomeModel : Entry -> HomeModel
initialHomeModel entry =
    { isTranslated = False
    , entry = entry
    , textDisposition = Nothing
    , expandSearchResults = False
    }


type Msg
    = HomeMsg HomeMsg
    | EditorMsg Pages.Editor.Msg
    | ReceiveDict (Result Decode.Error (List Entry))
    | SignInDone String
    | SyncEntryDone ()
    | CloseNotification
    | RouteChanged Url
    | WithModel (Model -> ( Model, Cmd Msg ))
    | ZoneResolved (Result String Zone)
    | ZoneNameResolved (Result String ZoneName)
    | NewUrlRequested UrlRequest
    | NoOp


type HomeMsg
    = ClickSearchResult Entry
    | Translate
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ToggleSearchResults
    | ClearSearchText
    | ToggleStar


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.route, msg ) of
        ( ShowCard homeModel, HomeMsg homeMsg ) ->
            case homeMsg of
                ClickSearchResult entry ->
                    ( model
                    , navigateTo model (Just entry)
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
                    ( model
                    , Browser.Navigation.pushUrl model.key
                        ("/entries/" ++ homeModel.entry.de ++ "?filter=" ++ text)
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
                    ( { model | searchText = Nothing }
                    , Cmd.none
                    )

                ToggleStar ->
                    let
                        entry =
                            homeModel.entry

                        updatedEntry =
                            { entry | starred = not entry.starred }
                    in
                    ( { model
                        | dict = model.dict |> Array.map (Help.replaceEntry entry updatedEntry)
                        , route = ShowCard { homeModel | entry = updatedEntry }
                      }
                    , model.userId
                        |> Maybe.map (\userId -> Ports.saveEntry ( userId, Entry.encode updatedEntry ))
                        |> Maybe.withDefault Cmd.none
                    )

        ( EditWord pageModel, EditorMsg editorMsg ) ->
            updateWithCurrentTime model
                (\now theModel ->
                    model.userId
                        |> Maybe.map
                            (\userId ->
                                let
                                    ( updatedPageModel, updatedDict, cmd ) =
                                        Pages.Editor.update userId now model.key pageModel model.dict editorMsg (navigateTo model)
                                in
                                ( { theModel
                                    | dict = updatedDict
                                    , route = EditWord updatedPageModel
                                  }
                                , cmd |> Cmd.map EditorMsg
                                )
                            )
                        |> Maybe.withDefault ( theModel, Cmd.none )
                )

        ( _, ReceiveDict (Ok dict) ) ->
            let
                newDict =
                    Array.fromList dict

                modelWithNewDict =
                    { model | dict = newDict }
            in
            case model.route of
                ShowCard { entry } ->
                    entry.de
                        |> entryOf newDict
                        |> initialHomeModel
                        |> (\homeModel -> ( { modelWithNewDict | route = ShowCard homeModel }, Cmd.none ))

                EditWord pageModel ->
                    pageModel.entry.de
                        |> entryOf newDict
                        |> (\e ->
                                ( { modelWithNewDict
                                    | route =
                                        EditWord
                                            { pageModel
                                                | entry = e
                                                , originalEntry = pageModel.originalEntry |> Maybe.map (\_ -> e)
                                            }
                                  }
                                , Cmd.none
                                )
                           )

                Initializing url ->
                    ( modelWithNewDict
                    , Browser.Navigation.pushUrl model.key
                        (url
                            |> Maybe.map Url.toString
                            |> Maybe.withDefault "/entries/_random"
                        )
                    )

        ( _, ReceiveDict (Err _) ) ->
            ( { model | notification = ( True, "Failed to load the dictionary." ) }, Cmd.none )

        ( _, SyncEntryDone _ ) ->
            ( { model | notification = ( True, "Changes were synchronized." ) }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
            )

        ( _, CloseNotification ) ->
            ( { model | notification = ( False, model.notification |> Tuple.second ) }, Cmd.none )

        ( _, SignInDone uid ) ->
            ( { model | userId = Just uid }, Cmd.none )

        ( _, RouteChanged url ) ->
            case Url.Parser.parse (routeParser model.dict) url of
                Just ( Ok r, filter ) ->
                    ( { model | route = r, searchText = filter }
                    , case r of
                        EditWord _ ->
                            Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)

                        _ ->
                            Cmd.none
                    )

                Just ( Err (), filter ) ->
                    let
                        ( maybeEntry, updatedSeed ) =
                            randomEntry
                                model.seed
                                (searchResults model.startTime
                                    (case model.route of
                                        ShowCard { entry } ->
                                            model.dict |> Dictionary.without entry

                                        _ ->
                                            model.dict
                                    )
                                    filter
                                )
                    in
                    ( { model
                        | route = Initializing (Just url)
                        , searchText = filter
                        , seed = updatedSeed
                      }
                    , Browser.Navigation.replaceUrl model.key
                        ("/entries/"
                            ++ (maybeEntry
                                    |> Maybe.map (\e -> e.de)
                                    |> Maybe.withDefault "_new"
                               )
                            ++ (filter
                                    |> Maybe.map (\st -> "?filter=" ++ st)
                                    |> Maybe.withDefault ""
                               )
                        )
                    )

                Nothing ->
                    ( { model | route = Initializing Nothing, searchText = Nothing }
                    , Cmd.none
                    )

        ( _, ZoneResolved (Ok zone) ) ->
            ( { model | zone = zone }, Cmd.none )

        ( _, ZoneResolved (Err errorMessage) ) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

        ( _, ZoneNameResolved (Ok zoneName) ) ->
            ( { model | zoneName = zoneName }, Cmd.none )

        ( _, ZoneNameResolved (Err errorMessage) ) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

        ( _, WithModel withModel ) ->
            withModel model

        ( _, NewUrlRequested urlRequest ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        ( _, _ ) ->
            ( model, Cmd.none )


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
        [ Help.classNames
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
            Initializing _ ->
                text "Initializing..."

            ShowCard entry ->
                Html.Lazy.lazy4
                    homeView
                    model.startTime
                    model.searchText
                    (searchResults model.startTime model.dict model.searchText)
                    entry

            EditWord pageModel ->
                Html.Lazy.lazy3
                    Pages.Editor.view
                    model.zone
                    model.zoneName
                    pageModel
                    |> Html.map EditorMsg
        , notificationView model.notification
        ]


notificationView ( isShown, message ) =
    div
        [ Help.classNames
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
                "-6em"
            )
        ]
        [ text message
        , button
            [ onClick CloseNotification
            , Help.classNames
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


homeView : Time.Posix -> Maybe String -> Dictionary -> HomeModel -> Html Msg
homeView startTime searchText results homeModel =
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
                    , onInput (SearchInput >> HomeMsg)
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
                , resultCountView startTime searchText results homeModel |> Html.map HomeMsg
                ]
           )
         ]
            ++ (case ( homeModel.expandSearchResults || Array.length results == 0, searchText ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView results text |> Html.map HomeMsg ) ]

                    _ ->
                        []
               )
            ++ [ ( "card", cardView searchText homeModel results homeModel.entry |> Html.map HomeMsg ) ]
            ++ [ ( "addButton", addButton searchText ) ]
        )


resultCountView : Time.Posix -> Maybe String -> Dictionary -> HomeModel -> Html HomeMsg
resultCountView startTime searchText results homeModel =
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


searchResults : Time.Posix -> Dictionary -> Maybe String -> Array Entry
searchResults now dict maybeSearchText =
    maybeSearchText
        |> Maybe.map
            (\searchText ->
                dict
                    |> Array.filter (FilterCondition.isMatchedTo now searchText)
            )
        |> Maybe.withDefault dict


searchResultView : Dictionary -> String -> Html HomeMsg
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
        [ Help.classNames
            [ "p-3"
            , "text-left"
            , "rounded"
            , "cursor-pointer"
            , "hover:bg-grey-lighter"
            ]
        , onClick (ClickSearchResult entry)
        ]
        [ div [ Help.classNames [ "inline-block", "mr-2" ] ] (hilighted searchText entry.de)
        , div [ Help.classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText entry.ja)
        ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView : Maybe String -> HomeModel -> Dictionary -> Entry -> Html HomeMsg
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


navigateTo model maybeEntry =
    Browser.Navigation.pushUrl model.key
        ("/"
            ++ (maybeEntry |> Maybe.map (\{ de } -> "entries/" ++ de ++ "/") |> Maybe.withDefault "")
            ++ (model.searchText
                    |> Maybe.map (\st -> "?filter=" ++ st)
                    |> Maybe.withDefault ""
               )
        )


updateWithCurrentTime :
    Model
    -> (Time.Posix -> Model -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
updateWithCurrentTime model theUpdate =
    ( model
    , Time.now
        |> Task.attempt
            (Result.map
                (\now ->
                    WithModel (theUpdate now)
                )
                >> Result.withDefault NoOp
            )
    )
