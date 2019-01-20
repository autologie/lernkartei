port module Main exposing (main)

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Entry exposing (Entry)
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
import Time exposing (Month(..), Zone, ZoneName(..))
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


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
        , onUrlRequest = Debug.log "url request" >> NewUrlRequested
        , onUrlChange = Debug.log "url changed" >> RouteChanged
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


routeParser : Dictionary -> Url.Parser.Parser (Route -> a) a
routeParser dict =
    let
        emptyEntry =
            Entry.empty
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (\filter ->
                let
                    homeModel =
                        initialHomeModel Nothing
                in
                ShowCard { homeModel | searchText = filter }
            )
            (Url.Parser.top <?> Url.Parser.Query.string "filter")
        , Url.Parser.map
            (\de -> AddWord { emptyEntry | de = Maybe.withDefault "" de })
            (Url.Parser.s "entries" </> Url.Parser.s "_new" <?> Url.Parser.Query.string "de")
        , Url.Parser.map
            (\de filter ->
                let
                    homeModel =
                        initialHomeModel (Just (entryOf dict de))
                in
                ShowCard { homeModel | searchText = filter }
            )
            (Url.Parser.s "entries" </> Url.Parser.string <?> Url.Parser.Query.string "filter")
        , Url.Parser.map
            (entryOf dict >> (\entry -> EditWord entry entry))
            (Url.Parser.s "entries" </> Url.Parser.string </> Url.Parser.s "_edit")
        ]


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


route : Url -> Dictionary -> ( Route, Cmd Msg )
route url dict =
    case Url.Parser.parse (routeParser dict) url of
        Just (AddWord word) ->
            ( AddWord word, Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp) )

        Just r ->
            ( r, Cmd.none )

        Nothing ->
            ( ShowCard (initialHomeModel Nothing), Cmd.none )


initialModel : Url -> Key -> Int -> ( Model, Cmd Msg )
initialModel url key randomSeed =
    let
        ( theRoute, routeCmd ) =
            route url Array.empty
    in
    ( { dict = Array.empty
      , seed = Random.initialSeed randomSeed
      , route = theRoute
      , userId = Nothing
      , notification = ( False, "" )
      , key = key
      , zone = Time.utc
      , zoneName = Offset 0
      }
    , Cmd.batch
        [ routeCmd
        , Time.here |> Task.attempt ZoneResolved
        , Time.getZoneName |> Task.attempt ZoneNameResolved
        ]
    )


initialHomeModel : Maybe Entry -> HomeModel
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
    | RouteChanged Url
    | WithModel (Model -> ( Model, Cmd Msg ))
    | ZoneResolved (Result String Zone)
    | ZoneNameResolved (Result String ZoneName)
    | NewUrlRequested UrlRequest
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
                            ( { model | seed = nextSeed }
                            , maybeEntry
                                |> Maybe.map (\{ de } -> Browser.Navigation.pushUrl model.key ("/entries/" ++ de))
                                |> Maybe.withDefault Cmd.none
                            )

                        ClickSearchResult entry ->
                            ( model
                            , Browser.Navigation.pushUrl model.key ("/entries/" ++ entry.de)
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
                            ( model
                            , Browser.Navigation.pushUrl model.key
                                ((homeModel.entry
                                    |> Maybe.map (\entry -> "/entries/" ++ entry.de)
                                    |> Maybe.withDefault "/"
                                 )
                                    ++ "?filter="
                                    ++ text
                                )
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
                ( _, CloseEditor ) ->
                    ( model, Browser.Navigation.back model.key 1 )

                ( EditWord entry originalEntry, SaveAndCloseEditor ) ->
                    updateWithCurrentTime model
                        (\now theModel ->
                            { entry | updatedAt = now }
                                |> (\theEntry ->
                                        let
                                            replace from to e =
                                                if e == originalEntry then
                                                    theEntry

                                                else
                                                    e

                                            dict =
                                                model.dict
                                                    |> Array.map (replace originalEntry theEntry)
                                        in
                                        ( { theModel
                                            | dict = dict
                                            , route = ShowCard (initialHomeModel (Just theEntry))
                                          }
                                        , theModel.userId
                                            |> Maybe.map (\userId -> saveEntry ( userId, Entry.encode theEntry ))
                                            |> Maybe.withDefault Cmd.none
                                        )
                                   )
                        )

                ( AddWord entry, SaveAndCloseEditor ) ->
                    updateWithCurrentTime model
                        (\now theModel ->
                            { entry | addedAt = now, updatedAt = now }
                                |> (\theEntry ->
                                        ( { theModel
                                            | dict = theModel.dict |> Array.append (Array.fromList [ theEntry ])
                                            , route = ShowCard (initialHomeModel (Just theEntry))
                                          }
                                        , theModel.userId
                                            |> Maybe.map (\userId -> saveEntry ( userId, Entry.encode theEntry ))
                                            |> Maybe.withDefault Cmd.none
                                        )
                                   )
                        )

                ( EditWord _ entry, DeleteEntry ) ->
                    ( { model | dict = model.dict |> Array.filter ((/=) entry) }
                    , Cmd.batch
                        [ model.userId
                            |> Maybe.map (\userId -> deleteEntry ( userId, entry.de ))
                            |> Maybe.withDefault Cmd.none
                        , Browser.Navigation.pushUrl model.key "" -- TODO
                        ]
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
                newDict =
                    Array.fromList dict

                modelWithNewDict =
                    { model | dict = newDict }
            in
            case model.route of
                ShowCard { entry, searchText } ->
                    case entry of
                        Just { de } ->
                            de
                                |> entryOf newDict
                                |> Just
                                |> initialHomeModel
                                |> (\homeModel -> ( { modelWithNewDict | route = ShowCard homeModel }, Cmd.none ))

                        Nothing ->
                            let
                                ( maybeEntry, updatedSeed ) =
                                    randomEntry model.seed
                                        (searchResults newDict searchText)
                            in
                            ( { modelWithNewDict | seed = updatedSeed }
                            , maybeEntry
                                |> Maybe.map (\{ de } -> Browser.Navigation.pushUrl model.key ("/entries/" ++ de))
                                |> Maybe.withDefault Cmd.none
                            )

                _ ->
                    ( modelWithNewDict, Cmd.none )

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

        RouteChanged url ->
            let
                ( theRoute, cmd ) =
                    route url model.dict |> Debug.log "route"
            in
            ( { model | route = theRoute }, cmd )

        ZoneResolved (Ok zone) ->
            ( { model | zone = zone }, Cmd.none )

        ZoneResolved (Err errorMessage) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

        ZoneNameResolved (Ok zoneName) ->
            ( { model | zoneName = zoneName }, Cmd.none )

        ZoneNameResolved (Err errorMessage) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

        WithModel withModel ->
            withModel model

        NewUrlRequested urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        NoOp ->
            ( model, Cmd.none )


updateWithCurrentTime :
    Model
    -> (Time.Posix -> Model -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
updateWithCurrentTime model theUpdate =
    ( model
    , Time.now |> Task.attempt (Result.map (\now -> WithModel (theUpdate now)) >> Result.withDefault NoOp)
    )


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
                    , "select-none"
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
            ++ (let
                    results =
                        searchResults dict homeModel.searchText
                            |> Array.toList
                in
                case ( homeModel.expandSearchResults || List.length results == 0, homeModel.searchText ) of
                    ( True, Just text ) ->
                        [ ( "searchResult", searchResultView dict results text |> Html.map HomeMsg ) ]

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
                [ text (prefix ++ (resultCount |> String.fromInt) ++ " Wörter") ]
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


searchResultView : Dictionary -> List Entry -> String -> Html HomeMsg
searchResultView dict results searchText =
    case List.length results of
        0 ->
            a
                [ href ("/entries/_new?de=" ++ searchText)
                , classNames
                    (btnClasses True False
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
            ul [ classNames [ "list-reset", "py-3" ] ]
                (results
                    |> List.sortBy Entry.toComparable
                    |> List.map (searchResultRow searchText)
                )


searchResultRow searchText entry =
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
        [ div [ classNames [ "inline-block", "mr-2" ] ] (hilighted searchText entry.de)
        , div [ classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText entry.ja)
        ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView : HomeModel -> Entry -> Html HomeMsg
cardView model entry =
    let
        textToShow =
            case ( model.direction, model.isTranslated ) of
                ( DeToJa, False ) ->
                    entry.de

                ( JaToDe, False ) ->
                    entry.ja

                ( DeToJa, True ) ->
                    entry.ja

                ( JaToDe, True ) ->
                    entry.de

        simpleDe =
            Entry.withoutArticle entry
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
                        [ href ("https://de.wiktionary.org/wiki/" ++ simpleDe)
                        , target "_blank"
                        , classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Untersuchen" ]
                    , a
                        [ href ("https://translate.google.co.jp/m/translate?hl=ja#view=home&op=translate&sl=de&tl=ja&text=" ++ simpleDe)
                        , target "_blank"
                        , classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Hören" ]
                    , a
                        [ href ("/entries/" ++ entry.de ++ "/_edit")
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


entryDetailView { de, pos, ja, example } =
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
            ++ (example
                    |> Maybe.map
                        (\ex ->
                            [ section [ classNames [ "mb-2" ] ]
                                [ h3 [] [ text "Beispiel" ]
                                , p [] [ text (Entry.censorExample ex) ]
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
            , "no-underline"
            ]
        , href "/entries/_new"
        ]
        [ div [ style "margin-top" "-8px" ] [ text "+" ] ]


editorView : Model -> Bool -> Entry -> Html EditorMsg
editorView model isNew ({ de, pos, ja, example, updatedAt, addedAt } as entry) =
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
            ([ inputRowView "Deutsch"
                (textInputView (Just "editor-input-de")
                    de
                    False
                    (\value -> WordChange { entry | de = value })
                )
             , inputRowView "Teil"
                (selectInputView
                    pos
                    (\value ->
                        WordChange
                            { entry
                                | pos =
                                    value
                                        |> PartOfSpeech.fromString
                                        |> Result.withDefault pos
                            }
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
                    (\value -> WordChange { entry | ja = value })
                )
             , inputRowView "Beispiel"
                (textInputView Nothing
                    (example |> Maybe.withDefault "")
                    True
                    (\value ->
                        WordChange
                            { entry
                                | example =
                                    if value == "" then
                                        Nothing

                                    else
                                        Just value
                            }
                    )
                )
             ]
                ++ (if isNew then
                        []

                    else
                        let
                            addedAtExpr =
                                describeDate model.zone model.zoneName addedAt

                            updatedAtExpr =
                                describeDate model.zone model.zoneName updatedAt
                        in
                        [ p
                            [ classNames
                                [ "text-grey-dark"
                                , "my-6"
                                ]
                            ]
                            [ text
                                ("Added on "
                                    ++ addedAtExpr
                                    ++ (if addedAtExpr /= updatedAtExpr then
                                            ", updated on "
                                                ++ updatedAtExpr

                                        else
                                            ""
                                       )
                                )
                            ]
                        ]
                   )
                ++ [ button
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
            )
        ]


describeDate zone zoneName posix =
    if Time.posixToMillis posix == 0 then
        "(Unknown)"

    else
        [ posix |> Time.toDay zone |> String.fromInt
        , "."
        , (case posix |> Time.toMonth zone of
            Jan ->
                1

            Feb ->
                2

            Mar ->
                3

            Apr ->
                4

            May ->
                5

            Jun ->
                6

            Jul ->
                7

            Aug ->
                8

            Sep ->
                9

            Oct ->
                10

            Nov ->
                11

            Dec ->
                12
          )
            |> String.fromInt
        , "."
        , posix |> Time.toYear zone |> String.fromInt
        , " ("
        , case zoneName of
            Name name ->
                name

            Offset value ->
                String.fromInt value
        , ")"
        ]
            |> String.join ""


isValid { de, ja, example } =
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
    , ( "select-none", True )
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
