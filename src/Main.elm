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
import Pages.Card exposing (Msg(..))
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


main : Program Int Model Msg
main =
    Browser.application
        { init =
            \startTimeMillis url key ->
                initialModel startTimeMillis url key startTimeMillis
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Ports.textDisposition (TextDispositionChange >> CardMsg)
                    , Ports.signInDone SignInDone
                    , Ports.syncEntryDone SyncEntryDone
                    , Ports.dictionaryLoaded
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


type Route
    = Initializing (Maybe Url)
    | ShowCard Pages.Card.Model
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
                        Pages.Card.initialModel (entryOf dict de)
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


type Msg
    = CardMsg Pages.Card.Msg
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.route, msg ) of
        ( ShowCard pageModel, CardMsg pageMsg ) ->
            model.userId
                |> Maybe.map
                    (\userId ->
                        let
                            ( updatedPageModel, updatedDict, pageCmd ) =
                                Pages.Card.update model.key userId model.dict pageModel pageMsg
                        in
                        ( { model
                            | route = ShowCard updatedPageModel
                            , dict = updatedDict
                          }
                        , pageCmd |> Cmd.map CardMsg
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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
                        |> Pages.Card.initialModel
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
                    Pages.Card.view
                    model.startTime
                    model.searchText
                    (searchResults model.startTime model.dict model.searchText)
                    entry
                    |> Html.map CardMsg

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


searchResults : Time.Posix -> Dictionary -> Maybe String -> Array Entry
searchResults now dict maybeSearchText =
    maybeSearchText
        |> Maybe.map
            (\searchText ->
                dict
                    |> Array.filter (FilterCondition.isMatchedTo now searchText)
            )
        |> Maybe.withDefault dict


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
