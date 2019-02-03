module Main exposing (main)

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
import Routes exposing (Route(..), RoutingAction(..))
import Session exposing (AccumulatingSession, Session)
import Task
import Time exposing (Month(..), Zone, ZoneName(..))
import Url exposing (Protocol(..), Url)
import Url.Parser


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ Html.Lazy.lazy view model ] }
        , onUrlRequest = NewUrlRequested
        , onUrlChange = RouteChanged
        }


type alias Model =
    { seed : Random.Seed
    , route : Route
    , notification : ( Bool, String )
    , key : Key
    , searchText : Maybe String
    , startTime : Time.Posix
    }


subscriptions _ =
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


init : Int -> Url -> Key -> ( Model, Cmd Msg )
init startTimeMillis url key =
    ( { seed = Random.initialSeed startTimeMillis
      , route =
            Initializing (Just url)
                { navigationKey = key
                , userId = Nothing
                , dict = Nothing
                , zone = Nothing
                , zoneName = Nothing
                }
      , notification = ( False, "" )
      , key = key
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
    | ZoneResolved (Result String Zone)
    | ZoneNameResolved (Result String ZoneName)
    | NewUrlRequested UrlRequest
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardMsg pageMsg ->
            cardStep model pageMsg

        EditorMsg pageMsg ->
            editorStep model pageMsg

        ReceiveDict (Ok dict) ->
            case model.route of
                Initializing url session ->
                    ( { model | route = Initializing url { session | dict = Just (Array.fromList dict) } }, Cmd.none )
                        |> testSessionAndRouteIfPossible url

                _ ->
                    ( model, Cmd.none )

        ReceiveDict (Err _) ->
            ( { model | notification = ( True, "Failed to load the dictionary." ) }, Cmd.none )

        SyncEntryDone _ ->
            ( { model | notification = ( True, "Changes were synchronized." ) }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
            )

        CloseNotification ->
            ( { model | notification = ( False, model.notification |> Tuple.second ) }, Cmd.none )

        SignInDone uid ->
            case model.route of
                Initializing url session ->
                    ( { model | route = Initializing url { session | userId = Just uid } }, Cmd.none )
                        |> testSessionAndRouteIfPossible url

                _ ->
                    ( model, Cmd.none )

        RouteChanged url ->
            dispatchRoute model url

        ZoneResolved (Ok zone) ->
            case model.route of
                Initializing url session ->
                    ( { model | route = Initializing url { session | zone = Just zone } }, Cmd.none )
                        |> testSessionAndRouteIfPossible url

                _ ->
                    ( model, Cmd.none )

        ZoneResolved (Err errorMessage) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

        ZoneNameResolved (Ok zoneName) ->
            case model.route of
                Initializing url session ->
                    ( { model | route = Initializing url { session | zoneName = Just zoneName } }, Cmd.none )
                        |> testSessionAndRouteIfPossible url

                _ ->
                    ( model, Cmd.none )

        ZoneNameResolved (Err errorMessage) ->
            ( { model | notification = ( True, errorMessage ) }, Cmd.none )

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


cardStep model msg =
    case model.route of
        ShowCard pageModel ->
            let
                ( updatedPageModel, pageCmd ) =
                    Pages.Card.update pageModel msg
            in
            ( { model | route = ShowCard updatedPageModel }
            , pageCmd |> Cmd.map CardMsg
            )

        _ ->
            ( model, Cmd.none )


editorStep model msg =
    case model.route of
        EditWord pageModel ->
            let
                ( updatedPageModel, cmd ) =
                    Pages.Editor.update pageModel msg (navigateTo model)
            in
            ( { model | route = EditWord updatedPageModel }
            , cmd |> Cmd.map EditorMsg
            )

        _ ->
            ( model, Cmd.none )


dispatchRoute : Model -> Url -> ( Model, Cmd Msg )
dispatchRoute model url =
    let
        maybeSession =
            extractSession model.route
    in
    case Url.Parser.parse (Routes.resolve maybeSession) url of
        Just (Show r filter) ->
            ( { model | route = r, searchText = filter }
            , case r of
                EditWord _ ->
                    Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)

                _ ->
                    Cmd.none
            )

        Just (RedirectToRandom filter) ->
            let
                ( maybeEntry, updatedSeed ) =
                    randomEntry
                        model.seed
                        (searchResults model.startTime
                            (case model.route of
                                ShowCard { entry, session } ->
                                    session.dict |> Dictionary.without entry

                                route ->
                                    extractSession route
                                        |> Maybe.map .dict
                                        |> Maybe.withDefault Dictionary.empty
                            )
                            filter
                        )
            in
            ( { model
                | route = Initializing (Just url) (extractAccumulatingSession model.route)
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

        Just AwaitInitialization ->
            ( model
            , Cmd.none
            )

        Nothing ->
            ( { model | route = NotFound model.key }
            , Cmd.none
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


view : Model -> Html Msg
view model =
    div
        [ Help.classNames
            [ "w-screen"
            , "flex-row"
            , "flex"
            , "text-center"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ case model.route of
            Initializing _ _ ->
                showText "Initializing..."

            NotFound _ ->
                showText "Not found."

            ShowCard pageModel ->
                Html.Lazy.lazy4
                    Pages.Card.view
                    model.startTime
                    model.searchText
                    (searchResults model.startTime pageModel.session.dict model.searchText)
                    pageModel
                    |> Html.map CardMsg

            EditWord pageModel ->
                Html.Lazy.lazy
                    Pages.Editor.view
                    pageModel
                    |> Html.map EditorMsg
        , notificationView model.notification
        ]


showText message =
    div
        [ Help.classNames
            [ "w-full"
            , "h-full"
            , "flex"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ div [] [ text message ] ]


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


extractAccumulatingSession : Route -> AccumulatingSession
extractAccumulatingSession routes =
    case routes of
        Initializing _ accSession ->
            accSession

        ShowCard { session } ->
            Session.toAccumulatingSession session

        EditWord { session } ->
            Session.toAccumulatingSession session

        NotFound navigationKey ->
            { navigationKey = navigationKey
            , userId = Nothing
            , dict = Nothing
            , zone = Nothing
            , zoneName = Nothing
            }


extractSession : Route -> Maybe Session
extractSession routes =
    case routes of
        Initializing _ accSession ->
            Maybe.map2
                (\userId dict ->
                    { navigationKey = accSession.navigationKey
                    , userId = userId
                    , dict = dict
                    , zone = Time.utc
                    , zoneName = Offset 0
                    }
                )
                accSession.userId
                accSession.dict

        ShowCard pageModel ->
            Just pageModel.session

        EditWord pageModel ->
            Just pageModel.session

        NotFound _ ->
            Nothing


testSessionAndRouteIfPossible maybeUrl ( model, cmd ) =
    extractSession model.route
        |> Maybe.map
            (\_ ->
                ( model
                , Cmd.batch
                    [ cmd
                    , Browser.Navigation.pushUrl model.key
                        (maybeUrl
                            |> Maybe.map Url.toString
                            |> Maybe.withDefault "/entries/_random"
                        )
                    ]
                )
            )
        |> Maybe.withDefault ( model, cmd )
