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
import Routes exposing (Route(..))
import Task
import Time exposing (Month(..), Zone, ZoneName(..))
import Url exposing (Protocol(..), Url)
import Url.Parser


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
    { seed : Random.Seed
    , route : Route
    , userId : Maybe String
    , notification : ( Bool, String )
    , key : Key
    , zone : Zone
    , zoneName : ZoneName
    , searchText : Maybe String
    , startTime : Time.Posix
    }


initialModel : Int -> Url -> Key -> Int -> ( Model, Cmd Msg )
initialModel startTimeMillis url key randomSeed =
    ( { seed = Random.initialSeed randomSeed
      , route = Initializing (Just url) Dictionary.empty
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
    case msg of
        CardMsg pageMsg ->
            cardStep model pageMsg

        EditorMsg pageMsg ->
            editorStep model pageMsg

        ReceiveDict (Ok dict) ->
            case model.route of
                Initializing url _ ->
                    ( { model | route = Initializing url (Array.fromList dict) }
                    , Browser.Navigation.pushUrl model.key
                        (url
                            |> Maybe.map Url.toString
                            |> Maybe.withDefault "/entries/_random"
                        )
                    )

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
            ( { model | userId = Just uid }, Cmd.none )

        RouteChanged url ->
            dispatchRoute model url

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


cardStep model msg =
    case model.route of
        ShowCard pageModel ->
            model.userId
                |> Maybe.map
                    (\userId ->
                        let
                            ( updatedPageModel, pageCmd ) =
                                Pages.Card.update model.key userId pageModel msg
                        in
                        ( { model | route = ShowCard updatedPageModel }
                        , pageCmd |> Cmd.map CardMsg
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


editorStep model msg =
    case model.route of
        EditWord pageModel ->
            updateWithCurrentTime model
                (\now theModel ->
                    model.userId
                        |> Maybe.map
                            (\userId ->
                                let
                                    ( updatedPageModel, cmd ) =
                                        Pages.Editor.update userId now model.key pageModel msg (navigateTo model)
                                in
                                ( { theModel | route = EditWord updatedPageModel }
                                , cmd |> Cmd.map EditorMsg
                                )
                            )
                        |> Maybe.withDefault ( theModel, Cmd.none )
                )

        _ ->
            ( model, Cmd.none )


dispatchRoute : Model -> Url -> ( Model, Cmd Msg )
dispatchRoute model url =
    let
        dict =
            extractDict model.route
    in
    case Url.Parser.parse (Routes.parse (extractDict model.route)) url of
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
                                    dict |> Dictionary.without entry

                                route ->
                                    dict
                            )
                            filter
                        )
            in
            ( { model
                | route = Initializing (Just url) dict
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
            ( { model | route = Initializing Nothing Dictionary.empty, searchText = Nothing }
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
                text "Initializing..."

            ShowCard entry ->
                Html.Lazy.lazy4
                    Pages.Card.view
                    model.startTime
                    model.searchText
                    (searchResults model.startTime (extractDict model.route) model.searchText)
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


extractDict : Route -> Dictionary
extractDict routes =
    case routes of
        Initializing _ dict ->
            dict

        ShowCard pageModel ->
            pageModel.dict

        EditWord pageModel ->
            pageModel.dict
