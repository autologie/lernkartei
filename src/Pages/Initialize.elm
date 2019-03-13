module Pages.Initialize exposing (Model, Msg, init, subscriptions, update, view)

import Array
import Browser.Navigation exposing (Key)
import Components.Notification as Notification
import Data.AppUrl as AppUrl
import Data.Dictionary as Dictionary
import Data.Entry as Entry exposing (Entry)
import Data.Session as Session exposing (AccumulatingSession)
import Help
import Html exposing (Html, div)
import Json.Decode as Decode
import Ports
import Random
import Task
import Time exposing (Month(..), Posix, Zone, ZoneName(..))
import Url exposing (Url)


type alias Model =
    { url : Maybe Url
    , session : AccumulatingSession
    , notification : Notification.Model
    }


type Msg
    = CloseNotification
    | ReceiveDict (Result Decode.Error (List Entry))
    | SignInDone String
    | ZoneResolved (Result String Zone)
    | ZoneNameResolved (Result String ZoneName)
    | ShowNotification String


init : Posix -> Url -> Key -> ( Model, Cmd Msg )
init startTime url navigationKey =
    ( { session =
            { navigationKey = navigationKey
            , userId = Nothing
            , dict = Nothing
            , zone = Nothing
            , zoneName = Nothing
            , startTime = startTime
            , language = Session.Japanese
            , seed = Random.initialSeed (startTime |> Time.posixToMillis)
            }
      , url = Just url
      , notification = Notification.initialModel
      }
    , Cmd.batch
        [ Time.here |> Task.attempt ZoneResolved
        , Time.getZoneName |> Task.attempt ZoneNameResolved
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.signInDone SignInDone
        , Ports.dictionaryLoaded
            (List.map (Decode.decodeValue Entry.decode)
                >> reduceError
                >> ReceiveDict
            )
        ]


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    let
        prevSession =
            model.session
    in
    case msg of
        ReceiveDict (Ok dict) ->
            ( { model | session = { prevSession | dict = Just (Dictionary.fromEntries dict) } }, Cmd.none )
                |> testSessionAndRouteIfPossible

        ReceiveDict (Err _) ->
            update model (ShowNotification "Failed to load the dictionary.")

        SignInDone uid ->
            ( { model | session = { prevSession | userId = Just uid } }, Cmd.none )
                |> testSessionAndRouteIfPossible

        ZoneResolved (Ok zone) ->
            ( { model | session = { prevSession | zone = Just zone } }, Cmd.none )
                |> testSessionAndRouteIfPossible

        ZoneResolved (Err errorMessage) ->
            update model (ShowNotification errorMessage)

        ZoneNameResolved (Ok zoneName) ->
            ( { model | session = { prevSession | zoneName = Just zoneName } }, Cmd.none )
                |> testSessionAndRouteIfPossible

        ZoneNameResolved (Err errorMessage) ->
            update model (ShowNotification errorMessage)

        CloseNotification ->
            ( { model
                | notification =
                    { isShown = False
                    , message = model.notification.message
                    }
              }
            , Cmd.none
            )

        ShowNotification message ->
            ( { model
                | notification =
                    { isShown = True
                    , message = message
                    }
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ Help.showText "Initializing..."
        , Notification.view model.notification CloseNotification
        ]


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


testSessionAndRouteIfPossible : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
testSessionAndRouteIfPossible ( { session, url } as model, cmd ) =
    Session.toSession session
        |> Maybe.map
            (\_ ->
                ( model
                , Cmd.batch
                    [ cmd
                    , Browser.Navigation.pushUrl session.navigationKey
                        (url
                            |> Maybe.map Url.toString
                            |> Maybe.withDefault
                                (AppUrl.nextEntry
                                    { shuffle = False
                                    , filters = []
                                    , translate = False
                                    }
                                    |> AppUrl.toString
                                )
                        )
                    ]
                )
            )
        |> Maybe.withDefault ( model, cmd )
