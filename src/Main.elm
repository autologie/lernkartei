module Main exposing (main)

import Array
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Components.Notification as Notification
import Data.AppUrl as AppUrl
import Data.Dictionary as Dictionary
import Data.Filter as Filter
import Data.PartOfSpeech exposing (PartOfSpeech(..))
import Help
import Html exposing (Html, div)
import Html.Lazy
import Pages.Card exposing (Msg(..))
import Pages.Editor
import Pages.Initialize
import Pages.Search
import Ports
import Process
import Random
import Routes exposing (Route(..), RoutingAction(..))
import Task
import Time exposing (Month(..), ZoneName(..))
import Url exposing (Protocol(..), Url)
import Url.Parser


main : Program Int Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        , onUrlRequest = NewUrlRequested
        , onUrlChange = RouteChanged
        }


type alias Model =
    { seed : Random.Seed
    , route : Route
    , notification : Notification.Model
    , navigationKey : Key
    , startTime : Time.Posix
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        globalSubscription =
            Sub.batch
                [ -- NOTE:
                  -- This subscription cannot be move to the `Pages.Card.elm` .
                  -- If I do so, no text shows up on the card when page is loaded
                  Ports.textDisposition (TextDispositionChange >> CardMsg >> PageMsg)
                , Ports.syncEntryDone SyncEntryDone
                ]
    in
    Sub.batch
        [ globalSubscription
        , case model.route of
            Initializing pageModel ->
                Pages.Initialize.subscriptions pageModel |> Sub.map (InitializeMsg >> PageMsg)

            _ ->
                Sub.none
        ]


init : Int -> Url -> Key -> ( Model, Cmd Msg )
init startTimeMillis url navigationKey =
    let
        ( pageModel, pageCmd ) =
            Pages.Initialize.init (startTimeMillis |> Time.millisToPosix) url navigationKey
    in
    ( { seed = Random.initialSeed startTimeMillis
      , route = Initializing pageModel
      , notification = Notification.initialModel
      , navigationKey = navigationKey
      , startTime = startTimeMillis |> Time.millisToPosix
      }
    , pageCmd |> Cmd.map (InitializeMsg >> PageMsg)
    )


type Msg
    = PageMsg PageMsg
    | SyncEntryDone ()
    | CloseNotification
    | RouteChanged Url
    | NewUrlRequested UrlRequest
    | NoOp


type PageMsg
    = CardMsg Pages.Card.Msg
    | EditorMsg Pages.Editor.Msg
    | SearchMsg Pages.Search.Msg
    | InitializeMsg Pages.Initialize.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageMsg pageMsg_ ->
            case ( model.route, pageMsg_ ) of
                ( Initializing pageModel, InitializeMsg pageMsg ) ->
                    pageStep Initializing InitializeMsg (Pages.Initialize.update pageModel pageMsg) model

                ( Search pageModel, SearchMsg pageMsg ) ->
                    pageStep Search SearchMsg (Pages.Search.update pageModel pageMsg) model

                ( Editor pageModel, EditorMsg pageMsg ) ->
                    pageStep Editor EditorMsg (Pages.Editor.update pageModel pageMsg) model

                ( Card pageModel, CardMsg pageMsg ) ->
                    pageStep Card CardMsg (Pages.Card.update pageModel pageMsg) model

                _ ->
                    ( model, Cmd.none )

        SyncEntryDone _ ->
            ( { model | notification = { isShown = True, message = "Changes were synchronized." } }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
            )

        CloseNotification ->
            ( { model | notification = { isShown = False, message = model.notification.message } }, Cmd.none )

        RouteChanged url ->
            dispatchRoute model url

        NewUrlRequested urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navigationKey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        NoOp ->
            ( model, Cmd.none )


pageStep : (a -> Route) -> (m -> PageMsg) -> ( a, Cmd m ) -> Model -> ( Model, Cmd Msg )
pageStep toRoute toMsg ( updatedPageModel, pageCmd ) model =
    ( { model | route = toRoute updatedPageModel }
    , pageCmd |> Cmd.map (toMsg >> PageMsg)
    )


dispatchRoute : Model -> Url -> ( Model, Cmd Msg )
dispatchRoute model url =
    let
        maybeSession =
            Routes.extractSession model.route
    in
    case Url.Parser.parse (Routes.resolve maybeSession) url of
        Just (Show r) ->
            ( { model | route = r }
            , Cmd.batch
                [ case r of
                    Editor _ ->
                        Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)

                    Search _ ->
                        Dom.focus "search-input" |> Task.attempt (\_ -> NoOp)

                    _ ->
                        Cmd.none
                , Dom.setViewport 0 0 |> Task.attempt (\_ -> NoOp)
                ]
            )

        Just (RedirectToRandom params) ->
            let
                shuffle =
                    maybeSession
                        |> Maybe.map (\session -> session.globalParams.shuffle)
                        |> Maybe.withDefault False

                entries =
                    Filter.applied model.startTime
                        (case model.route of
                            Card { entry, session } ->
                                if shuffle then
                                    session.dict |> Dictionary.without entry

                                else
                                    session.dict

                            route ->
                                Routes.extractSession route
                                    |> Maybe.map .dict
                                    |> Maybe.withDefault Dictionary.empty
                        )
                        params.filters

                currentEntry =
                    case model.route of
                        Card { entry } ->
                            entries |> Array.filter ((==) entry) |> Array.get 0

                        _ ->
                            Nothing

                ( maybeEntry, updatedSeed ) =
                    case ( shuffle, currentEntry ) of
                        ( False, Just { index } ) ->
                            ( Dictionary.nextEntry index entries, model.seed )

                        _ ->
                            Dictionary.randomEntry model.seed entries
            in
            ( { model
                | route =
                    Initializing
                        { url = Just url
                        , session = Routes.extractAccumulatingSession model.route
                        , notification = Notification.initialModel
                        }
                , seed = updatedSeed
              }
            , Browser.Navigation.replaceUrl model.navigationKey
                (maybeEntry
                    |> Maybe.map (\{ index } -> AppUrl.card index params)
                    |> Maybe.withDefault (AppUrl.newEntry Nothing params)
                    |> AppUrl.toString
                )
            )

        Just AwaitInitialization ->
            ( model
            , Cmd.none
            )

        Nothing ->
            ( { model | route = NotFound model.navigationKey }
            , Cmd.none
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
            Initializing pageModel ->
                Pages.Initialize.view pageModel |> Html.map (InitializeMsg >> PageMsg)

            NotFound _ ->
                Help.showText "Not found."

            Card pageModel ->
                Html.Lazy.lazy Pages.Card.view pageModel
                    |> Html.map (CardMsg >> PageMsg)

            Search pageModel ->
                Html.Lazy.lazy Pages.Search.view pageModel
                    |> Html.map (SearchMsg >> PageMsg)

            Editor pageModel ->
                Html.Lazy.lazy
                    Pages.Editor.view
                    pageModel
                    |> Html.map (EditorMsg >> PageMsg)
        , Notification.view model.notification CloseNotification
        ]
