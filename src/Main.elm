module Main exposing (main)

import AppUrl exposing (GlobalQueryParams)
import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Components.Notification as Notification
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.FilterCondition as FilterCondition
import Data.PartOfSpeech as PartOfSpeech exposing (PartOfSpeech(..))
import Data.Session as Session exposing (AccumulatingSession, Session)
import Help
import Html exposing (Html, a, button, div, h1, h3, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Lazy
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Card exposing (Msg(..))
import Pages.Editor
import Pages.Initialize
import Ports
import Process
import Random
import Routes exposing (Route(..), RoutingAction(..))
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
    , notification : Notification.Model
    , navigationKey : Key
    , params : GlobalQueryParams
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
            Pages.Initialize.init url navigationKey
    in
    ( { seed = Random.initialSeed startTimeMillis
      , route = Initializing pageModel
      , notification = Notification.initialModel
      , navigationKey = navigationKey
      , params = AppUrl.emptyParams
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
    | InitializeMsg Pages.Initialize.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageMsg pageMsg_ ->
            case ( model.route, pageMsg_ ) of
                ( Initializing pageModel, InitializeMsg pageMsg ) ->
                    pageStep Initializing InitializeMsg (Pages.Initialize.update pageModel pageMsg) model

                ( Editor pageModel, EditorMsg pageMsg ) ->
                    pageStep Editor EditorMsg (Pages.Editor.update pageModel pageMsg (navigateTo model)) model

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
        Just (Show r params) ->
            ( { model | route = r, params = params }
            , case r of
                Editor _ ->
                    Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)

                _ ->
                    Cmd.none
            )

        Just (RedirectToRandom params) ->
            let
                ( maybeEntry, updatedSeed ) =
                    Dictionary.randomEntry
                        model.seed
                        (FilterCondition.applied model.startTime
                            (case model.route of
                                Card { entry, session } ->
                                    session.dict |> Dictionary.without entry

                                route ->
                                    Routes.extractSession route
                                        |> Maybe.map .dict
                                        |> Maybe.withDefault Dictionary.empty
                            )
                            params.filters
                        )
            in
            ( { model
                | route =
                    Initializing
                        { url = Just url
                        , session = Routes.extractAccumulatingSession model.route
                        , notification = Notification.initialModel
                        }
                , params = params
                , seed = updatedSeed
              }
            , Browser.Navigation.replaceUrl model.navigationKey
                ("/entries/"
                    ++ (maybeEntry
                            |> Maybe.map (\e -> e.de)
                            |> Maybe.withDefault "_new"
                       )
                    ++ (params.filters
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
                Html.Lazy.lazy4
                    Pages.Card.view
                    model.startTime
                    model.params.filters
                    (FilterCondition.applied model.startTime pageModel.session.dict model.params.filters)
                    pageModel
                    |> Html.map (CardMsg >> PageMsg)

            Editor pageModel ->
                Html.Lazy.lazy
                    Pages.Editor.view
                    pageModel
                    |> Html.map (EditorMsg >> PageMsg)
        , Notification.view model.notification CloseNotification
        ]


navigateTo model maybeEntry =
    Browser.Navigation.pushUrl model.navigationKey
        (maybeEntry
            |> Maybe.map (\{ de } -> AppUrl.card de model.params)
            |> Maybe.withDefault (AppUrl.top model.params)
            |> AppUrl.toString
        )
