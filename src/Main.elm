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
import Notification
import Pages.Card exposing (Msg(..))
import Pages.Editor
import Pages.Initialize
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
    , notification : Notification.Model
    , key : Key
    , searchText : Maybe String
    , startTime : Time.Posix
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        globalSubscription =
            Sub.batch
                [ Ports.textDisposition (TextDispositionChange >> CardMsg >> PageMsg)
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
init startTimeMillis url key =
    let
        ( pageModel, pageCmd ) =
            Pages.Initialize.init url key
    in
    ( { seed = Random.initialSeed startTimeMillis
      , route =
            Initializing pageModel
      , notification = Notification.initialModel
      , key = key
      , searchText = Nothing
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
            let
                navigate =
                    navigateTo model
            in
            case ( model.route, pageMsg_ ) of
                ( Initializing pageModel, InitializeMsg pageMsg ) ->
                    pageStep Initializing InitializeMsg (Pages.Initialize.update pageModel pageMsg) model

                ( EditWord pageModel, EditorMsg pageMsg ) ->
                    pageStep EditWord EditorMsg (Pages.Editor.update pageModel pageMsg navigate) model

                ( ShowCard pageModel, CardMsg pageMsg ) ->
                    pageStep ShowCard CardMsg (Pages.Card.update pageModel pageMsg) model

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
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
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
                                    Routes.extractSession route
                                        |> Maybe.map .dict
                                        |> Maybe.withDefault Dictionary.empty
                            )
                            filter
                        )
            in
            ( { model
                | route =
                    Initializing
                        { url = Just url
                        , session = extractAccumulatingSession model.route
                        , notification = Notification.initialModel
                        }
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
            Initializing pageModel ->
                Pages.Initialize.view pageModel |> Html.map (InitializeMsg >> PageMsg)

            NotFound _ ->
                Help.showText "Not found."

            ShowCard pageModel ->
                Html.Lazy.lazy4
                    Pages.Card.view
                    model.startTime
                    model.searchText
                    (searchResults model.startTime pageModel.session.dict model.searchText)
                    pageModel
                    |> Html.map (CardMsg >> PageMsg)

            EditWord pageModel ->
                Html.Lazy.lazy
                    Pages.Editor.view
                    pageModel
                    |> Html.map (EditorMsg >> PageMsg)
        , Notification.view model.notification CloseNotification
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
        Initializing { session } ->
            session

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
