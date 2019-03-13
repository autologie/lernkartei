module Main exposing (main)

import Array
import Browser exposing (UrlRequest(..))
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Components.Notification as Notification
import Data.AppUrl as AppUrl
import Data.Dictionary as Dictionary
import Data.Entry
import Data.Filter as Filter
import Data.PartOfSpeech exposing (PartOfSpeech(..))
import Data.Progress as Progress exposing (Progress)
import Data.Session exposing (Session)
import Help
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Lazy
import Pages.Editor
import Pages.Entries
import Pages.Entry
import Pages.Initialize
import Pages.Search
import Ports
import Process
import Random
import Routes exposing (PageMsg(..), Route(..), RoutingAction(..))
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
        , view = \model -> { title = title model, body = [ view model ] }
        , onUrlRequest = NewUrlRequested
        , onUrlChange = RouteChanged
        }


type alias Model =
    { route : Route
    , notification : Notification.Model
    , navigationKey : Key
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        globalSubscription =
            Sub.batch
                [ Ports.syncEntryDone SyncEntryDone
                , Ports.copyToClipboardDone CopyToClipboardDone
                ]
    in
    Sub.batch
        [ globalSubscription
        , case model.route of
            Initializing pageModel ->
                Pages.Initialize.subscriptions pageModel |> Sub.map (InitializeMsg >> PageMsg)

            Search pageModel ->
                Pages.Search.subscriptions pageModel |> Sub.map (SearchMsg >> PageMsg)

            Editor pageModel ->
                Pages.Editor.subscriptions pageModel |> Sub.map (EditorMsg >> PageMsg)

            Entry pageModel ->
                Pages.Entry.subscriptions pageModel |> Sub.map (EntryMsg >> PageMsg)

            _ ->
                Sub.none
        ]


init : Int -> Url -> Key -> ( Model, Cmd Msg )
init startTimeMillis url navigationKey =
    let
        ( pageModel, pageCmd ) =
            Pages.Initialize.init (startTimeMillis |> Time.millisToPosix) url navigationKey
    in
    ( { route = Initializing pageModel
      , notification = Notification.initialModel
      , navigationKey = navigationKey
      }
    , pageCmd |> Cmd.map (InitializeMsg >> PageMsg)
    )


type Msg
    = PageMsg PageMsg
    | SyncEntryDone ()
    | CloseNotification
    | RouteChanged Url
    | NewUrlRequested UrlRequest
    | CopyToClipboardDone ()
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageMsg pageMsg_ ->
            case ( model.route, pageMsg_ ) of
                ( Initializing pageModel, InitializeMsg pageMsg ) ->
                    pageStep Initializing InitializeMsg (Pages.Initialize.update pageModel pageMsg) model

                ( Search pageModel, SearchMsg pageMsg ) ->
                    pageStep Search SearchMsg (Pages.Search.update pageModel pageMsg) model

                ( Entries pageModel, EntriesMsg pageMsg ) ->
                    pageStep Entries EntriesMsg (Pages.Entries.update pageModel pageMsg) model

                ( Editor pageModel, EditorMsg pageMsg ) ->
                    pageStep Editor EditorMsg (Pages.Editor.update pageModel pageMsg) model

                ( Entry pageModel, EntryMsg pageMsg ) ->
                    pageStep Entry EntryMsg (Pages.Entry.update pageModel pageMsg) model

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

        CopyToClipboardDone _ ->
            ( { model | notification = { isShown = True, message = "Copied!" } }
            , Process.sleep 2000 |> Task.attempt (\_ -> CloseNotification)
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
        Just (Show ( route, pageCmd )) ->
            ( { model | route = route }
            , Cmd.batch
                [ pageCmd |> Cmd.map PageMsg
                , Dom.setViewport 0 0 |> Task.attempt (\_ -> NoOp)
                ]
            )

        Just (NextCard params) ->
            moveToEntry Progress.next model params

        Just (PrevCard params) ->
            moveToEntry Progress.prev model params

        Just AwaitInitialization ->
            ( model, Cmd.none )

        Nothing ->
            ( { model | route = NotFound model.navigationKey }, Cmd.none )


title : Model -> String
title model =
    case model.route of
        Entry { entry } ->
            "Lernkartei | " ++ entry.index

        Editor { entry } ->
            "Lernkartei | " ++ entry.index ++ " (Edit)"

        _ ->
            "Lernkartei"


view : Model -> Html Msg
view model =
    div
        [ class "bg-grey-lightest font-sans font-normal outline-none overflow-x-hidden w-screen flex-row flex text-center justify-center items-center" ]
        [ case model.route of
            Initializing pageModel ->
                Pages.Initialize.view pageModel |> Html.map (InitializeMsg >> PageMsg)

            NotFound _ ->
                Help.showText "Not found."

            Entry pageModel ->
                Html.Lazy.lazy Pages.Entry.view pageModel
                    |> Html.map (EntryMsg >> PageMsg)

            Search pageModel ->
                Html.Lazy.lazy Pages.Search.view pageModel
                    |> Html.map (SearchMsg >> PageMsg)

            Entries pageModel ->
                Html.Lazy.lazy Pages.Entries.view pageModel
                    |> Html.map (EntriesMsg >> PageMsg)

            Editor pageModel ->
                Html.Lazy.lazy
                    Pages.Editor.view
                    pageModel
                    |> Html.map (EditorMsg >> PageMsg)
        , Notification.view model.notification CloseNotification
        ]


moveToEntry :
    (Progress -> Progress)
    -> Model
    -> AppUrl.GlobalQueryParams
    -> ( Model, Cmd Msg )
moveToEntry updateProgress model params =
    let
        updateSession progress entry session =
            { session | progress = updateProgress progress }

        fromEntry progress entry =
            ( Just entry
            , { model
                | route =
                    Routes.updateSession (updateSession progress entry)
                        model.route
              }
            )

        fromSession session =
            session.progress
                |> Progress.current
                |> Maybe.andThen (\index -> Dictionary.get index session.dict)
                |> Maybe.map (fromEntry session.progress)

        ( maybeEntry, updatedModel ) =
            model.route
                |> Routes.extractSession
                |> Maybe.andThen fromSession
                |> Maybe.withDefault ( Nothing, model )
    in
    ( updatedModel
    , Browser.Navigation.replaceUrl model.navigationKey
        (maybeEntry
            |> Maybe.map (\{ index } -> AppUrl.entry index params)
            |> Maybe.withDefault (AppUrl.createEntry Nothing params)
            |> AppUrl.toString
        )
    )
