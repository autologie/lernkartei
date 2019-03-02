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

        Just (RedirectToRandom params) ->
            redirectToRandomEntry model params

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


redirectToRandomEntry : Model -> AppUrl.GlobalQueryParams -> ( Model, Cmd Msg )
redirectToRandomEntry model params =
    let
        entries =
            Filter.applied model.startTime
                (case model.route of
                    Entry { entry, session } ->
                        if params.shuffle then
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
                Entry { entry } ->
                    entries |> Array.filter ((==) entry) |> Array.get 0

                _ ->
                    Nothing

        ( maybeEntry, updatedSeed ) =
            case ( params.shuffle, currentEntry ) of
                ( False, Just { index } ) ->
                    ( Dictionary.nextEntry index entries, model.seed )

                _ ->
                    Dictionary.randomEntry model.seed entries
    in
    ( { model | seed = updatedSeed }
    , Browser.Navigation.replaceUrl model.navigationKey
        (maybeEntry
            |> Maybe.map (\{ index } -> AppUrl.entry index params)
            |> Maybe.withDefault (AppUrl.createEntry Nothing params)
            |> AppUrl.toString
        )
    )
