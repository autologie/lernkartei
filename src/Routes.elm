module Routes exposing (Route(..), RoutingAction(..), extractAccumulatingSession, extractSession, resolve)

import AppUrl exposing (GlobalQueryParams)
import Browser.Navigation exposing (Key)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry
import Data.Session as Session exposing (AccumulatingSession, Session)
import Pages.Card
import Pages.Editor
import Pages.Initialize
import Time
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>), s, string)
import Url.Parser.Query as Query


type Route
    = Initializing Pages.Initialize.Model
    | Card Pages.Card.Model
    | Editor Pages.Editor.Model
    | NotFound Key


type RoutingAction
    = AwaitInitialization
    | RedirectToRandom GlobalQueryParams
    | Show Route


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> a) a
resolve maybeSession =
    Url.Parser.oneOf
        [ Url.Parser.map
            (\a b -> buildQueryParams a b |> RedirectToRandom)
            (s "entries" </> s "_random" <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (\a b -> buildQueryParams a b |> RedirectToRandom)
            (Url.Parser.top <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (dispatchNewEntry maybeSession)
            (s "entries" </> s "_new" <?> Query.string "de" <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (dispatchCard maybeSession)
            (s "entries" </> string <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (dispatchEditor maybeSession)
            (s "entries" </> string </> s "_edit" <?> Query.string "filter" <?> Query.int "shuffle")
        ]


dispatchCard maybeSession de filter shuffle =
    let
        withSession session =
            Show
                (Card
                    (Pages.Card.initialModel
                        { session | globalParams = buildQueryParams filter shuffle }
                        de
                    )
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


dispatchNewEntry maybeSession de filter shuffle =
    let
        emptyEntry =
            Entry.empty

        withSession session =
            Show
                (Editor
                    { entry = { emptyEntry | de = Maybe.withDefault "" de }
                    , originalEntry = Nothing
                    , dialog = Nothing
                    , session = { session | globalParams = buildQueryParams filter shuffle }
                    }
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


dispatchEditor maybeSession de filter shuffle =
    let
        withSession session =
            let
                entry =
                    Dictionary.get de session.dict
            in
            Show
                (Editor
                    { entry = entry
                    , originalEntry = Just entry
                    , dialog = Nothing
                    , session = { session | globalParams = buildQueryParams filter shuffle }
                    }
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


buildQueryParams : Maybe String -> Maybe Int -> GlobalQueryParams
buildQueryParams filters shuffle =
    { filters = filters
    , shuffle =
        shuffle
            |> Maybe.map (\n -> n == 1)
            |> Maybe.withDefault False
    }


extractSession : Route -> Maybe Session
extractSession routes =
    case routes of
        Initializing { session } ->
            Session.toSession session

        Card pageModel ->
            Just pageModel.session

        Editor pageModel ->
            Just pageModel.session

        NotFound _ ->
            Nothing


extractAccumulatingSession : Route -> AccumulatingSession
extractAccumulatingSession routes =
    case routes of
        Initializing { session } ->
            session

        Card { session } ->
            Session.toAccumulatingSession session

        Editor { session } ->
            Session.toAccumulatingSession session

        NotFound navigationKey ->
            { navigationKey = navigationKey
            , userId = Nothing
            , dict = Nothing
            , zone = Nothing
            , zoneName = Nothing
            , startTime = Time.millisToPosix 0
            }
