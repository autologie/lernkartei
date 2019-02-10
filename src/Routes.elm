module Routes exposing (Route(..), RoutingAction(..), extractAccumulatingSession, extractSession, resolve)

import Browser.Navigation exposing (Key)
import Data.AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary
import Data.Entry as Entry
import Data.Filter as Filter
import Data.Session as Session exposing (AccumulatingSession, Session)
import Pages.Card
import Pages.Editor
import Pages.Initialize
import Pages.Search
import Time
import Url.Parser exposing ((</>), (<?>), s, string)
import Url.Parser.Query as Query


type Route
    = Initializing Pages.Initialize.Model
    | Card Pages.Card.Model
    | Editor Pages.Editor.Model
    | Search Pages.Search.Model
    | NotFound Key


type RoutingAction
    = AwaitInitialization
    | RedirectToRandom GlobalQueryParams
    | Show Route


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> a) a
resolve maybeSession =
    let
        globalParams path =
            path
                <?> Query.string "filter"
                <?> Query.int "translate"
                <?> Query.int "shuffle"
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (resolveSearch maybeSession)
            (s "search" |> globalParams)
        , Url.Parser.map
            (\a b c -> buildQueryParams a b c |> RedirectToRandom)
            (s "entries" </> s "_next" |> globalParams)
        , Url.Parser.map
            (\a b c -> buildQueryParams a b c |> RedirectToRandom)
            (Url.Parser.top |> globalParams)
        , Url.Parser.map
            (resolveNewEntry maybeSession)
            (s "entries" </> s "_new" <?> Query.string "de" |> globalParams)
        , Url.Parser.map
            (resolveCard maybeSession)
            (s "entries" </> string |> globalParams)
        , Url.Parser.map
            (resolveEditor maybeSession)
            (s "entries" </> string </> s "_edit" |> globalParams)
        ]


resolveCard : Maybe Session -> String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveCard maybeSession de filter shuffle translate =
    let
        withSession session =
            Show
                (Card
                    (Pages.Card.initialModel
                        { session | globalParams = buildQueryParams filter shuffle translate }
                        de
                    )
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


resolveNewEntry : Maybe Session -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveNewEntry maybeSession de filter shuffle translate =
    let
        emptyEntry =
            Entry.empty

        withSession session =
            Show
                (Editor
                    { entry = { emptyEntry | de = Maybe.withDefault "" de }
                    , originalEntry = Nothing
                    , dialog = Nothing
                    , session = { session | globalParams = buildQueryParams filter shuffle translate }
                    }
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


resolveEditor : Maybe Session -> String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveEditor maybeSession de filter shuffle translate =
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
                    , session = { session | globalParams = buildQueryParams filter shuffle translate }
                    }
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


resolveSearch : Maybe Session -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveSearch maybeSession filter shuffle translate =
    let
        withSession session =
            Show
                (Search
                    (Pages.Search.initialModel
                        { session
                            | globalParams = buildQueryParams filter shuffle translate
                        }
                    )
                )
    in
    maybeSession
        |> Maybe.map withSession
        |> Maybe.withDefault AwaitInitialization


buildQueryParams : Maybe String -> Maybe Int -> Maybe Int -> GlobalQueryParams
buildQueryParams maybeFilters shuffle translate =
    let
        parseBool =
            Maybe.map ((==) 1) >> Maybe.withDefault False
    in
    { filters =
        maybeFilters
            |> Maybe.map Filter.parse
            |> Maybe.withDefault []
    , shuffle = parseBool shuffle
    , translate = parseBool translate
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

        Search pageModel ->
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

        Search { session } ->
            Session.toAccumulatingSession session

        NotFound navigationKey ->
            { navigationKey = navigationKey
            , userId = Nothing
            , dict = Nothing
            , zone = Nothing
            , zoneName = Nothing
            , startTime = Time.millisToPosix 0
            }
