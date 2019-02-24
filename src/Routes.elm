module Routes exposing (Route(..), RoutingAction(..), extractAccumulatingSession, extractSession, resolve)

import Browser.Navigation exposing (Key)
import Data.AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary
import Data.Entry as Entry
import Data.Filter as Filter
import Data.Session as Session exposing (AccumulatingSession, Session)
import Pages.Editor
import Pages.Entry
import Pages.Initialize
import Pages.List
import Pages.Search
import Time
import Url.Parser exposing ((</>), (<?>), s, string)
import Url.Parser.Query as Query


type Route
    = Initializing Pages.Initialize.Model
    | Entry Pages.Entry.Model
    | Editor Pages.Editor.Model
    | Search Pages.Search.Model
    | Entries Pages.List.Model
    | NotFound Key


type RoutingAction
    = AwaitInitialization
    | RedirectToRandom GlobalQueryParams
    | Show Route


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> a) a
resolve =
    Maybe.map resolveWithSession
        >> Maybe.withDefault (Url.Parser.custom "INITIALIZING" <| always (Just AwaitInitialization))


resolveWithSession : Session -> Url.Parser.Parser (RoutingAction -> a) a
resolveWithSession session =
    let
        globalParams path =
            path
                <?> Query.string "filter"
                <?> Query.int "shuffle"
                <?> Query.int "translate"
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (resolveSearch session)
            (s "search" |> globalParams)
        , Url.Parser.map
            (resolveList session)
            (s "entries" |> globalParams)
        , Url.Parser.map
            (\a b c -> buildQueryParams a b c |> RedirectToRandom)
            (s "entries" </> s "_next" |> globalParams)
        , Url.Parser.map
            (\a b c -> buildQueryParams a b c |> RedirectToRandom)
            (Url.Parser.top |> globalParams)
        , Url.Parser.map
            (resolveNewEntry session)
            (s "entries" </> s "_new" <?> Query.string "de" |> globalParams)
        , Url.Parser.map
            (resolveEntry session)
            (s "entries" </> string |> globalParams)
        , Url.Parser.map
            (resolveEditor session)
            (s "entries" </> string </> s "_edit" |> globalParams)
        ]


resolveEntry : Session -> String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveEntry session index filter shuffle translate =
    resolveWithFallback
        (\entry ->
            Pages.Entry.initialModel
                { session | globalParams = buildQueryParams filter shuffle translate }
                entry
                |> (Entry >> Show)
        )
        session
        index


resolveNewEntry : Session -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveNewEntry session index filter shuffle translate =
    let
        emptyEntry =
            Entry.empty
    in
    { entry = { emptyEntry | index = Maybe.withDefault "" index }
    , originalEntry = Nothing
    , dialog = Nothing
    , session = { session | globalParams = buildQueryParams filter shuffle translate }
    }
        |> (Editor >> Show)


resolveEditor : Session -> String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveEditor session index filter shuffle translate =
    resolveWithFallback
        (\entry ->
            { entry = entry
            , originalEntry = Just entry
            , dialog = Nothing
            , session = { session | globalParams = buildQueryParams filter shuffle translate }
            }
                |> (Editor >> Show)
        )
        session
        index


resolveSearch : Session -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveSearch session filter shuffle translate =
    Pages.Search.initialModel
        { session
            | globalParams = buildQueryParams filter shuffle translate
        }
        |> (Search >> Show)


resolveList : Session -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveList session filter shuffle translate =
    Pages.List.initialModel
        { session
            | globalParams = buildQueryParams filter shuffle translate
        }
        |> (Entries >> Show)


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

        Entry pageModel ->
            Just pageModel.session

        Editor pageModel ->
            Just pageModel.session

        Search pageModel ->
            Just pageModel.session

        Entries pageModel ->
            Just pageModel.session

        NotFound _ ->
            Nothing


extractAccumulatingSession : Route -> AccumulatingSession
extractAccumulatingSession routes =
    case routes of
        Initializing { session } ->
            session

        Entry { session } ->
            Session.toAccumulatingSession session

        Editor { session } ->
            Session.toAccumulatingSession session

        Search { session } ->
            Session.toAccumulatingSession session

        Entries { session } ->
            Session.toAccumulatingSession session

        NotFound navigationKey ->
            { navigationKey = navigationKey
            , userId = Nothing
            , dict = Nothing
            , zone = Nothing
            , zoneName = Nothing
            , startTime = Time.millisToPosix 0
            , language = Session.Japanese
            }


resolveWithFallback : (Entry.Entry -> RoutingAction) -> Session -> String -> RoutingAction
resolveWithFallback resolveWithEntry session index =
    Dictionary.get index session.dict
        |> Maybe.map resolveWithEntry
        |> Maybe.withDefault (Show (NotFound session.navigationKey))
