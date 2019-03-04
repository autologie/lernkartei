module Routes exposing
    ( PageMsg(..)
    , Route(..)
    , RoutingAction(..)
    , extractAccumulatingSession
    , extractSession
    , resolve
    )

import Browser.Navigation exposing (Key)
import Data.AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary
import Data.Entry as Entry
import Data.Filter as Filter
import Data.Session as Session exposing (AccumulatingSession, Session)
import Pages.Editor
import Pages.Entries
import Pages.Entry
import Pages.Initialize
import Pages.Search
import Time
import Url
import Url.Parser exposing ((</>), (<?>), s, string)
import Url.Parser.Query as Query


type Route
    = Initializing Pages.Initialize.Model
    | Entry Pages.Entry.Model
    | Editor Pages.Editor.Model
    | Search Pages.Search.Model
    | Entries Pages.Entries.Model
    | NotFound Key


type PageMsg
    = EntryMsg Pages.Entry.Msg
    | EditorMsg Pages.Editor.Msg
    | SearchMsg Pages.Search.Msg
    | EntriesMsg Pages.Entries.Msg
    | InitializeMsg Pages.Initialize.Msg


type RoutingAction
    = AwaitInitialization
    | RedirectToRandom GlobalQueryParams
    | Show ( Route, Cmd PageMsg )


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> RoutingAction) RoutingAction
resolve =
    Maybe.map resolveWithSession
        >> Maybe.withDefault (Url.Parser.custom "INITIALIZING" <| always (Just AwaitInitialization))


resolveWithSession : Session -> Url.Parser.Parser (RoutingAction -> RoutingAction) RoutingAction
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
            Pages.Entry.init
                { session | globalParams = buildQueryParams filter shuffle translate }
                entry
                |> (\( m, c ) -> Show ( Entry m, c |> Cmd.map EntryMsg ))
        )
        session
        (decode index)


resolveNewEntry : Session -> Maybe String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveNewEntry session index filter shuffle translate =
    Pages.Editor.initCreate index { session | globalParams = buildQueryParams filter shuffle translate }
        |> (\( m, c ) -> Show ( Editor m, c |> Cmd.map EditorMsg ))


resolveEditor : Session -> String -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveEditor session index filter shuffle translate =
    resolveWithFallback
        (\entry ->
            Pages.Editor.initEdit entry { session | globalParams = buildQueryParams filter shuffle translate }
                |> (\( m, c ) -> Show ( Editor m, c |> Cmd.map EditorMsg ))
        )
        session
        (decode index)


resolveSearch : Session -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveSearch session filter shuffle translate =
    Pages.Search.init
        { session
            | globalParams = buildQueryParams filter shuffle translate
        }
        |> (\( m, c ) -> Show ( Search m, c |> Cmd.map SearchMsg ))


resolveList : Session -> Maybe String -> Maybe Int -> Maybe Int -> RoutingAction
resolveList session filter shuffle translate =
    Pages.Entries.init
        { session
            | globalParams = buildQueryParams filter shuffle translate
        }
        |> (\( m, c ) -> Show ( Entries m, c |> Cmd.map EntriesMsg ))


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
        |> Maybe.withDefault (Show ( NotFound session.navigationKey, Cmd.none ))


decode : String -> String
decode index =
    Url.percentDecode index |> Maybe.withDefault index
