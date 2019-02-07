module Routes exposing (Route(..), RoutingAction(..), extractAccumulatingSession, extractSession, resolve)

import AppUrl exposing (GlobalQueryParams)
import Browser.Navigation exposing (Key)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry
import Data.Session as Session exposing (AccumulatingSession, Session)
import Pages.Card
import Pages.Editor
import Pages.Initialize
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
    | Show Route GlobalQueryParams


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> a) a
resolve maybeSession =
    let
        emptyEntry =
            Entry.empty

        buildQueryParams : Maybe String -> Maybe Int -> GlobalQueryParams
        buildQueryParams filters shuffle =
            { filters = filters
            , shuffle =
                shuffle
                    |> Maybe.map (\n -> n == 1)
                    |> Maybe.withDefault False
            }
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (\a b -> buildQueryParams a b |> RedirectToRandom)
            (s "entries" </> s "_random" <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (\a b -> buildQueryParams a b |> RedirectToRandom)
            (Url.Parser.top <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (\de filter shuffle ->
                maybeSession
                    |> Maybe.map
                        (\session ->
                            Show
                                (Editor
                                    { entry = { emptyEntry | de = Maybe.withDefault "" de }
                                    , originalEntry = Nothing
                                    , dialog = Nothing
                                    , session = session
                                    }
                                )
                                (buildQueryParams filter shuffle)
                        )
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> s "_new" <?> Query.string "de" <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (\de filter shuffle ->
                maybeSession
                    |> Maybe.map
                        (\session ->
                            Show (Card (Pages.Card.initialModel session de))
                                (buildQueryParams filter shuffle)
                        )
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> string <?> Query.string "filter" <?> Query.int "shuffle")
        , Url.Parser.map
            (\de filter shuffle ->
                maybeSession
                    |> Maybe.map
                        (\session ->
                            Dictionary.get de session.dict
                                |> (\entry ->
                                        Show
                                            (Editor
                                                { entry = entry
                                                , originalEntry = Just entry
                                                , dialog = Nothing
                                                , session = session
                                                }
                                            )
                                            (buildQueryParams filter shuffle)
                                   )
                        )
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> string </> s "_edit" <?> Query.string "filter" <?> Query.int "shuffle")
        ]


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
            }
