module Routes exposing (Route(..), RoutingAction(..), extractSession, resolve)

import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Entry
import Pages.Card
import Pages.Editor
import Pages.Initialize
import Session exposing (AccumulatingSession, Session)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>), s, string)
import Url.Parser.Query as Query


type Route
    = Initializing Pages.Initialize.Model
    | ShowCard Pages.Card.Model
    | EditWord Pages.Editor.Model
    | NotFound Key


type RoutingAction
    = AwaitInitialization
    | RedirectToRandom (Maybe String)
    | Show Route (Maybe String)


resolve : Maybe Session -> Url.Parser.Parser (RoutingAction -> a) a
resolve maybeSession =
    let
        emptyEntry =
            Entry.empty
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (\filter -> RedirectToRandom filter)
            (s "entries" </> s "_random" <?> Query.string "filter")
        , Url.Parser.map
            (\filter -> RedirectToRandom filter)
            (Url.Parser.top <?> Query.string "filter")
        , Url.Parser.map
            (\de filter ->
                maybeSession
                    |> Maybe.map
                        (\session ->
                            Show
                                (EditWord
                                    { entry = { emptyEntry | de = Maybe.withDefault "" de }
                                    , originalEntry = Nothing
                                    , dialog = Nothing
                                    , session = session
                                    }
                                )
                                filter
                        )
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> s "_new" <?> Query.string "de" <?> Query.string "filter")
        , Url.Parser.map
            (\de filter ->
                maybeSession
                    |> Maybe.map (\session -> Show (ShowCard (Pages.Card.initialModel session de)) filter)
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> string <?> Query.string "filter")
        , Url.Parser.map
            (\de filter ->
                maybeSession
                    |> Maybe.map
                        (\session ->
                            Dictionary.get de session.dict
                                |> (\entry ->
                                        Show
                                            (EditWord
                                                { entry = entry
                                                , originalEntry = Just entry
                                                , dialog = Nothing
                                                , session = session
                                                }
                                            )
                                            filter
                                   )
                        )
                    |> Maybe.withDefault AwaitInitialization
            )
            (s "entries" </> string </> s "_edit" <?> Query.string "filter")
        ]


extractSession : Route -> Maybe Session
extractSession routes =
    case routes of
        Initializing { session } ->
            Session.toSession session

        ShowCard pageModel ->
            Just pageModel.session

        EditWord pageModel ->
            Just pageModel.session

        NotFound _ ->
            Nothing
