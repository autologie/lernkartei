module Routes exposing (Route(..), parse)

import Dictionary exposing (Dictionary)
import Entry
import Pages.Card
import Pages.Editor
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query


type Route
    = Initializing (Maybe Url) Dictionary
    | ShowCard Pages.Card.Model
    | EditWord Pages.Editor.Model


parse : Dictionary -> Url.Parser.Parser (( Result () Route, Maybe String ) -> a) a
parse dict =
    let
        emptyEntry =
            Entry.empty
    in
    Url.Parser.oneOf
        [ Url.Parser.map
            (\filter -> ( Err (), filter ))
            (Url.Parser.s "entries"
                </> Url.Parser.s "_random"
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\filter -> ( Err (), filter ))
            (Url.Parser.top
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                ( Ok
                    (EditWord
                        { entry = { emptyEntry | de = Maybe.withDefault "" de }
                        , originalEntry = Nothing
                        , dialog = Nothing
                        , dict = dict
                        }
                    )
                , filter
                )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.s "_new"
                <?> Url.Parser.Query.string "de"
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                ( Ok (ShowCard (Pages.Card.initialModel (Dictionary.get de dict) dict))
                , filter
                )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.string
                <?> Url.Parser.Query.string "filter"
            )
        , Url.Parser.map
            (\de filter ->
                Dictionary.get de dict
                    |> (\entry ->
                            ( Ok
                                (EditWord
                                    { entry = entry
                                    , originalEntry = Just entry
                                    , dialog = Nothing
                                    , dict = dict
                                    }
                                )
                            , filter
                            )
                       )
            )
            (Url.Parser.s "entries"
                </> Url.Parser.string
                </> Url.Parser.s "_edit"
                <?> Url.Parser.Query.string "filter"
            )
        ]
