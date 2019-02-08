module AppUrl exposing
    ( AppUrl
    , GlobalQueryParams
    , card
    , editorFor
    , newEntry
    , nextCard
    , toString
    , toggleShuffle
    , top
    , withFilters
    , withoutFilters
    )

import Url.Builder as Builder exposing (QueryParameter)


type alias GlobalQueryParams =
    { filters : Maybe String
    , shuffle : Bool
    }


type AppUrl
    = TopUrl GlobalQueryParams
    | CardUrl String GlobalQueryParams
    | NextCardUrl GlobalQueryParams
    | EditorUrl String GlobalQueryParams
    | NewEntryUrl (Maybe String) GlobalQueryParams


toString : AppUrl -> String
toString url =
    let
        shuffleToString shuffle =
            if shuffle then
                1

            else
                0

        toStringWithParams path extraParams { filters, shuffle } =
            Builder.relative path
                ((filters
                    |> Maybe.map (\f -> [ Builder.string "filter" f ])
                    |> Maybe.withDefault []
                 )
                    ++ [ Builder.int "shuffle" (shuffleToString shuffle) ]
                    ++ extraParams
                )
    in
    case url of
        TopUrl params ->
            toStringWithParams [ "" ] [] params

        NextCardUrl params ->
            toStringWithParams [ "", "entries", "_next" ] [] params

        CardUrl index params ->
            toStringWithParams [ "", "entries", index ] [] params

        EditorUrl index params ->
            toStringWithParams [ "", "entries", index, "_edit" ] [] params

        NewEntryUrl Nothing params ->
            toStringWithParams [ "", "entries", "_new" ] [] params

        NewEntryUrl (Just index) params ->
            toStringWithParams [ "", "entries", "_new" ] [ Builder.string "de" index ] params


top : GlobalQueryParams -> AppUrl
top params =
    TopUrl params


card : String -> GlobalQueryParams -> AppUrl
card index params =
    CardUrl index params


nextCard : GlobalQueryParams -> AppUrl
nextCard params =
    NextCardUrl params


editorFor : String -> GlobalQueryParams -> AppUrl
editorFor index params =
    EditorUrl index params


newEntry : Maybe String -> GlobalQueryParams -> AppUrl
newEntry maybeIndex params =
    NewEntryUrl maybeIndex params


withFilters : String -> AppUrl -> AppUrl
withFilters filters =
    withParams (\params -> { params | filters = Just filters })


withoutFilters : AppUrl -> AppUrl
withoutFilters =
    withParams (\params -> { params | filters = Nothing })


toggleShuffle : AppUrl -> AppUrl
toggleShuffle =
    withParams (\params -> { params | shuffle = not params.shuffle })


withParams : (GlobalQueryParams -> GlobalQueryParams) -> AppUrl -> AppUrl
withParams updateParams url =
    case url of
        TopUrl params ->
            TopUrl (updateParams params)

        NextCardUrl params ->
            NextCardUrl (updateParams params)

        CardUrl index params ->
            CardUrl index (updateParams params)

        EditorUrl index params ->
            EditorUrl index (updateParams params)

        NewEntryUrl maybeIndex params ->
            NewEntryUrl maybeIndex (updateParams params)
