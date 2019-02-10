module Data.AppUrl exposing
    ( AppUrl
    , GlobalQueryParams
    , card
    , editorFor
    , newEntry
    , nextCard
    , search
    , toString
    , top
    , withFilters
    , withShuffle
    , withTranslate
    , withoutFilters
    )

import Data.Filter as Filter exposing (Filter)
import Url.Builder as Builder


type alias GlobalQueryParams =
    { filters : List Filter
    , shuffle : Bool
    , translate : Bool
    }


type AppUrl
    = TopUrl GlobalQueryParams
    | CardUrl String GlobalQueryParams
    | NextCardUrl GlobalQueryParams
    | EditorUrl String GlobalQueryParams
    | SearchUrl GlobalQueryParams
    | NewEntryUrl (Maybe String) GlobalQueryParams


toString : AppUrl -> String
toString url =
    let
        encodeBool value =
            if value then
                1

            else
                0

        toStringWithParams path extraParams { filters, shuffle, translate } =
            Builder.relative path
                (if List.length filters == 0 then
                    []

                 else
                    [ Builder.string "filter" (Filter.toString filters)
                    , Builder.int "shuffle" (encodeBool shuffle)
                    , Builder.int "translate" (encodeBool translate)
                    ]
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

        SearchUrl params ->
            toStringWithParams [ "", "search" ] [] params

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


search : GlobalQueryParams -> AppUrl
search params =
    SearchUrl params


nextCard : GlobalQueryParams -> AppUrl
nextCard params =
    NextCardUrl params


editorFor : String -> GlobalQueryParams -> AppUrl
editorFor index params =
    EditorUrl index params


newEntry : Maybe String -> GlobalQueryParams -> AppUrl
newEntry maybeIndex params =
    NewEntryUrl maybeIndex params


withFilters : List Filter -> AppUrl -> AppUrl
withFilters filters =
    withParams (\params -> { params | filters = filters })


withoutFilters : AppUrl -> AppUrl
withoutFilters =
    withParams (\params -> { params | filters = [] })


withShuffle : Bool -> AppUrl -> AppUrl
withShuffle value =
    withParams (\params -> { params | shuffle = value })


withTranslate : Bool -> AppUrl -> AppUrl
withTranslate value =
    withParams (\params -> { params | translate = value })


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

        SearchUrl params ->
            SearchUrl (updateParams params)

        NewEntryUrl maybeIndex params ->
            NewEntryUrl maybeIndex (updateParams params)
