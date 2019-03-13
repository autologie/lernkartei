module Data.AppUrl exposing
    ( AppUrl
    , GlobalQueryParams
    , buildQueryParams
    , createEntry
    , editEntry
    , entries
    , entry
    , nextEntry
    , prevEntry
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
    | EntryUrl String GlobalQueryParams
    | NextEntryUrl GlobalQueryParams
    | PrevEntryUrl GlobalQueryParams
    | EditorUrl String GlobalQueryParams
    | SearchUrl GlobalQueryParams
    | ListUrl GlobalQueryParams
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
            Builder.absolute path
                (List.concat
                    [ if List.length filters == 0 then
                        []

                      else
                        [ Builder.string "filter" (Filter.toString filters) ]
                    , [ Builder.int "shuffle" (encodeBool shuffle) ]
                    , [ Builder.int "translate" (encodeBool translate) ]
                    , extraParams
                    ]
                )
    in
    case url of
        TopUrl params ->
            toStringWithParams [] [] params

        NextEntryUrl params ->
            toStringWithParams [ "entries", "_next" ] [] params

        PrevEntryUrl params ->
            toStringWithParams [ "entries", "_prev" ] [] params

        EntryUrl index params ->
            toStringWithParams [ "entries", index ] [] params

        EditorUrl index params ->
            toStringWithParams [ "entries", index, "_edit" ] [] params

        SearchUrl params ->
            toStringWithParams [ "search" ] [] params

        ListUrl params ->
            toStringWithParams [ "entries" ] [] params

        NewEntryUrl Nothing params ->
            toStringWithParams [ "entries", "_new" ] [] params

        NewEntryUrl (Just index) params ->
            toStringWithParams [ "entries", "_new" ] [ Builder.string "de" index ] params


top : GlobalQueryParams -> AppUrl
top params =
    TopUrl params


entry : String -> GlobalQueryParams -> AppUrl
entry index params =
    EntryUrl index params


entries : GlobalQueryParams -> AppUrl
entries params =
    ListUrl params


search : GlobalQueryParams -> AppUrl
search params =
    SearchUrl params


nextEntry : GlobalQueryParams -> AppUrl
nextEntry params =
    NextEntryUrl params


prevEntry : GlobalQueryParams -> AppUrl
prevEntry params =
    PrevEntryUrl params


editEntry : String -> GlobalQueryParams -> AppUrl
editEntry index params =
    EditorUrl index params


createEntry : Maybe String -> GlobalQueryParams -> AppUrl
createEntry maybeIndex params =
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

        PrevEntryUrl params ->
            PrevEntryUrl (updateParams params)

        NextEntryUrl params ->
            NextEntryUrl (updateParams params)

        EntryUrl index params ->
            EntryUrl index (updateParams params)

        EditorUrl index params ->
            EditorUrl index (updateParams params)

        SearchUrl params ->
            SearchUrl (updateParams params)

        ListUrl params ->
            ListUrl (updateParams params)

        NewEntryUrl maybeIndex params ->
            NewEntryUrl maybeIndex (updateParams params)


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
