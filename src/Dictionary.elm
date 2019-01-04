module Dictionary exposing (Dictionary, parse, serialize)

import Array exposing (Array)
import Entry exposing (Entry(..))
import Json.Decode as Decode
import Json.Encode as Encode


type alias Dictionary =
    Array Entry


parse : String -> Dictionary
parse dict =
    dict
        |> String.split "\n"
        |> List.filterMap parseLine
        |> Array.fromList


parseLine : String -> Maybe Entry
parseLine line =
    if line == "" || (line |> String.slice 0 1) == "#" then
        Nothing

    else
        case line |> String.split "\t" of
            [ de, ja ] ->
                Just (Entry de ja Nothing)

            [ de, ja, "" ] ->
                Just (Entry de ja Nothing)

            [ de, ja, example ] ->
                Just (Entry de ja (Just example))

            _ ->
                Nothing


serialize : Dictionary -> String
serialize dict =
    dict
        |> Array.toList
        |> List.map serializeEntry
        |> List.sort
        |> String.join "\n"


serializeEntry : Entry -> String
serializeEntry (Entry de ja maybeExample) =
    [ de
    , ja
    , maybeExample
        |> Maybe.withDefault ""
    ]
        |> String.join "\t"
