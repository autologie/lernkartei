module Data.Dictionary exposing
    ( DictValidationError(..)
    , Dictionary
    , added
    , empty
    , entries
    , filter
    , findFirstError
    , fromEntries
    , get
    , isEmpty
    , nextEntry
    , nonEmpty
    , randomEntry
    , replacedWith
    , size
    , tags
    , without
    )

import Data.Entry as Entry exposing (Entry, EntryValidationError)
import Dict exposing (Dict)
import Help
import Random exposing (Seed)
import Set


type Dictionary
    = Dictionary (Dict String Entry)


type DictValidationError
    = Duplicate String
    | InvalidEntry Entry EntryValidationError


size : Dictionary -> Int
size (Dictionary entries_) =
    Dict.size entries_


isEmpty : Dictionary -> Bool
isEmpty dict =
    size dict == 0


nonEmpty : Dictionary -> Bool
nonEmpty dict =
    not (isEmpty dict)


entries : Dictionary -> List Entry
entries (Dictionary entries_) =
    entries_ |> Dict.toList |> List.map Tuple.second


fromEntries : List Entry -> Dictionary
fromEntries =
    List.map (\entry -> ( entry.index, entry ))
        >> Dict.fromList
        >> Dictionary


without : Entry -> Dictionary -> Dictionary
without entry (Dictionary entries_) =
    Dictionary (Dict.remove entry.index entries_)


empty : Dictionary
empty =
    Dictionary Dict.empty


filter : (Entry -> Bool) -> Dictionary -> Dictionary
filter test (Dictionary entries_) =
    Dictionary (Dict.filter (\_ entry -> test entry) entries_)


get : String -> Dictionary -> Maybe Entry
get index (Dictionary entries_) =
    Dict.get index entries_


type Traversing
    = None
    | TakeNext
    | Taken Entry


nextEntry : String -> Dictionary -> Maybe Entry
nextEntry index (Dictionary entries_) =
    let
        traverse _ entry passed =
            case passed of
                None ->
                    if entry.index == index then
                        TakeNext

                    else
                        None

                TakeNext ->
                    Taken entry

                taken ->
                    taken

        toMaybe traversing =
            case traversing of
                None ->
                    Nothing

                TakeNext ->
                    Nothing

                Taken entry ->
                    Just entry
    in
    entries_ |> Dict.foldl traverse None |> toMaybe


randomEntry : Seed -> Dictionary -> ( Maybe Entry, Seed )
randomEntry seed (Dictionary entries_) =
    let
        ( index, nextSeed ) =
            Random.step
                (Random.int 0 (Dict.size entries_ - 1))
                seed
    in
    ( entries_
        |> Dict.toList
        |> List.drop (index - 1)
        |> List.head
        |> Maybe.map Tuple.second
    , nextSeed
    )


findFirstError : Dictionary -> Maybe DictValidationError
findFirstError (Dictionary entries_) =
    let
        collectError _ entry =
            Result.andThen
                (\uniqueItems ->
                    if List.member entry.index uniqueItems then
                        Err (Duplicate entry.index)

                    else
                        case Entry.findFirstError entry of
                            Just err ->
                                Err (InvalidEntry entry err)

                            Nothing ->
                                Ok (entry.index :: uniqueItems)
                )

        result =
            entries_
                |> Dict.foldl collectError (Ok [])
    in
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing


replacedWith : Entry -> Entry -> Dictionary -> Dictionary
replacedWith original replacement (Dictionary entries_) =
    let
        updateEntry maybeEntry =
            maybeEntry |> Maybe.map (always replacement)
    in
    entries_
        |> Dict.update original.index updateEntry
        |> Dictionary


added : Entry -> Dictionary -> Dictionary
added entry (Dictionary entries_) =
    Dictionary (Dict.insert entry.index entry entries_)


tags : Dictionary -> List String
tags (Dictionary entries_) =
    let
        traverse _ entry passed =
            Set.union (Set.fromList entry.tags) passed
    in
    entries_
        |> Dict.foldl traverse Set.empty
        |> Set.toList
        |> List.sort
