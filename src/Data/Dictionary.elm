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
import Help
import Random exposing (Seed)
import Url


type Dictionary
    = Dictionary (List Entry)


type DictValidationError
    = Duplicate String
    | InvalidEntry Entry EntryValidationError


size : Dictionary -> Int
size (Dictionary entries_) =
    List.length entries_


isEmpty : Dictionary -> Bool
isEmpty dict =
    size dict == 0


nonEmpty : Dictionary -> Bool
nonEmpty dict =
    not (isEmpty dict)


entries : Dictionary -> List Entry
entries (Dictionary entries_) =
    entries_


fromEntries : List Entry -> Dictionary
fromEntries entries_ =
    entries_
        |> List.sortBy Entry.toComparable
        |> Dictionary


fromEntriesWithoutSorting : List Entry -> Dictionary
fromEntriesWithoutSorting =
    Dictionary


without : Entry -> Dictionary -> Dictionary
without entry =
    entries
        >> List.filter ((/=) entry)
        >> fromEntriesWithoutSorting


empty : Dictionary
empty =
    Dictionary []


filter : (Entry -> Bool) -> Dictionary -> Dictionary
filter test =
    entries
        >> List.filter test
        >> fromEntriesWithoutSorting


get : String -> Dictionary -> Maybe Entry
get index =
    let
        decoded =
            Url.percentDecode index |> Maybe.withDefault index
    in
    entries
        >> List.filter (\e -> e.index == decoded)
        >> List.head


type Traversing
    = None
    | TakeNext
    | Taken Entry


nextEntry : String -> Dictionary -> Maybe Entry
nextEntry index =
    let
        consume entry passed =
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
    entries >> List.foldl consume None >> toMaybe


randomEntry : Seed -> Dictionary -> ( Maybe Entry, Seed )
randomEntry seed dict =
    let
        ( index, nextSeed ) =
            Random.step
                (Random.int 0 (size dict - 1))
                seed
    in
    ( dict
        |> entries
        |> List.drop (index - 1)
        |> List.head
    , nextSeed
    )


findFirstError : Dictionary -> Maybe DictValidationError
findFirstError (Dictionary dict) =
    let
        collectError entry =
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
            dict |> List.foldl collectError (Ok [])
    in
    case result of
        Err e ->
            Just e

        Ok _ ->
            Nothing


replacedWith : Entry -> Entry -> Dictionary -> Dictionary
replacedWith original replacement =
    let
        updateEntry e =
            if e == original then
                replacement

            else
                e
    in
    entries
        >> List.map updateEntry
        >> fromEntries


added : Entry -> Dictionary -> Dictionary
added entry =
    entries
        >> List.append [ entry ]
        >> fromEntries


tags : Dictionary -> List String
tags =
    entries
        >> List.concatMap .tags
        >> Help.uniq
        >> List.sort
