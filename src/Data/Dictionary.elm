module Data.Dictionary exposing (Dictionary, empty, get, randomEntry, without)

import Array exposing (Array)
import Data.Entry as Entry exposing (Entry)
import Random exposing (Seed)
import Url


type alias Dictionary =
    Array Entry


without entry =
    Array.filter ((/=) entry)


empty =
    Array.empty


get : String -> Dictionary -> Entry
get de dict =
    let
        emptyEntry =
            Entry.empty

        decoded =
            Url.percentDecode de |> Maybe.withDefault de
    in
    dict
        |> Array.filter (\e -> e.de == decoded)
        |> Array.toList
        |> List.head
        |> Maybe.withDefault { emptyEntry | de = decoded }


randomEntry : Seed -> Dictionary -> ( Maybe Entry, Seed )
randomEntry seed entries =
    let
        ( index, nextSeed ) =
            Random.step
                (Random.int 0 (Array.length entries - 1))
                seed
    in
    ( entries |> Array.get index
    , nextSeed
    )
