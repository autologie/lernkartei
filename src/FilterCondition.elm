module FilterCondition exposing (FilterCondition(..), isMatchedTo)

import Entry exposing (Entry(..))
import PartOfSpeech exposing (PartOfSpeech(..))


type FilterCondition
    = StartsWith String
    | EndsWith String
    | Contains String
    | PartOfSpeechIs PartOfSpeech


isMatchedTo : String -> Entry -> Bool
isMatchedTo str entry =
    str |> parse |> List.all (isMatchedToHelp entry)


parse : String -> List FilterCondition
parse str =
    str
        |> String.split " "
        |> List.concatMap
            (fromString
                >> Result.toMaybe
                >> Maybe.map List.singleton
                >> Maybe.withDefault []
            )


fromString : String -> Result String FilterCondition
fromString str =
    if String.startsWith ":v" str then
        Ok (PartOfSpeechIs Verb)

    else if String.startsWith ":s" str then
        Ok (PartOfSpeechIs Substantiv)

    else if String.startsWith ":adj" str then
        Ok (PartOfSpeechIs Adjektiv)

    else if String.startsWith ":adv" str then
        Ok (PartOfSpeechIs Adverb)

    else if String.startsWith ":k" str then
        Ok (PartOfSpeechIs Konjunktion)

    else if String.startsWith ":p" str then
        Ok (PartOfSpeechIs Praeposition)

    else if String.startsWith ":m" str then
        Ok (PartOfSpeechIs Modalpartikel)

    else if String.startsWith ":" str then
        Err (str ++ " is unsupported directive.")

    else if String.startsWith "^" str then
        Ok (StartsWith (String.toLower (String.dropLeft 1 str)))

    else if String.endsWith "$" str then
        Ok (EndsWith (String.toLower (String.dropRight 1 str)))

    else
        Ok (Contains str)


isMatchedToHelp : Entry -> FilterCondition -> Bool
isMatchedToHelp (Entry de pos ja _) filter =
    let
        lowerDe =
            String.toLower de

        lowerJa =
            String.toLower ja

        targets =
            [ lowerDe, lowerJa ]
    in
    case filter of
        StartsWith head ->
            targets |> List.any (String.startsWith head)

        EndsWith last ->
            targets |> List.any (String.endsWith last)

        Contains text ->
            targets |> List.any (String.contains text)

        PartOfSpeechIs myPos ->
            pos == myPos
