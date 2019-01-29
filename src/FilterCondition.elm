module FilterCondition exposing (FilterCondition(..), isMatchedTo)

import Entry exposing (Entry)
import PartOfSpeech exposing (PartOfSpeech(..))
import Regex
import Time


type FilterCondition
    = StartsWith String
    | EndsWith String
    | Contains String
    | PartOfSpeechIs PartOfSpeech
    | IsStarred
    | IsAddedIn Duration


type Duration
    = RelativeDays Int Int


isMatchedTo : Time.Posix -> String -> Entry -> Bool
isMatchedTo now str entry =
    str |> parse |> List.all (isMatchedToHelp now entry)


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


relativeDaysRegex =
    Regex.fromString "^-([\\d]+)d\\+([\\d]+)d$" |> Maybe.withDefault Regex.never


fromString : String -> Result String FilterCondition
fromString str =
    if String.startsWith "t:" str then
        case
            String.dropLeft 2 str
                |> Regex.find relativeDaysRegex
                |> List.map .submatches
        of
            [ [ Just fromExpr, Just numberOfDaysExpr ] ] ->
                Maybe.map2
                    (\from numberOfDays -> Ok (IsAddedIn (RelativeDays from numberOfDays)))
                    (String.toInt fromExpr)
                    (String.toInt numberOfDaysExpr)
                    |> Maybe.withDefault (Err "Failed to parse number.")

            _ ->
                Err (str ++ " is unsupported notation of a term.")

    else if String.startsWith ":" str then
        case str of
            ":s" ->
                Ok IsStarred

            ":v" ->
                Ok (PartOfSpeechIs Verb)

            ":sub" ->
                Ok (PartOfSpeechIs Substantiv)

            ":adj" ->
                Ok (PartOfSpeechIs Adjektiv)

            ":adv" ->
                Ok (PartOfSpeechIs Adverb)

            ":k" ->
                Ok (PartOfSpeechIs Konjunktion)

            ":p" ->
                Ok (PartOfSpeechIs Praeposition)

            ":m" ->
                Ok (PartOfSpeechIs Modalpartikel)

            _ ->
                Err (str ++ " is unsupported directive.")

    else if String.startsWith "^" str then
        Ok (StartsWith (String.toLower (String.dropLeft 1 str)))

    else if String.endsWith "$" str then
        Ok (EndsWith (String.toLower (String.dropRight 1 str)))

    else
        Ok (Contains (String.toLower str))


isMatchedToHelp : Time.Posix -> Entry -> FilterCondition -> Bool
isMatchedToHelp now { de, pos, ja, starred, addedAt } filter =
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

        IsStarred ->
            starred

        IsAddedIn (RelativeDays from numberOfDays) ->
            let
                millisInDay =
                    24 * 60 * 60 * 1000

                addedAtMillis =
                    addedAt |> Time.posixToMillis

                fromMillis =
                    ((now |> Time.posixToMillis) // millisInDay + 1 - from) * millisInDay

                toMillis =
                    fromMillis + numberOfDays * millisInDay
            in
            fromMillis < addedAtMillis && addedAtMillis < toMillis
