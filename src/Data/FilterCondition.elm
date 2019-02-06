module Data.FilterCondition exposing (FilterCondition(..), applied, isMatchedTo)

import Array
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry exposing (Entry)
import Data.PartOfSpeech exposing (PartOfSpeech(..))
import Regex
import Time


type FilterCondition
    = StartsWith String
    | EndsWith String
    | Contains String
    | PartOfSpeechIs PartOfSpeech
    | IsStarred
    | IsAddedIn Duration
    | NeverMatch String


type Duration
    = RelativeDays Int Int


isMatchedTo : Time.Posix -> String -> Entry -> Bool
isMatchedTo now str entry =
    str |> parse |> List.all (isMatchedToHelp now entry)


parse : String -> List FilterCondition
parse str =
    str
        |> String.split " "
        |> List.map fromString


relativeDaysRegex =
    Regex.fromString "^-([\\d]+)d\\+([\\d]+)d$" |> Maybe.withDefault Regex.never


fromString : String -> FilterCondition
fromString str =
    if String.startsWith "t:" str then
        case
            String.dropLeft 2 str
                |> Regex.find relativeDaysRegex
                |> List.map .submatches
        of
            [ [ Just fromExpr, Just numberOfDaysExpr ] ] ->
                Maybe.map2
                    (\from numberOfDays -> IsAddedIn (RelativeDays from numberOfDays))
                    (String.toInt fromExpr)
                    (String.toInt numberOfDaysExpr)
                    |> Maybe.withDefault (NeverMatch "Failed to parse number.")

            _ ->
                NeverMatch (str ++ " is unsupported notation of a term.")

    else if String.startsWith ":" str then
        case str of
            ":s" ->
                IsStarred

            ":v" ->
                PartOfSpeechIs Verb

            ":sub" ->
                PartOfSpeechIs Substantiv

            ":adj" ->
                PartOfSpeechIs Adjektiv

            ":adv" ->
                PartOfSpeechIs Adverb

            ":k" ->
                PartOfSpeechIs Konjunktion

            ":p" ->
                PartOfSpeechIs Praeposition

            ":m" ->
                PartOfSpeechIs Modalpartikel

            _ ->
                NeverMatch (str ++ " is unsupported directive.")

    else if String.startsWith "^" str then
        StartsWith (String.toLower (String.dropLeft 1 str))

    else if String.endsWith "$" str then
        EndsWith (String.toLower (String.dropRight 1 str))

    else
        Contains (String.toLower str)


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

        NeverMatch _ ->
            False


applied : Time.Posix -> Dictionary -> Maybe String -> Dictionary
applied now dict maybeSearchText =
    maybeSearchText
        |> Maybe.map
            (\searchText ->
                dict
                    |> Array.filter (isMatchedTo now searchText)
            )
        |> Maybe.withDefault dict
