module Data.FilterCondition exposing (Duration(..), FilterCondition(..), applied, parse, toString)

import Array
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry exposing (Entry)
import Data.PartOfSpeech as PartOfSpeech exposing (PartOfSpeech(..))
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
    | HasTag String


type Duration
    = RelativeDays Int Int


parse : String -> List FilterCondition
parse str =
    str
        |> String.split " "
        |> List.map fromString


toString : List FilterCondition -> String
toString =
    List.map toStringHelp >> String.join " "


toStringHelp : FilterCondition -> String
toStringHelp filter =
    case filter of
        StartsWith s ->
            "^" ++ s

        EndsWith s ->
            s ++ "$"

        Contains s ->
            s

        PartOfSpeechIs Verb ->
            ":v"

        PartOfSpeechIs Substantiv ->
            ":sub"

        PartOfSpeechIs Adjektiv ->
            ":adj"

        PartOfSpeechIs Adverb ->
            ":adv"

        PartOfSpeechIs Konjunktion ->
            ":k"

        PartOfSpeechIs Praeposition ->
            ":p"

        PartOfSpeechIs Modalpartikel ->
            ":m"

        IsStarred ->
            ":s"

        IsAddedIn (RelativeDays from numberOfDays) ->
            "t:-" ++ String.fromInt from ++ "d+" ++ String.fromInt numberOfDays ++ "d"

        HasTag s ->
            "e:" ++ s

        NeverMatch _ ->
            ""


relativeDaysRegex =
    Regex.fromString "^-([\\d]+)d\\+([\\d]+)d$" |> Maybe.withDefault Regex.never


fromString : String -> FilterCondition
fromString str =
    if String.startsWith "e:" str then
        HasTag (String.dropLeft 2 str)

    else if String.startsWith "t:" str then
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
                NeverMatch ("'" ++ str ++ "' is unsupported notation of a term.")

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
                NeverMatch ("'" ++ str ++ "' is unsupported directive.")

    else if String.startsWith "^" str then
        StartsWith (String.toLower (String.dropLeft 1 str))

    else if String.endsWith "$" str then
        EndsWith (String.toLower (String.dropRight 1 str))

    else
        Contains (String.toLower str)


isMatchedTo : Time.Posix -> Entry -> FilterCondition -> Bool
isMatchedTo now { de, pos, ja, starred, addedAt, tags } filter =
    let
        lowerDe =
            String.toLower de

        lowerJa =
            String.toLower ja

        targets =
            [ lowerDe, lowerJa ]
    in
    case filter of
        HasTag tag ->
            List.member tag tags

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


applied : Time.Posix -> Dictionary -> List FilterCondition -> Dictionary
applied now dict filters =
    dict |> Array.filter (\e -> List.all (isMatchedTo now e) filters)
