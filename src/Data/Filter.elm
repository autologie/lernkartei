module Data.Filter exposing (Duration(..), Filter(..), applied, parse, toString)

import Array
import Data.Dictionary exposing (Dictionary)
import Data.Entry exposing (Entry)
import Data.PartOfSpeech exposing (PartOfSpeech(..))
import Regex exposing (Regex)
import Time


type Filter
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


parse : String -> List Filter
parse str =
    str
        |> String.split " "
        |> List.filter ((/=) "")
        |> List.map fromString


toString : List Filter -> String
toString =
    List.map toStringHelp >> String.join " "


toStringHelp : Filter -> String
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


relativeDaysRegex : Regex
relativeDaysRegex =
    Regex.fromString "^-([\\d]+)d\\+([\\d]+)d$" |> Maybe.withDefault Regex.never


fromString : String -> Filter
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


isMatchedTo : Time.Posix -> Entry -> Filter -> Bool
isMatchedTo now { index, pos, translation, starred, addedAt, tags } filter =
    let
        lowerIndex =
            String.toLower index

        lowerTranslation =
            String.toLower translation

        targets =
            [ lowerIndex, lowerTranslation ]
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


applied : Time.Posix -> Dictionary -> List Filter -> Dictionary
applied now dict filters =
    dict |> Array.filter (\e -> List.all (isMatchedTo now e) filters)
