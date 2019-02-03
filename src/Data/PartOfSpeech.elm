module Data.PartOfSpeech exposing (PartOfSpeech(..), decode, encode, fromString, items, toString)

import Json.Decode as Decode
import Json.Encode as Encode


type PartOfSpeech
    = Verb
    | Substantiv
    | Adjektiv
    | Adverb
    | Praeposition
    | Konjunktion
    | Modalpartikel


items =
    [ Verb
    , Substantiv
    , Adjektiv
    , Adverb
    , Praeposition
    , Konjunktion
    , Modalpartikel
    ]


toString : PartOfSpeech -> String
toString pos =
    case pos of
        Verb ->
            "Verb"

        Substantiv ->
            "Substantiv"

        Adjektiv ->
            "Adjektiv"

        Adverb ->
            "Adverb"

        Praeposition ->
            "Praeposition"

        Konjunktion ->
            "Konjunktion"

        Modalpartikel ->
            "Modalpartikel"


fromString : String -> Result String PartOfSpeech
fromString str =
    case str of
        "Verb" ->
            Ok Verb

        "Substantiv" ->
            Ok Substantiv

        "Adjektiv" ->
            Ok Adjektiv

        "Adverb" ->
            Ok Adverb

        "Praeposition" ->
            Ok Praeposition

        "Konjunktion" ->
            Ok Konjunktion

        "Modalpartikel" ->
            Ok Modalpartikel

        other ->
            Err (other ++ " is not a valid part-of-speech.")


encode : PartOfSpeech -> Encode.Value
encode =
    toString >> Encode.string


decode : Decode.Decoder PartOfSpeech
decode =
    Decode.andThen
        (\str ->
            case fromString str of
                Ok pos ->
                    Decode.succeed pos

                Err message ->
                    Decode.fail message
        )
        Decode.string
