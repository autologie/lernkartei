module Entry exposing (Entry(..), PartOfSpeech(..), decode, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type Entry
    = Entry String PartOfSpeech String (Maybe String)


type PartOfSpeech
    = Verb
    | Substantiv
    | Adjektiv
    | Adverb
    | Praeposition
    | Konjunktion
    | Modalpartikel


encode : Entry -> Encode.Value
encode (Entry de pos ja example) =
    Encode.object
        ([ ( "id", Encode.string de )
         , ( "partOfSpeech", encodePartOfSpeech pos )
         , ( "translation", Encode.string ja )
         ]
            ++ (example
                    |> Maybe.map
                        (\e -> [ ( "example", Encode.string e ) ])
                    |> Maybe.withDefault []
               )
        )


decode : Decode.Decoder Entry
decode =
    Decode.map4
        (\de pos ja example ->
            Entry de
                (pos |> Maybe.withDefault Verb)
                ja
                (if example == Just "" then
                    Nothing

                 else
                    example
                )
        )
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "partOfSpeech" decodePartOfSpeech))
        (Decode.field "translation" Decode.string)
        (Decode.maybe (Decode.field "example" Decode.string))


encodePartOfSpeech : PartOfSpeech -> Encode.Value
encodePartOfSpeech pos =
    (case pos of
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
    )
        |> Encode.string


decodePartOfSpeech : Decode.Decoder PartOfSpeech
decodePartOfSpeech =
    Decode.andThen
        (\str ->
            case str of
                "Verb" ->
                    Decode.succeed Verb

                "Substantiv" ->
                    Decode.succeed Substantiv

                "Adjektiv" ->
                    Decode.succeed Adjektiv

                "Adverb" ->
                    Decode.succeed Adverb

                "Praeposition" ->
                    Decode.succeed Praeposition

                "Konjunktion" ->
                    Decode.succeed Konjunktion

                "Modalpartikel" ->
                    Decode.succeed Modalpartikel

                other ->
                    Decode.fail (other ++ " is not a valid part-of-speech.")
        )
        Decode.string
