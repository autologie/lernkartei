module Entry exposing (Entry(..), decode, encode, toComparable, censorExample)

import Json.Decode as Decode
import Json.Encode as Encode
import PartOfSpeech exposing (PartOfSpeech(..))
import Regex exposing (Regex)


type Entry
    = Entry String PartOfSpeech String (Maybe String)


encode : Entry -> Encode.Value
encode (Entry de pos ja example) =
    Encode.object
        ([ ( "id", Encode.string de )
         , ( "partOfSpeech", PartOfSpeech.encode pos )
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
        (Decode.maybe (Decode.field "partOfSpeech" PartOfSpeech.decode))
        (Decode.field "translation" Decode.string)
        (Decode.maybe (Decode.field "example" Decode.string))


toComparable : Entry -> String
toComparable (Entry de _ _ _) =
    let
        articleRegex =
            Regex.fromString "^(der|die|das) " |> Maybe.withDefault Regex.never
    in
    de
        |> String.toLower
        |> Regex.replace articleRegex (.match >> (\_ -> ""))
        |> String.replace "ä" "a"
        |> String.replace "ü" "u"
        |> String.replace "ö" "o"
        |> String.replace "ß" "ss"

censorExample text =
    let
        regex =
            Regex.fromString "\\[[^\\]]+\\]"
                |> Maybe.withDefault Regex.never

        replacer =
            \_ -> "(...)"
    in
    Regex.replace regex replacer text