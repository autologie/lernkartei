module Entry exposing (Entry, censorExample, decode, empty, encode, toComparable)

import Json.Decode as Decode
import Json.Encode as Encode
import PartOfSpeech exposing (PartOfSpeech(..))
import Regex exposing (Regex)
import Time


type alias Entry =
    { de : String
    , ja : String
    , pos : PartOfSpeech
    , example : Maybe String
    , addedAt : Time.Posix
    , updatedAt : Time.Posix
    }


encode : Entry -> Encode.Value
encode { de, pos, ja, example, addedAt, updatedAt } =
    Encode.object
        ([ ( "id", Encode.string de )
         , ( "partOfSpeech", PartOfSpeech.encode pos )
         , ( "translation", Encode.string ja )
         , ( "addedAt", Encode.int (Time.posixToMillis addedAt) )
         , ( "updatedAt", Encode.int (Time.posixToMillis updatedAt) )
         ]
            ++ (example
                    |> Maybe.map
                        (\e -> [ ( "example", Encode.string e ) ])
                    |> Maybe.withDefault []
               )
        )


decode : Decode.Decoder Entry
decode =
    Decode.map6
        (\de pos ja example addedAt updatedAt ->
            { de = de
            , pos = pos |> Maybe.withDefault Verb
            , ja = ja
            , example =
                if example == Just "" then
                    Nothing

                else
                    example
            , addedAt = Time.millisToPosix (addedAt |> Maybe.withDefault 0)
            , updatedAt = Time.millisToPosix (updatedAt |> Maybe.withDefault 0)
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "partOfSpeech" PartOfSpeech.decode))
        (Decode.field "translation" Decode.string)
        (Decode.maybe (Decode.field "example" Decode.string))
        (Decode.maybe (Decode.field "addedAt" Decode.int))
        (Decode.maybe (Decode.field "updatedAt" Decode.int))


toComparable : Entry -> String
toComparable { de } =
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


empty =
    { de = ""
    , pos = Verb
    , ja = ""
    , example = Nothing
    , addedAt = Time.millisToPosix 0
    , updatedAt = Time.millisToPosix 0
    }
