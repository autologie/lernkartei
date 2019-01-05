module Entry exposing (Entry(..), decode, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import PartOfSpeech exposing (PartOfSpeech(..))


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
