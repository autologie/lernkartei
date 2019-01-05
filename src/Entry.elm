module Entry exposing (Entry(..), decode, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type Entry
    = Entry String String (Maybe String)


encode : Entry -> Encode.Value
encode (Entry de ja example) =
    Encode.object
        ([ ( "id", Encode.string de )
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
    Decode.map3
        (\de ja example ->
            Entry de
                ja
                (if example == Just "" then
                    Nothing

                 else
                    example
                )
        )
        (Decode.field "id" Decode.string)
        (Decode.field "translation" Decode.string)
        (Decode.maybe (Decode.field "example" Decode.string))
