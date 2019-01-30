port module Ports exposing (deleteEntry, dictionaryLoaded, saveEntry, signInDone, syncEntryDone, textDisposition)

import Json.Encode as Encode


port saveEntry : ( String, Encode.Value ) -> Cmd msg


port deleteEntry : ( String, String ) -> Cmd msg


port syncEntryDone : (() -> msg) -> Sub msg


port signInDone : (String -> msg) -> Sub msg


port dictionaryLoaded : (List Encode.Value -> msg) -> Sub msg


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg
