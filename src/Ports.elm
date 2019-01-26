port module Ports exposing (saveEntry,deleteEntry)

import Json.Encode as Encode


port saveEntry : ( String, Encode.Value ) -> Cmd msg


port deleteEntry : ( String, String ) -> Cmd msg
