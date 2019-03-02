port module Ports exposing
    ( archiveEntry
    , confirmDialogResponded
    , copyToClipboard
    , copyToClipboardDone
    , deleteEntry
    , dictionaryLoaded
    , saveEntry
    , scrollChange
    , showConfirmDialog
    , signInDone
    , syncEntryDone
    )

import Json.Encode as Encode


port saveEntry : ( String, Encode.Value ) -> Cmd msg


port archiveEntry : ( String, Encode.Value ) -> Cmd msg


port deleteEntry : ( String, String ) -> Cmd msg


port syncEntryDone : (() -> msg) -> Sub msg


port signInDone : (String -> msg) -> Sub msg


port dictionaryLoaded : (List Encode.Value -> msg) -> Sub msg


port scrollChange : (Int -> msg) -> Sub msg


port copyToClipboard : String -> Cmd msg


port copyToClipboardDone : (() -> msg) -> Sub msg


port showConfirmDialog : String -> Cmd msg


port confirmDialogResponded : (Bool -> msg) -> Sub msg
