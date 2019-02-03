module Dictionary exposing (Dictionary, empty, without)

import Array exposing (Array)
import Entry exposing (Entry)


type alias Dictionary =
    Array Entry


without entry =
    Array.filter ((/=) entry)


empty =
    Array.empty
