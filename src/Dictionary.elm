module Dictionary exposing (Dictionary)

import Array exposing (Array)
import Entry exposing (Entry)


type alias Dictionary =
    Array Entry
