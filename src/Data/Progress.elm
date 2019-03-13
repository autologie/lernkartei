module Data.Progress exposing (Progress, current, init, next, prev)

import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry
import Data.Filter as Filter exposing (Filter)
import Random exposing (Seed)
import Random.List as RandomList
import Time exposing (Posix)


type alias Progress =
    { shown : List String
    , toBeShown : List String
    }


init : Dictionary -> List Filter -> Bool -> Posix -> Seed -> ( Progress, Seed )
init dict filters shuffle time seed =
    let
        entries =
            Filter.applied time dict filters
                |> Dictionary.entries
                |> List.sortBy Entry.toComparable
                |> List.map .index

        ( toBeShown, updatedSeed ) =
            if shuffle then
                Random.step (RandomList.shuffle entries) seed

            else
                ( entries, seed )
    in
    case toBeShown of
        head :: tail ->
            ( { shown = [ head ]
              , toBeShown = tail
              }
            , updatedSeed
            )

        [] ->
            ( { shown = [], toBeShown = [] }, updatedSeed )


next : Progress -> Progress
next { shown, toBeShown } =
    { shown = shown ++ (toBeShown |> List.head |> maybeToList)
    , toBeShown = List.tail toBeShown |> Maybe.withDefault []
    }


prev : Progress -> Progress
prev { shown, toBeShown } =
    { shown = shown |> List.take (List.length shown - 1)
    , toBeShown = (shown |> last |> maybeToList) ++ toBeShown
    }


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


last : List a -> Maybe a
last list =
    list
        |> List.drop (List.length list - 1)
        |> List.head


current : Progress -> Maybe String
current { shown } =
    last shown
