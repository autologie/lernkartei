module Help exposing (btnClasses, classNames, groupedBtnClasses, replaceEntry, updateWithCurrentTime)

import Entry exposing (Entry)
import Html.Attributes exposing (classList)
import Task
import Time


classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList


btnClasses selected disabled =
    groupedBtnClasses selected disabled True True


groupedBtnClasses selected disabled isFirst isLast =
    [ ( "rounded-l", isFirst )
    , ( "rounded-r", isLast )
    , ( "block", True )
    , ( "no-underline", True )
    , ( "bg-blue", selected && not disabled )
    , ( "text-white", selected && not disabled )
    , ( "text-grey", disabled )
    , ( "text-blue", not selected && not disabled )
    , ( "shadow", selected )
    , ( "cursor-default", disabled )
    , ( "pointer-events-none", disabled )
    , ( "select-none", True )
    ]
        |> List.filter (\( _, isIncluded ) -> isIncluded)
        |> List.map Tuple.first


replaceEntry : Entry -> Entry -> Entry -> Entry
replaceEntry from to e =
    if e == from then
        to

    else
        e


updateWithCurrentTime : a -> (Time.Posix -> a -> ( a, Cmd b )) -> ((a -> ( a, Cmd b )) -> b) -> b -> ( a, Cmd b )
updateWithCurrentTime model theUpdate onSuccessfulTime onFailedTime =
    ( model
    , Time.now
        |> Task.attempt
            (Result.map
                (\now ->
                    onSuccessfulTime (theUpdate now)
                )
                >> Result.withDefault onFailedTime
            )
    )
