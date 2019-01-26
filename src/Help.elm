module Help exposing (btnClasses, classNames, groupedBtnClasses, replaceEntry)

import Entry exposing (Entry)
import Html.Attributes exposing (classList)


classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList


btnClasses selected disabled =
    groupedBtnClasses selected disabled True True


groupedBtnClasses selected disabled isFirst isLast =
    [ ( "rounded-l", isFirst )
    , ( "rounded-r", isLast )
    , ( "bg-blue", selected && not disabled )
    , ( "text-white", selected && not disabled )
    , ( "text-grey", disabled )
    , ( "text-blue", not selected && not disabled )
    , ( "shadow", selected )
    , ( "cursor-default", disabled )
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
