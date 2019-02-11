module Help exposing
    ( btnClasses
    , classNames
    , groupedBtnClasses
    , isJust
    , monthNumber
    , replaceEntry
    , showText
    , updateWithCurrentTime
    )

import Data.Entry exposing (Entry)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (classList)
import Task
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


classNames : List String -> Attribute a
classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList


btnClasses : Bool -> Bool -> List String
btnClasses selected disabled =
    groupedBtnClasses selected disabled True True


groupedBtnClasses : Bool -> Bool -> Bool -> Bool -> List String
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


showText : String -> Html a
showText message =
    div
        [ classNames
            [ "w-full"
            , "h-full"
            , "flex"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ div [] [ text message ] ]


isJust : Maybe a -> Bool
isJust =
    Maybe.map (\_ -> True) >> Maybe.withDefault False


monthNumber : Posix -> Zone -> Int
monthNumber posix zone =
    case posix |> Time.toMonth zone of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
