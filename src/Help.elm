module Help exposing
    ( btnClasses
    , groupedBtnClasses
    , isJust
    , monthNumber
    , showText
    , toggle
    , uniq
    , updateWithCurrentTime
    )

import Data.Entry exposing (Entry)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (class, classList)
import Task
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


btnClasses : Bool -> Bool -> Attribute a
btnClasses selected disabled =
    groupedBtnClasses selected disabled True True


groupedBtnClasses : Bool -> Bool -> Bool -> Bool -> Attribute a
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
        |> classList


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
        [ class "w-full h-full flex justify-center items-center" ]
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


uniq : List a -> List a
uniq list =
    List.foldl
        (\value passed ->
            if List.member value passed then
                passed

            else
                passed ++ [ value ]
        )
        []
        list


toggle : a -> List a -> List a
toggle value list =
    if List.member value list then
        list |> List.filter ((/=) value)

    else
        list ++ [ value ]
