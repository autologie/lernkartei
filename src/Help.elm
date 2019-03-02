module Help exposing
    ( C(..)
    , btnClasses
    , classNames
    , flatten
    , groupedBtnClasses
    , isJust
    , monthNumber
    , replaceEntry
    , showText
    , toggle
    , uniq
    , updateWithCurrentTime
    )

import Data.Entry exposing (Entry)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (classList)
import Task
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


type C a b
    = V a
    | B Bool (() -> a) (() -> a)
    | O Bool (() -> a)
    | Q Bool (() -> Maybe a)
    | M (Maybe a)
    | L (List a)
    | G (Maybe b) (b -> List a)


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


flatten : List (C a b) -> List a
flatten values =
    List.concatMap
        (\value ->
            case value of
                V v ->
                    [ v ]

                O True v ->
                    [ v () ]

                O False _ ->
                    []

                Q True v ->
                    v () |> Maybe.map List.singleton |> Maybe.withDefault []

                Q False _ ->
                    []

                B True v _ ->
                    [ v () ]

                B False _ v ->
                    [ v () ]

                M v ->
                    v |> Maybe.map (\vv -> [ vv ]) |> Maybe.withDefault []

                L v ->
                    v

                G v gen ->
                    v |> Maybe.map gen |> Maybe.withDefault []
        )
        values
