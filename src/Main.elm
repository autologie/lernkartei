module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, label, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Http
import Random
import Array exposing (Array)


main : Program ( String, Int ) Model Msg
main =
    Browser.document
        { init =
            \( dictUrl, randomSeed ) ->
                ( initialModel randomSeed
                , Http.get
                    { url = dictUrl
                    , expect = Http.expectString ReceiveDict
                    }
                )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \model -> { title = "Wortkarten", body = [ view model ] }
        }


type Entry
    = Entry String String


type Direction
    = DeToJa
    | JaToDe


type alias Model =
    { dict : Array Entry
    , showing : Maybe ( Bool, Entry )
    , seed : Random.Seed
    , direction : Direction
    }


initialModel randomSeed =
    { dict = Array.empty
    , showing = Nothing
    , seed = Random.initialSeed randomSeed
    , direction = DeToJa
    }


type Msg
    = NextWord
    | Translate
    | ReceiveDict (Result Http.Error String)
    | DirectionChange


update msg model =
    case msg of
        NextWord ->
            let
                ( index, nextSeed ) =
                    Random.step
                        (Random.int 0 ((Array.length model.dict) - 1))
                        model.seed
            in
                ( { model
                    | showing =
                        model.dict
                            |> Array.get index
                            |> Maybe.map (\entry -> ( False, entry ))
                    , seed = nextSeed
                  }
                , Cmd.none
                )

        ReceiveDict (Ok str) ->
            update NextWord { model | dict = parseDict str }

        ReceiveDict (Err message) ->
            ( model, Cmd.none )

        Translate ->
            ( { model
                | showing =
                    model.showing
                        |> Maybe.map
                            (\( showTranslation, entry ) ->
                                ( not showTranslation, entry )
                            )
              }
            , Cmd.none
            )

        DirectionChange ->
            ( { model
                | direction =
                    case model.direction of
                        DeToJa ->
                            JaToDe

                        JaToDe ->
                            DeToJa
              }
            , Cmd.none
            )


parseDict : String -> Array Entry
parseDict dict =
    dict
        |> String.split "\n"
        |> List.filterMap parseLine
        |> Array.fromList


parseLine : String -> Maybe Entry
parseLine line =
    if line == "" || (line |> String.slice 0 1) == "#" then
        Nothing
    else
        case line |> String.split "," of
            [ de, ja ] ->
                Just (Entry de ja)

            _ ->
                Nothing


view model =
    div
        [ classNames
            [ "bg-grey-lighter"
            , "w-screen"
            , "h-screen"
            , "p-5"
            , "flex-row"
            , "flex"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ div
            [ classNames
                [ "container"
                , "max-w-md"
                ]
            ]
            [ div [ classNames [ "py-3" ] ]
                [ span [] [ text "Question Type: " ]
                , label []
                    [ input
                        [ type_ "radio"
                        , onClick DirectionChange
                        , checked (model.direction == DeToJa)
                        ]
                        []
                    , span [] [ text "De → Ja" ]
                    ]
                , label []
                    [ input
                        [ type_ "radio"
                        , onClick DirectionChange
                        , checked (model.direction == JaToDe)
                        ]
                        []
                    , span [] [ text "Ja → De" ]
                    ]
                ]
            , div
                [ classNames
                    [ "select-none"
                    , "bg-white"
                    , "px5"
                    , "py-20"
                    , "shadow-lg"
                    , "text-center"
                    , fontSizeClass model.showing
                    ]
                , onClick Translate
                ]
                [ text
                    (model.showing
                        |> Maybe.map
                            (\( showTranslation, Entry de ja ) ->
                                case ( showTranslation, model.direction ) of
                                    ( False, DeToJa ) ->
                                        de

                                    ( True, JaToDe ) ->
                                        de

                                    _ ->
                                        ja
                            )
                        |> Maybe.withDefault ""
                    )
                ]
            , button
                [ classNames
                    [ "my-5"
                    , "p-4"
                    , "bg-blue"
                    , "text-white"
                    , "shadow-lg"
                    , "w-full"
                    , "btn"
                    , "text-lg"
                    ]
                , onClick NextWord
                ]
                [ text "Nächst" ]
            ]
        ]


classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList


fontSizeClass showing =
    showing
        |> Maybe.map
            (\( showTranslation, Entry de ja ) ->
                if showTranslation then
                    "text-2xl"
                else
                    "text-2xl"
             {-
                let
                    length =
                        (if showTranslation then
                            ja
                         else
                            de
                        )
                            |> String.length
                in
                    if length > 7 then
                        "text-lg"
                    else if length > 5 then
                        "text-2xl"
                    else
                        "text-3xl"
             -}
            )
        |> Maybe.withDefault ""
