port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, label, span, h1, ul, li)
import Html.Keyed
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import Http
import Random
import Array exposing (Array)


port textDisposition : (( Int, Int, Float ) -> msg) -> Sub msg


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
        , subscriptions = \_ -> Sub.batch [ textDisposition TextDispositionChange ]
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
    , textDisposition : Maybe ( Int, Int, Float )
    , searchText : Maybe String
    }


initialModel randomSeed =
    { dict = Array.empty
    , showing = Nothing
    , seed = Random.initialSeed randomSeed
    , direction = DeToJa
    , textDisposition = Nothing
    , searchText = Nothing
    }


type Msg
    = NextRandomWord
    | Translate
    | ReceiveDict (Result Http.Error String)
    | DirectionChange
    | TextDispositionChange ( Int, Int, Float )
    | SearchInput String
    | ClickSearchResult Entry


update msg model =
    case msg of
        NextRandomWord ->
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
                    , textDisposition = Nothing
                  }
                , Cmd.none
                )

        ReceiveDict (Ok str) ->
            update NextRandomWord { model | dict = parseDict str }

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
                , textDisposition = Nothing
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
                , textDisposition = Nothing
              }
            , Cmd.none
            )

        TextDispositionChange value ->
            ( { model | textDisposition = Just value }, Cmd.none )

        SearchInput text ->
            ( { model
                | searchText =
                    if text == "" then
                        Nothing
                    else
                        Just text
              }
            , Cmd.none
            )

        ClickSearchResult entry ->
            ( { model
                | searchText = Nothing
                , showing = Just ( False, entry )
                , textDisposition = Nothing
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
            [ "w-screen"
            , "p-5"
            , "flex-row"
            , "flex"
            , "text-center"
            , "justify-center"
            , "items-center"
            ]
        ]
        [ Html.Keyed.node "div"
            [ classNames
                [ "container"
                , "max-w-md"
                ]
            ]
            ([ ( "header"
               , h1
                    [ classNames
                        [ "text-center"
                        , "py-8"
                        , "text-grey-dark"
                        ]
                    ]
                    [ text "Wortkarten" ]
               )
             , ( "search"
               , div []
                    [ input
                        [ type_ "text"
                        , onInput SearchInput
                        , classNames
                            [ "border-b"
                            , "text-grey-darkest"
                            , "bg-transparent"
                            , "w-full"
                            , "text-lg"
                            , "py-2"
                            ]
                        , placeholder "Suchen"
                        , value (model.searchText |> Maybe.withDefault "")
                        ]
                        []
                    ]
               )
             ]
                ++ (case model.searchText of
                        Just text ->
                            [ ( "searchResult", searchResultView model text ) ]

                        Nothing ->
                            []
                   )
                ++ [ ( "card", cardView model ) ]
            )
        ]


searchResultView model searchText =
    ul [ classNames [ "list-reset", "py-3" ] ]
        (model.dict
            |> Array.toList
            |> List.filter (isMatchedTo searchText)
            |> List.map (searchResultRow searchText)
        )


isMatchedTo searchText (Entry de ja) =
    String.contains searchText de
        || String.contains searchText ja


searchResultRow searchText entry =
    let
        (Entry de ja) =
            entry
    in
        li
            [ classNames
                [ "p-3"
                , "text-left"
                , "rounded"
                , "cursor-pointer"
                , "hover:bg-grey-lighter"
                ]
            , onClick (ClickSearchResult entry)
            ]
            [ div [ classNames [ "inline-block", "mr-2" ] ] (hilighted searchText de)
            , div [ classNames [ "inline-block", "text-grey-dark" ] ] (hilighted searchText ja)
            ]


hilighted searchText str =
    -- TODO
    [ span [] [ text str ] ]


cardView model =
    let
        textToShow =
            model.showing
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
    in
        div []
            [ ul
                [ classNames
                    [ "py-4"
                    , "list-reset"
                    , "flex"
                    ]
                ]
                [ li [ classNames [ "flex-1", "mr-2" ] ]
                    [ button
                        [ onClick DirectionChange
                        , classNames (btnClasses (model.direction == DeToJa) ++ [ "p-3" ])
                        ]
                        [ text "De → Ja" ]
                    ]
                , li [ classNames [ "flex-1" ] ]
                    [ button
                        [ onClick DirectionChange
                        , classNames (btnClasses (model.direction == JaToDe) ++ [ "p-3" ])
                        ]
                        [ text "Ja → De" ]
                    ]
                ]
            , div
                [ classNames
                    [ "select-none"
                    , "rounded"
                    , "bg-white"
                    , "h-64"
                    , "shadow-lg"
                    , "text-grey-darkest"
                    , "relative"
                    ]
                , onClick Translate
                ]
                [ div
                    ([ id "text", attribute "data-text" textToShow ]
                        ++ (case model.textDisposition of
                                Just ( x, y, scale ) ->
                                    [ classNames
                                        [ "absolute"
                                        , "inline-block"
                                        ]
                                    , style "transform" ("scale(" ++ (String.fromFloat scale) ++ ")")
                                    , style "left" (String.fromInt x)
                                    , style "top" (String.fromInt y)
                                    ]

                                Nothing ->
                                    [ classNames [ "inline-block text-transparent" ] ]
                           )
                    )
                    [ text textToShow ]
                ]
            , button
                [ classNames
                    ((btnClasses True)
                        ++ [ "my-5"
                           , "p-4"
                           , "text-lg"
                           ]
                    )
                , onClick NextRandomWord
                ]
                [ text "Nächst" ]
            ]


btnClasses selected =
    let
        common =
            [ "rounded", "w-full" ]

        conditional =
            if selected then
                [ "bg-blue", "text-white", "shadow" ]
            else
                [ "text-blue" ]
    in
        common ++ conditional


classNames names =
    names
        |> List.map (\className -> ( className, True ))
        |> classList
