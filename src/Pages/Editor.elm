port module Pages.Editor exposing (Model, Msg(..), update, view)

import Array
import Browser.Navigation exposing (Key)
import Dialog
import Dictionary exposing (Dictionary)
import Entry exposing (Entry)
import Help
import Html exposing (Html, a, button, div, h1, h3, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Icon
import Json.Encode as Encode
import PartOfSpeech
import Ports
import Task
import Time exposing (Month(..), Zone, ZoneName(..))


type Msg
    = CloseEditor
    | SaveAndCloseEditor
    | WordChange Entry
    | DeleteEntry
    | DoDeleteEntry
    | CloseDialog


type alias Model =
    { entry : Entry
    , originalEntry : Maybe Entry
    , dialog : Maybe (Dialog.Dialog Msg)
    , dict : Dictionary
    }


view : Zone -> ZoneName -> Model -> Html Msg
view zone zoneName { entry, originalEntry, dialog } =
    let
        hasError =
            not (Entry.isValid entry)

        { de, pos, ja, example, updatedAt, addedAt } =
            entry

        isNew =
            originalEntry
                |> Maybe.map (\_ -> False)
                |> Maybe.withDefault True
    in
    div
        [ Help.classNames
            [ "w-full"
            , "flex"
            , "justify-center"
            , "items-start"
            ]
        ]
        ([ div
            [ Help.classNames
                [ "container"
                , "max-w-md"
                , "p-5"
                ]
            ]
            ([ inputRowView "Deutsch"
                (textInputView (Just "editor-input-de")
                    de
                    False
                    (\value -> WordChange { entry | de = value })
                )
             , inputRowView "Teil"
                (selectInputView
                    (pos |> PartOfSpeech.toString)
                    (\value ->
                        WordChange
                            { entry
                                | pos =
                                    value
                                        |> PartOfSpeech.fromString
                                        |> Result.withDefault pos
                            }
                    )
                    (PartOfSpeech.items
                        |> List.map PartOfSpeech.toString
                        |> List.map (\v -> ( v, v ))
                    )
                )
             , inputRowView "Übersetzung"
                (textInputView Nothing
                    ja
                    False
                    (\value -> WordChange { entry | ja = value })
                )
             , inputRowView "Beispiel"
                (textInputView Nothing
                    (example |> Maybe.withDefault "")
                    True
                    (\value ->
                        WordChange
                            { entry
                                | example =
                                    if value == "" then
                                        Nothing

                                    else
                                        Just value
                            }
                    )
                )
             ]
                ++ (if isNew then
                        []

                    else
                        let
                            addedAtExpr =
                                describeDate zone zoneName addedAt

                            updatedAtExpr =
                                describeDate zone zoneName updatedAt
                        in
                        [ p
                            [ Help.classNames
                                [ "text-grey-dark"
                                , "my-6"
                                ]
                            ]
                            [ text
                                ("Added on "
                                    ++ addedAtExpr
                                    ++ (if addedAtExpr /= updatedAtExpr then
                                            ", updated on "
                                                ++ updatedAtExpr

                                        else
                                            ""
                                       )
                                )
                            ]
                        ]
                   )
                ++ [ button
                        [ onClick SaveAndCloseEditor
                        , Help.classNames
                            (Help.btnClasses True hasError
                                ++ [ "w-full"
                                   , "p-3"
                                   , "text-base"
                                   , "mb-2"
                                   ]
                            )
                        , disabled hasError
                        ]
                        [ text "Hinzufügen" ]
                   , button
                        [ onClick DeleteEntry
                        , style "display"
                            (if isNew then
                                "none"

                             else
                                "inline"
                            )
                        , Help.classNames
                            ((Help.btnClasses True False |> List.filter (\c -> c /= "bg-blue"))
                                ++ [ "w-full"
                                   , "p-3"
                                   , "text-base"
                                   , "bg-red"
                                   ]
                            )
                        , disabled hasError
                        ]
                        [ text "Löschen" ]
                   , button
                        [ onClick CloseEditor
                        , Help.classNames
                            [ "fixed"
                            , "pin-r"
                            , "pin-t"
                            , "m-4"
                            ]
                        ]
                        [ Icon.close "width: 2em; height: 2em" ]
                   ]
            )
         ]
            ++ (dialog |> Maybe.map (\d -> [ Dialog.view d ]) |> Maybe.withDefault [])
        )


update : String -> Time.Posix -> Key -> Model -> Msg -> (Maybe Entry -> Cmd Msg) -> ( Model, Cmd Msg )
update userId now navigationKey ({ entry, originalEntry } as model) msg navigateTo =
    case msg of
        CloseEditor ->
            ( model
            , Browser.Navigation.back navigationKey 1
            )

        SaveAndCloseEditor ->
            { entry
                | updatedAt = now
                , addedAt = originalEntry |> Maybe.map (\_ -> entry.addedAt) |> Maybe.withDefault now
            }
                |> (\theEntry ->
                        ( { model
                            | dict =
                                originalEntry
                                    |> Maybe.map (\oe -> model.dict |> Array.map (Help.replaceEntry oe theEntry))
                                    |> Maybe.withDefault (model.dict |> Array.append (Array.fromList [ theEntry ]))
                          }
                        , Cmd.batch
                            [ Ports.saveEntry ( userId, Entry.encode theEntry )
                            , navigateTo (Just theEntry)
                            ]
                        )
                   )

        DeleteEntry ->
            ( { model | dialog = Just (Dialog.YesNoDialog "Sind Sie sich da sicher?" DoDeleteEntry CloseDialog) }
            , Cmd.none
            )

        CloseDialog ->
            ( { model | dialog = Nothing }
            , Cmd.none
            )

        DoDeleteEntry ->
            ( { model | dict = model.dict |> Array.filter ((/=) entry) }
            , Cmd.batch
                [ Ports.deleteEntry ( userId, entry.de )
                , navigateTo Nothing
                ]
            )

        WordChange updatedEntry ->
            ( { model | entry = updatedEntry }
            , Cmd.none
            )


inputRowView : String -> (List String -> Html msg) -> Html msg
inputRowView fieldName inputView =
    div [ Help.classNames [ "mb-6", "w-full" ] ]
        [ label [ Help.classNames [ "w-full" ] ]
            [ div
                [ Help.classNames
                    [ "mr-2"
                    , "text-left"
                    , "text-xs"
                    , "my-2"
                    , "w-full"
                    , "text-grey-dark"
                    ]
                ]
                [ text fieldName ]
            , inputView
                [ "bg-grey-lighter"
                , "rounded"
                , "p-3"
                , "text-grey-darkest"
                , "w-full"
                ]
            ]
        ]


textInputView maybeInputId inputValue multiline handleInput formClasses =
    if multiline then
        textarea
            [ Help.classNames
                ([ "text-sm"
                 , "leading-normal"
                 , "resize-none"
                 ]
                    ++ formClasses
                )
            , rows 5
            , value inputValue
            , onInput handleInput
            ]
            []

    else
        input
            ([ type_ "text"
             , Help.classNames ([ "text-base" ] ++ formClasses)
             , value inputValue
             , onInput handleInput
             ]
                ++ (maybeInputId
                        |> Maybe.map (\value -> [ id value ])
                        |> Maybe.withDefault []
                   )
            )
            []


selectInputView : String -> (String -> a) -> List ( String, String ) -> List String -> Html a
selectInputView inputValue handleInput options formClasses =
    select
        [ Help.classNames ([ "text-base" ] ++ formClasses)
        , style "-webkit-appearance" "none"
        , onInput handleInput
        ]
        (options
            |> List.map
                (\( v, label ) ->
                    option
                        [ value v
                        , selected (inputValue == v)
                        ]
                        [ text label ]
                )
        )


describeDate zone zoneName posix =
    if Time.posixToMillis posix == 0 then
        "(Unknown)"

    else
        [ posix |> Time.toDay zone |> String.fromInt
        , "."
        , (case posix |> Time.toMonth zone of
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
          )
            |> String.fromInt
        , "."
        , posix |> Time.toYear zone |> String.fromInt
        , " ("
        , case zoneName of
            Name name ->
                name

            Offset value ->
                String.fromInt value
        , ")"
        ]
            |> String.join ""
