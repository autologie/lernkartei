port module Pages.Editor exposing (Model, Msg(..), update, view)

import Array
import Browser.Navigation exposing (Key)
import Components.Dialog as Dialog
import Components.Icon as Icon
import Data.AppUrl as AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (DictValidationError(..), Dictionary)
import Data.Entry as Entry exposing (Entry, EntryValidationError(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h1, h3, input, label, li, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Json.Encode as Encode
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
    | WithModel (Model -> ( Model, Cmd Msg ))
    | NoOp


type alias Model =
    { entry : Entry
    , originalEntry : Maybe Entry
    , dialog : Maybe (Dialog.Dialog Msg)
    , session : Session
    }


view : Model -> Html Msg
view { entry, originalEntry, dialog, session } =
    let
        ( isNew, dictToBe ) =
            originalEntry
                |> Maybe.map (\oe -> ( False, session.dict |> Dictionary.replacedWith oe entry ))
                |> Maybe.withDefault ( True, Dictionary.added entry session.dict )

        ( deError, jaError ) =
            case Dictionary.findFirstError dictToBe of
                Just (Duplicate de) ->
                    ( Just ("\"" ++ de ++ "\" ist shon registriert sein."), Nothing )

                Just (InvalidEntry _ WordIsEmpty) ->
                    ( Just "Enter a word.", Nothing )

                Just (InvalidEntry _ TranslationIsEmpty) ->
                    ( Nothing, Just "Enter the translation." )

                _ ->
                    ( Nothing, Nothing )

        hasError =
            [ deError, jaError ]
                |> List.map Help.isJust
                |> List.member True

        { pos, ja, example, updatedAt, addedAt, tags } =
            entry
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
            ([ inputRowView "Das Wort"
                (textInputView (Just "editor-input-de")
                    entry.de
                    False
                    (\value -> WordChange { entry | de = value })
                )
                deError
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
                Nothing
             , inputRowView "Übersetzung"
                (textInputView Nothing
                    ja
                    False
                    (\value -> WordChange { entry | ja = value })
                )
                jaError
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
                Nothing
             , inputRowView "Etikett"
                (textInputView Nothing
                    (tags |> String.join "\n")
                    True
                    (\value ->
                        WordChange
                            { entry
                                | tags =
                                    value
                                        |> String.split "\n"
                                        |> List.map String.trim
                                        |> List.filter ((/=) "")
                            }
                    )
                )
                Nothing
             ]
                ++ (if isNew then
                        []

                    else
                        let
                            addedAtExpr =
                                describeDate session.zone session.zoneName addedAt

                            updatedAtExpr =
                                describeDate session.zone session.zoneName updatedAt
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


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        CloseEditor ->
            ( model
            , Browser.Navigation.back model.session.navigationKey 1
            )

        SaveAndCloseEditor ->
            Help.updateWithCurrentTime model
                (\now { entry, originalEntry, session } ->
                    let
                        theEntry =
                            { entry
                                | updatedAt = now
                                , addedAt = originalEntry |> Maybe.map (\_ -> entry.addedAt) |> Maybe.withDefault now
                            }

                        theSession =
                            model.session
                                |> Session.withDict
                                    (originalEntry
                                        |> Maybe.map (\oe -> model.session.dict |> Array.map (Help.replaceEntry oe theEntry))
                                        |> Maybe.withDefault (model.session.dict |> Array.append (Array.fromList [ theEntry ]))
                                    )
                    in
                    ( { model | session = theSession }
                    , Cmd.batch
                        [ Ports.saveEntry ( session.userId, Entry.encode theEntry )
                        , model.originalEntry
                            |> Maybe.map
                                (\oe ->
                                    if oe.de == model.entry.de then
                                        Cmd.none

                                    else
                                        Ports.deleteEntry ( model.session.userId, oe.de )
                                )
                            |> Maybe.withDefault Cmd.none
                        , navigateTo model.session (Just theEntry)
                        ]
                    )
                )
                WithModel
                NoOp

        DeleteEntry ->
            ( { model | dialog = Just (Dialog.YesNoDialog "Sind Sie sich da sicher?" DoDeleteEntry CloseDialog) }
            , Cmd.none
            )

        CloseDialog ->
            ( { model | dialog = Nothing }
            , Cmd.none
            )

        DoDeleteEntry ->
            model.originalEntry
                |> Maybe.map
                    (\oe ->
                        ( { model
                            | session =
                                model.session
                                    |> Session.withDict (model.session.dict |> Dictionary.without oe)
                          }
                        , Cmd.batch
                            [ Ports.deleteEntry ( model.session.userId, oe.de )
                            , navigateTo model.session Nothing
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        WordChange updatedEntry ->
            ( { model | entry = updatedEntry }
            , Cmd.none
            )

        WithModel withModel ->
            withModel model

        NoOp ->
            ( model, Cmd.none )


inputRowView : String -> (List String -> Html msg) -> Maybe String -> Html msg
inputRowView fieldName inputView maybeError =
    div [ Help.classNames [ "mb-6", "w-full" ] ]
        ([ label [ Help.classNames [ "w-full" ] ]
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
            ++ (maybeError
                    |> Maybe.map
                        (\e ->
                            [ p
                                [ Help.classNames
                                    [ "my-2"
                                    , "text-red"
                                    ]
                                ]
                                [ text e ]
                            ]
                        )
                    |> Maybe.withDefault []
               )
        )


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


navigateTo { navigationKey, globalParams } maybeEntry =
    Browser.Navigation.pushUrl navigationKey
        (maybeEntry
            |> Maybe.map (\{ de } -> AppUrl.card de globalParams)
            |> Maybe.withDefault (AppUrl.top globalParams)
            |> AppUrl.toString
        )
