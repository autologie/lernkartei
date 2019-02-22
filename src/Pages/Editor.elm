module Pages.Editor exposing (Model, Msg(..), update, view)

import Array
import Browser.Navigation
import Components.Dialog as Dialog
import Components.Icon as Icon
import Data.AppUrl as AppUrl
import Data.Dictionary as Dictionary exposing (DictValidationError(..), Dictionary)
import Data.Entry as Entry exposing (Entry, EntryValidationError(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, button, div, input, label, li, option, p, select, text, textarea, ul)
import Html.Attributes exposing (disabled, id, rows, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Ports
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


type Msg
    = CloseEditor
    | SaveAndCloseEditor
    | WordChange Entry
    | DeleteEntry
    | DoDeleteEntry
    | CloseDialog
    | WithModel (Model -> ( Model, Cmd Msg ))
    | TagClicked String
    | NoOp


type alias Model =
    { entry : Entry
    , originalEntry : Maybe Entry
    , dialog : Maybe (Dialog.Dialog Msg)
    , session : Session
    }


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
                                    if oe.index == model.entry.index then
                                        Cmd.none

                                    else
                                        Ports.deleteEntry ( model.session.userId, oe.index )
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
                            [ Ports.deleteEntry ( model.session.userId, oe.index )
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

        TagClicked tag ->
            let
                entry =
                    model.entry

                updatedEntry =
                    { entry | tags = entry.tags |> Help.toggle tag }
            in
            ( { model | entry = updatedEntry }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view { entry, originalEntry, dialog, session } =
    let
        ( isNew, dictToBe ) =
            originalEntry
                |> Maybe.map (\oe -> ( False, session.dict |> Dictionary.replacedWith oe entry ))
                |> Maybe.withDefault ( True, Dictionary.added entry session.dict )

        ( deError, jaError ) =
            case Dictionary.findFirstError dictToBe of
                Just (Duplicate index) ->
                    ( Just ("\"" ++ index ++ "\" ist shon registriert sein."), Nothing )

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

        { pos, translation, example, updatedAt, addedAt, tags } =
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
        (Help.flatten
            [ div
                [ Help.classNames
                    [ "container"
                    , "max-w-md"
                    , "p-5"
                    ]
                ]
                (Help.flatten
                    [ inputRowView "Das Wort"
                        (textInputView (Just "editor-input-de")
                            entry.index
                            False
                            (\value -> WordChange { entry | index = value })
                        )
                        deError
                        |> Help.V
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
                        |> Help.V
                    , inputRowView "Übersetzung"
                        (textInputView Nothing
                            translation
                            False
                            (\value -> WordChange { entry | translation = value })
                        )
                        jaError
                        |> Help.V
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
                        |> Help.V
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
                        |> Help.V
                    , Help.V <| tagList session.dict entry
                    , Help.O (not isNew) (\_ -> dateView session.zone session.zoneName addedAt updatedAt)
                    , Help.V <|
                        button
                            [ onClick SaveAndCloseEditor
                            , Help.classNames
                                (Help.flatten
                                    [ Help.L (Help.btnClasses True hasError)
                                    , Help.V "w-full"
                                    , Help.V "p-3"
                                    , Help.V "text-base"
                                    , Help.V "mb-2"
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
                            (Help.flatten
                                [ Help.L (Help.btnClasses True False |> List.filter ((/=) "bg-blue"))
                                , Help.V "w-full"
                                , Help.V "p-3"
                                , Help.V "text-base"
                                , Help.V "bg-red"
                                ]
                            )
                        , disabled hasError
                        ]
                        [ text "Löschen" ]
                        |> Help.V
                    , button
                        [ onClick CloseEditor
                        , Help.classNames
                            [ "fixed"
                            , "pin-r"
                            , "pin-t"
                            , "m-4"
                            ]
                        ]
                        [ Icon.close "width: 2em; height: 2em" "#3d4852" ]
                        |> Help.V
                    ]
                )
                |> Help.V
            , Help.M <| (dialog |> Maybe.map (\d -> Dialog.view d))
            ]
        )


dateView zone zoneName addedAt updatedAt =
    let
        addedAtExpr =
            describeDate zone zoneName addedAt

        updatedAtExpr =
            describeDate zone zoneName updatedAt
    in
    p
        [ Help.classNames
            [ "text-grey-dark"
            , "my-6"
            ]
        ]
        [ text
            ([ Help.V "Added on"
             , Help.V addedAtExpr
             , Help.O (addedAtExpr /= updatedAtExpr) (\_ -> ", updated on " ++ updatedAtExpr)
             ]
                |> Help.flatten
                |> String.join " "
            )
        ]


inputRowView : String -> (List String -> Html msg) -> Maybe String -> Html msg
inputRowView fieldName inputView maybeError =
    div [ Help.classNames [ "mb-6", "w-full" ] ]
        (Help.flatten
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
                |> Help.V
            , Help.M <| (maybeError |> Maybe.map inputRowErrorView)
            ]
        )


inputRowErrorView e =
    p
        [ Help.classNames
            [ "my-2"
            , "text-red"
            ]
        ]
        [ text e ]


textInputView : Maybe String -> String -> Bool -> (String -> Msg) -> List String -> Html Msg
textInputView maybeInputId inputValue multiline handleInput formClasses =
    if multiline then
        textarea
            [ Help.classNames
                (Help.flatten
                    [ Help.V "text-sm"
                    , Help.V "leading-normal"
                    , Help.V "resize-none"
                    , Help.L formClasses
                    ]
                )
            , rows 5
            , value inputValue
            , onInput handleInput
            ]
            []

    else
        input
            (Help.flatten
                [ Help.V <| type_ "text"
                , Help.V <| Help.classNames (Help.flatten [ Help.V "text-base", Help.L formClasses ])
                , Help.V <| value inputValue
                , Help.V <| onInput handleInput
                , Help.G maybeInputId (\value -> [ id value ])
                ]
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


describeDate : Zone -> ZoneName -> Posix -> String
describeDate zone zoneName posix =
    if Time.posixToMillis posix == 0 then
        "(Unknown)"

    else
        [ posix |> Time.toDay zone |> String.fromInt
        , "."
        , Help.monthNumber posix zone |> String.fromInt
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


navigateTo : Session -> Maybe Entry -> Cmd Msg
navigateTo { navigationKey, globalParams } maybeEntry =
    Browser.Navigation.pushUrl navigationKey
        (maybeEntry
            |> Maybe.map (\{ index } -> AppUrl.card index globalParams)
            |> Maybe.withDefault (AppUrl.top globalParams)
            |> AppUrl.toString
        )


tagList : Dictionary -> Entry -> Html Msg
tagList dict entry =
    ul
        [ Help.classNames
            [ "list-reset"
            , "flex"
            , "flex-wrap"
            , "justify-center"
            ]
        ]
        (dict
            |> Dictionary.tags
            |> List.map
                (\tag ->
                    let
                        isMember =
                            List.member tag entry.tags
                    in
                    li
                        [ Help.classNames
                            (Help.flatten
                                [ Help.V "mb-2"
                                , Help.V "mx-1"
                                , Help.V "p-2"
                                , Help.V "rounded"
                                , Help.B isMember (\_ -> "bg-blue") (\_ -> "bg-grey-light")
                                , Help.B isMember (\_ -> "text-white") (\_ -> "text-grey-darkest")
                                ]
                            )
                        , onClick (TagClicked tag)
                        ]
                        [ text tag ]
                )
        )
