module Pages.Editor exposing (Model, Msg(..), initCreate, initEdit, subscriptions, update, view)

import Array
import Browser.Dom
import Browser.Navigation
import Components.Tag as Tag
import Data.AppUrl as AppUrl exposing (AppUrl)
import Data.Dictionary as Dictionary exposing (DictValidationError(..), Dictionary)
import Data.Entry as Entry exposing (Entry, EntryValidationError(..))
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, button, div, h3, input, li, option, p, select, text, textarea, ul)
import Html.Attributes exposing (class, disabled, id, rows, selected, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Ports
import Task
import Templates.Entry
import Time exposing (Posix)


type Msg
    = CloseEditor
    | SaveAndCloseEditor
    | WordChange Entry
    | DeleteEntry
    | DoDeleteEntry
    | WithModel (Model -> ( Model, Cmd Msg ))
    | TagClicked String
    | CopyToClipboard
    | NavigateTo AppUrl
    | ConfirmDialogResponded Bool
    | NoOp


type alias Model =
    { entry : Entry
    , originalEntry : Maybe Entry
    , confirmDialogCallback : Maybe { onYes : Msg, onNo : Msg }
    , session : Session
    }


initCreate : Maybe String -> Session -> ( Model, Cmd Msg )
initCreate index session =
    let
        emptyEntry =
            Entry.empty
    in
    ( { entry = { emptyEntry | index = Maybe.withDefault "" index }
      , originalEntry = Nothing
      , confirmDialogCallback = Nothing
      , session = session
      }
    , Browser.Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)
    )


initEdit : Entry -> Session -> ( Model, Cmd Msg )
initEdit entry session =
    ( { entry = entry
      , originalEntry = Just entry
      , confirmDialogCallback = Nothing
      , session = session
      }
    , Browser.Dom.focus "editor-input-de" |> Task.attempt (\_ -> NoOp)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    model.confirmDialogCallback
        |> Maybe.map (\_ -> Ports.confirmDialogResponded ConfirmDialogResponded)
        |> Maybe.withDefault Sub.none


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        CloseEditor ->
            ( model
            , Browser.Navigation.back model.session.navigationKey 1
            )

        SaveAndCloseEditor ->
            Help.updateWithCurrentTime model
                (\now currentModel ->
                    currentModel.originalEntry
                        |> Maybe.map (saveExistingEntryStep now currentModel)
                        |> Maybe.withDefault (saveNewEntryStep now currentModel)
                )
                WithModel
                NoOp

        DeleteEntry ->
            ( { model
                | confirmDialogCallback =
                    Just
                        { onNo = NoOp
                        , onYes = DoDeleteEntry
                        }
              }
            , Ports.showConfirmDialog "Sind Sie sich da sicher?"
            )

        DoDeleteEntry ->
            model.originalEntry
                |> Maybe.map (deleteExistingEntryStep model)
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

        CopyToClipboard ->
            ( model, Ports.copyToClipboard model.entry.index )

        NavigateTo url ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.toString url)
            )

        ConfirmDialogResponded yesNo ->
            model.confirmDialogCallback
                |> Maybe.map
                    (\{ onYes, onNo } ->
                        update
                            { model | confirmDialogCallback = Nothing }
                            (if yesNo then
                                onYes

                             else
                                onNo
                            )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view ({ entry, session } as model) =
    let
        ( isNew, dictToBe ) =
            model.originalEntry
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

        linkState =
            if Entry.withoutArticle entry == "" then
                Templates.Entry.Disabled

            else
                Templates.Entry.Enabled

        { tags } =
            entry
    in
    Templates.Entry.layout
        { session = session
        , results =
            Filter.applied
                model.session.startTime
                model.session.dict
                model.session.globalParams.filters
        , entry = model.entry
        , cardContent = cardContentView model deError jaError
        , partOfSpeech =
            [ selectInputView
                (model.entry.pos |> PartOfSpeech.toString)
                (applyPartOfSpeech model.entry.pos model.entry)
                (PartOfSpeech.items
                    |> List.map PartOfSpeech.toString
                    |> List.map (\v -> ( v, v ))
                )
            ]
        , example =
            [ textInputView Nothing
                (model.entry.example |> Maybe.withDefault "")
                True
                (applyExample model.entry)
                "shadow-md"
            ]
        , tags =
            [ textInputView Nothing
                (tags |> String.join "\n")
                True
                (applyTags entry)
                "shadow-md mb-2"
            , tagList session.dict entry
            ]
        , extraContent = div [] []
        , actions = actionsView hasError isNew
        , onNavigationRequested = NavigateTo
        , onBackLinkClicked = NoOp
        , onCopyToClipboardClicked = CopyToClipboard
        , onEditButtonClicked = CloseEditor
        , buttons =
            { imageSearchResults = linkState
            , wiktionary = linkState
            , googleTranslation = linkState
            , edit = Templates.Entry.Enabled
            , copyToClipboard = linkState
            , prevLink = Templates.Entry.Hidden
            , nextLink = Templates.Entry.Hidden
            }
        }


cardContentView : Model -> Maybe String -> Maybe String -> String -> Html Msg
cardContentView { entry } deError jaError defaultClasses =
    let
        extraClasses =
            "bg-grey-lighter p-3 text-lg"
    in
    div [ class (defaultClasses ++ " flex justify-around flex-col p-4 pb-8") ]
        (Help.flatten
            [ Help.V <| h3 [ class "text-xs p-2 text-grey" ] [ text "Das Wort" ]
            , Help.V <|
                textInputView (Just "editor-input-de")
                    entry.index
                    False
                    (\value -> WordChange { entry | index = value })
                    extraClasses
            , Help.M (deError |> Maybe.map inputRowErrorView)
            , Help.V <| h3 [ class "text-xs p-2 text-grey" ] [ text "Übersetzung" ]
            , Help.V <|
                textInputView Nothing
                    entry.translation
                    False
                    (\value -> WordChange { entry | translation = value })
                    extraClasses
            , Help.M (jaError |> Maybe.map inputRowErrorView)
            ]
        )


actionsView : Bool -> Bool -> Html Msg
actionsView hasError isNew =
    div
        [ class "pt-12 flex" ]
        (Help.flatten
            [ Help.O (not isNew)
                (\_ ->
                    button
                        [ onClick DeleteEntry
                        , class "mr-2 w-full p-3 text-sm mb-2 bg-red rounded-full text-white"
                        ]
                        [ text "Löschen" ]
                )
            , Help.V <|
                button
                    (Help.flatten
                        [ Help.V <| onClick SaveAndCloseEditor
                        , Help.V <| class "w-full p-3 text-sm mb-2 bg-green rounded-full text-white"
                        , Help.O (not isNew) (\_ -> class "ml-2")
                        , Help.O hasError (\_ -> class "opacity-50")
                        , Help.V <| disabled hasError
                        ]
                    )
                    [ text "Hinzufügen" ]
            ]
        )


inputRowErrorView : String -> Html msg
inputRowErrorView e =
    p
        [ Help.classNames
            [ "my-2"
            , "text-red"
            ]
        ]
        [ text e ]


textInputView : Maybe String -> String -> Bool -> (String -> Msg) -> String -> Html Msg
textInputView maybeInputId inputValue multiline handleInput extraClasses =
    if multiline then
        textarea
            [ class ("text-sm leading-normal resize-none w-full rounded p-2 " ++ extraClasses)
            , rows 5
            , value inputValue
            , onInput handleInput
            ]
            []

    else
        input
            (Help.flatten
                [ Help.V <| type_ "text"
                , Help.V <| class ("text-base w-full p-2 rounded " ++ extraClasses)
                , Help.V <| value inputValue
                , Help.V <| onInput handleInput
                , Help.G maybeInputId (\value -> [ id value ])
                ]
            )
            []


selectInputView : String -> (String -> a) -> List ( String, String ) -> Html a
selectInputView inputValue handleInput options =
    select
        [ Help.classNames [ "text-base w-full p-2 bg-white rounded shadow-md" ]
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


navigateTo : Session -> Maybe Entry -> Cmd Msg
navigateTo { navigationKey, globalParams } maybeEntry =
    Browser.Navigation.pushUrl navigationKey
        (maybeEntry
            |> Maybe.map (\{ index } -> AppUrl.entry index globalParams)
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
                    li [] [ Tag.view tag isMember (TagClicked tag) ]
                )
        )


applyPartOfSpeech : PartOfSpeech.PartOfSpeech -> Entry -> String -> Msg
applyPartOfSpeech pos entry value =
    WordChange
        { entry
            | pos =
                value
                    |> PartOfSpeech.fromString
                    |> Result.withDefault pos
        }


applyExample : Entry -> String -> Msg
applyExample entry value =
    WordChange
        { entry
            | example =
                if value == "" then
                    Nothing

                else
                    Just value
        }


applyTags : Entry -> String -> Msg
applyTags entry value =
    WordChange
        { entry
            | tags =
                value
                    |> String.split "\n"
                    |> List.map String.trim
                    |> List.filter ((/=) "")
        }


saveExistingEntryStep : Posix -> Model -> Entry -> ( Model, Cmd Msg )
saveExistingEntryStep now ({ entry, session } as model) oe =
    let
        theEntry =
            { entry | updatedAt = now }
    in
    ( { model
        | session =
            session
                |> Session.withDict (session.dict |> Array.map (Help.replaceEntry oe theEntry))
      }
    , Cmd.batch
        [ Ports.saveEntry ( session.userId, Entry.encode theEntry )
        , if oe.index == entry.index then
            Cmd.none

          else
            Ports.deleteEntry ( session.userId, oe.index )
        , navigateTo session (Just theEntry)
        ]
    )


saveNewEntryStep : Posix -> Model -> ( Model, Cmd Msg )
saveNewEntryStep now ({ entry, session } as model) =
    let
        theEntry =
            { entry | updatedAt = now, addedAt = now }
    in
    ( { model
        | session =
            session
                |> Session.withDict
                    (session.dict |> Array.append (Array.fromList [ theEntry ]))
      }
    , Cmd.batch
        [ Ports.saveEntry ( session.userId, Entry.encode theEntry )
        , navigateTo session (Just theEntry)
        ]
    )


deleteExistingEntryStep : Model -> Entry -> ( Model, Cmd Msg )
deleteExistingEntryStep model oe =
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
