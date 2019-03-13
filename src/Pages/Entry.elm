module Pages.Entry exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Array
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation
import Components.Button as Button
import Components.Icon as Icon
import Data.AppUrl as AppUrl exposing (AppUrl)
import Data.Dictionary as Dictionary
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, li, p, text, ul)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Decode
import Ports
import Task
import Templates.Entry


type alias Model =
    { entry : Entry
    , textElementSize : Maybe ( Float, Float )
    , textWrapperElementSize : Maybe ( Float, Float )
    , session : Session
    }


type Msg
    = TextElementMeasured (Result Dom.Error Dom.Element)
    | TextWrapperElementMeasured (Result Dom.Error Dom.Element)
    | ToggleStar
    | NavigateTo AppUrl
    | CopyToClipboard
    | WindowResized
    | RequestArchive
    | NoOp


init : Session -> Entry -> ( Model, Cmd Msg )
init session entry =
    ( { entry = entry
      , textElementSize = Nothing
      , textWrapperElementSize = Nothing
      , session = session
      }
    , requestElementSize
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (decodeKeyDownEvent model.session)
        , Browser.Events.onResize (\_ _ -> WindowResized)
        ]


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        TextWrapperElementMeasured (Ok { element }) ->
            ( { model
                | textWrapperElementSize = Just ( element.width, element.height )
              }
            , Cmd.none
            )

        TextWrapperElementMeasured _ ->
            -- ignore
            ( model, Cmd.none )

        TextElementMeasured (Ok { element }) ->
            ( { model
                | textElementSize = Just ( element.width, element.height )
              }
            , Cmd.none
            )

        TextElementMeasured _ ->
            -- ignore
            ( model, Cmd.none )

        ToggleStar ->
            let
                entry =
                    model.entry

                updatedEntry =
                    { entry | starred = not entry.starred }

                updatedDict =
                    model.session.dict
                        |> Dictionary.replacedWith entry updatedEntry
            in
            ( { model
                | entry = updatedEntry
                , session = model.session |> Session.withDict updatedDict
              }
            , Ports.saveEntry ( model.session.userId, Entry.encode updatedEntry )
            )

        NavigateTo url ->
            ( model
            , Browser.Navigation.pushUrl model.session.navigationKey
                (AppUrl.toString url)
            )

        CopyToClipboard ->
            ( model, Ports.copyToClipboard model.entry.index )

        WindowResized ->
            ( { model
                | textElementSize = Nothing
                , textWrapperElementSize = Nothing
              }
            , requestElementSize
            )

        RequestArchive ->
            let
                url =
                    AppUrl.nextEntry model.session.globalParams
            in
            ( { model
                | session =
                    model.session
                        |> Session.withDict (model.session.dict |> Dictionary.without model.entry)
              }
            , Cmd.batch
                [ Ports.archiveEntry ( model.session.userId, Entry.encode model.entry )
                , Browser.Navigation.pushUrl model.session.navigationKey (AppUrl.toString url)
                ]
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Templates.Entry.layout
        { session = model.session
        , results =
            Filter.applied
                model.session.startTime
                model.session.dict
                model.session.globalParams.filters
        , entry = model.entry
        , cardContent = cardContentView model
        , partOfSpeech = [ p [] [ text (PartOfSpeech.toString model.entry.pos) ] ]
        , example = [ exampleView model ]
        , tags =
            if List.length model.entry.tags > 0 then
                [ tagsView model ]

            else
                [ p [] [ text "-" ] ]
        , extraContent = buttons model.session model.entry
        , actions = actionsView
        , onNavigationRequested = NavigateTo
        , onCopyToClipboardClicked = CopyToClipboard
        , onEditButtonClicked = NavigateTo (AppUrl.editEntry model.entry.index model.session.globalParams)
        , buttons =
            { imageSearchResults = Templates.Entry.Enabled
            , wiktionary = Templates.Entry.Enabled
            , googleTranslation = Templates.Entry.Enabled
            , edit = Templates.Entry.Enabled
            , copyToClipboard = Templates.Entry.Enabled
            , prevLink =
                if List.length model.session.progress.shown > 1 then
                    Templates.Entry.Enabled

                else
                    Templates.Entry.Disabled
            , nextLink =
                if List.length model.session.progress.toBeShown > 0 then
                    Templates.Entry.Enabled

                else
                    Templates.Entry.Disabled
            }
        }


actionsView : Html Msg
actionsView =
    div
        []
        [ button
            [ onClick RequestArchive
            , class "w-full p-3 text-sm mb-2 bg-orange rounded-full text-white"
            ]
            [ text "Archive" ]
        ]


computeTextDisposition : ( Float, Float ) -> ( Float, Float ) -> ( Int, Int, Float )
computeTextDisposition ( textWidth, textHeight ) ( wrapperWidth, wrapperHeight ) =
    ( round ((wrapperWidth - textWidth) / 2)
    , round ((wrapperHeight - textHeight) / 2)
    , min (0.6 * wrapperWidth / textWidth) (0.4 * wrapperHeight / textHeight)
    )


starView : Bool -> Html Msg
starView starred =
    let
        starViewTemplate extraStyles extraClasses txt =
            div
                [ class "absolute pin-t pin-l m-2" ]
                [ button
                    -- TODO: handle event at the capture phase to avoid navigation
                    -- https://package.elm-lang.org/packages/elm/virtual-dom/latest/VirtualDom#on
                    ([ stopPropagationOn "click" (Decode.map (\msg -> ( msg, True )) (Decode.succeed ToggleStar))
                     , class ("text-lg 1text-black " ++ extraClasses)
                     ]
                        ++ (extraStyles |> List.map (\( k, v ) -> style k v))
                    )
                    [ text txt ]
                ]
    in
    if starred then
        starViewTemplate [ ( "text-shadow", "0 0 .4em rgba(0,0,0,.1)" ) ] "text-orange" "★"

    else
        starViewTemplate [] "text-grey" "☆"


tagsView : Model -> Html Msg
tagsView model =
    ul [ class "list-reset" ]
        (model.entry.tags
            |> List.map
                (\tag ->
                    li
                        [ class "bg-grey-light text-grey-darker px-2 py-1 mr-2 mb-1 inline-block rounded text-xs"
                        , onClick (NavigateTo (AppUrl.nextEntry model.session.globalParams |> AppUrl.withFilters [ HasTag tag ]))
                        ]
                        [ text tag ]
                )
        )


exampleView : Model -> Html Msg
exampleView model =
    p [ class "whitespace-pre-wrap" ]
        [ text
            (model.entry.example
                |> Maybe.map Entry.censorExample
                |> Maybe.withDefault "-"
            )
        ]


cardContentView : Model -> String -> Html Msg
cardContentView ({ entry, session } as model) defaultClasses =
    a
        [ id "text-wrapper"
        , class defaultClasses
        , href
            (AppUrl.entry entry.index session.globalParams
                |> AppUrl.withTranslate (not session.globalParams.translate)
                |> AppUrl.toString
            )
        ]
        [ cardTextView model
        , starView entry.starred
        ]


cardTextView : Model -> Html Msg
cardTextView model =
    let
        textToShow =
            if model.session.globalParams.translate then
                model.entry.translation

            else
                model.entry.index

        textDisposition =
            Maybe.map2
                computeTextDisposition
                model.textElementSize
                model.textWrapperElementSize
    in
    div
        ([ id "text"
         , attribute "data-text" textToShow
         ]
            ++ (case textDisposition of
                    Just ( x, y, scale ) ->
                        [ class "absolute inline-block"
                        , style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                        , style "left" (String.fromInt x)
                        , style "top" (String.fromInt y)
                        ]

                    Nothing ->
                        [ class "inline-block text-transparent" ]
               )
        )
        [ text textToShow ]


buttons : Session -> Entry -> Html msg
buttons session entry =
    Button.floatingGroup
        [ a
            [ class "rounded-full text-xl shadow-md flex w-8 h-8 justify-center items-center mb-2"
            , class
                (if session.globalParams.shuffle then
                    "bg-green"

                 else
                    "bg-grey"
                )
            , href
                (AppUrl.entry entry.index session.globalParams
                    |> AppUrl.withShuffle (not session.globalParams.shuffle)
                    |> AppUrl.toString
                )
            ]
            [ Icon.shuffle "width: .4em; height: .4em;"
                (if session.globalParams.shuffle then
                    "#38c172"

                 else
                    "#b8c2cc"
                )
            ]
        , Button.addNewEntry (AppUrl.createEntry Nothing session.globalParams)
        ]


decodeKeyDownEvent : Session -> Decode.Decoder Msg
decodeKeyDownEvent session =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case key of
                    "ArrowRight" ->
                        NavigateTo (AppUrl.nextEntry session.globalParams)

                    "ArrowLeft" ->
                        NavigateTo (AppUrl.prevEntry session.globalParams)

                    _ ->
                        NoOp
            )


requestElementSize : Cmd Msg
requestElementSize =
    Cmd.batch
        [ Dom.getElement "text" |> Task.attempt TextElementMeasured
        , Dom.getElement "text-wrapper" |> Task.attempt TextWrapperElementMeasured
        ]
