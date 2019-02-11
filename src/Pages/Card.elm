module Pages.Card exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import Array
import Browser.Navigation
import Components.Icon as Icon exposing (add)
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl, GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Session)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode as Decode
import Ports


type alias Model =
    { entry : Entry
    , textDisposition : Maybe ( Int, Int, Float )
    , session : Session
    }


type Msg
    = TextDispositionChange ( Int, Int, Float )
    | ToggleStar
    | NavigateTo AppUrl


initialModel : Session -> String -> Model
initialModel session entryDe =
    { entry = Dictionary.get entryDe session.dict
    , textDisposition = Nothing
    , session = session
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


update : Model -> Msg -> ( Model, Cmd Msg )
update model msg =
    case msg of
        TextDispositionChange value ->
            ( { model
                | textDisposition = Just value
              }
            , Cmd.none
            )

        ToggleStar ->
            let
                entry =
                    model.entry

                updatedEntry =
                    { entry | starred = not entry.starred }
            in
            ( { model
                | entry = updatedEntry
                , session = model.session |> Session.withDict (model.session.dict |> Array.map (Help.replaceEntry entry updatedEntry))
              }
            , Ports.saveEntry ( model.session.userId, Entry.encode updatedEntry )
            )

        NavigateTo url ->
            ( model, Browser.Navigation.pushUrl model.session.navigationKey (AppUrl.toString url) )


view : Model -> Html Msg
view model =
    let
        filters =
            model.session.globalParams.filters

        results =
            Filter.applied
                model.session.startTime
                model.session.dict
                filters
    in
    Html.Keyed.node "div"
        [ Help.classNames
            [ "container"
            , "max-w-md"
            , "p-5"
            , "pt-12"
            ]
        ]
        [ ( "nav"
          , SearchField.view results
                (Filter.toString filters)
                filters
                |> Html.map (translateSearchFieldMsg model)
          )
        , ( "card", cardView model results model.entry )
        , ( "buttons", buttons model.entry model.session.globalParams )
        ]


cardView : Model -> Dictionary -> Entry -> Html Msg
cardView model results entry =
    let
        textToShow =
            if model.session.globalParams.translate then
                entry.translation

            else
                entry.index

        simpleDe =
            Entry.withoutArticle entry

        hasNext =
            Array.length results > 1

        searchText =
            model.session.globalParams.filters
    in
    div []
        [ div
            [ Help.classNames
                [ "rounded"
                , "bg-white"
                , "shadow-lg"
                , "relative"
                , "mt-4"
                , "mb-8"
                ]
            ]
            [ cardBehindView 1.6 5 -1
            , cardBehindView 2 10 -2
            , div
                [ Help.classNames
                    [ "select-none"
                    , "h-64"
                    , "text-grey-darkest"
                    , "relative"
                    ]
                , onClick
                    (NavigateTo
                        (AppUrl.card model.entry.index model.session.globalParams
                            |> AppUrl.withTranslate (not model.session.globalParams.translate)
                        )
                    )
                ]
                [ div
                    ([ id "text", attribute "data-text" textToShow ]
                        ++ (case model.textDisposition of
                                Just ( x, y, scale ) ->
                                    [ Help.classNames
                                        [ "absolute"
                                        , "inline-block"
                                        ]
                                    , style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                                    , style "left" (String.fromInt x)
                                    , style "top" (String.fromInt y)
                                    ]

                                Nothing ->
                                    [ Help.classNames [ "inline-block text-transparent" ] ]
                           )
                    )
                    [ text textToShow ]
                , div
                    [ Help.classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-l"
                        , "m-2"
                        ]
                    ]
                    [ button
                        [ stopPropagationOn "click" (Decode.map (\msg -> ( msg, True )) (Decode.succeed ToggleStar))
                        , Help.classNames
                            ([ "text-lg"
                             , "text-black"
                             ]
                                ++ (if entry.starred then
                                        [ "text-orange" ]

                                    else
                                        [ "text-grey" ]
                                   )
                            )
                        , if entry.starred then
                            style "text-shadow" "0 0 .4em rgba(0,0,0,.1)"

                          else
                            style "" ""
                        ]
                        [ text
                            (if entry.starred then
                                "★"

                             else
                                "☆︎"
                            )
                        ]
                    ]
                , div
                    [ Help.classNames
                        [ "absolute"
                        , "pin-t"
                        , "pin-r"
                        , "m-2"
                        ]
                    ]
                    [ a
                        [ href ("https://www.google.com/search?q=" ++ simpleDe ++ "&tbm=isch")
                        , target "_blank"
                        , Help.classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Bilder" ]
                    , a
                        [ href ("https://de.wiktionary.org/wiki/" ++ simpleDe)
                        , target "_blank"
                        , Help.classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Untersuchen" ]
                    , a
                        [ href ("https://translate.google.co.jp/m/translate?hl=translation#view=home&op=translate&sl=de&tl=translation&text=" ++ simpleDe)
                        , target "_blank"
                        , Help.classNames [ "text-blue", "no-underline", "mr-2" ]
                        ]
                        [ text "Hören" ]
                    , a
                        [ href
                            (AppUrl.editorFor entry.index model.session.globalParams
                                |> AppUrl.withFilters searchText
                                |> AppUrl.toString
                            )
                        , Help.classNames [ "text-blue", "no-underline" ]
                        ]
                        [ text "Edit" ]
                    ]
                ]
            ]
        , a
            [ Help.classNames
                (Help.btnClasses True (not hasNext)
                    ++ [ "my-5"
                       , "p-4"
                       , "text-lg"
                       , "w-full"
                       ]
                )
            , href (AppUrl.nextCard model.session.globalParams |> AppUrl.toString)
            ]
            [ text "Nächst" ]
        , entryDetailView entry
        ]


cardBehindView : Float -> Float -> Int -> Html Msg
cardBehindView rotateValue y zIndex =
    div
        [ Help.classNames
            [ "absolute"
            , "w-full"
            , "h-full"
            , "shadow"
            , "bg-white"
            , "rounded"
            ]
        , style "transform" ("rotate(" ++ String.fromFloat rotateValue ++ "deg) translateY(" ++ String.fromFloat y ++ "px)")
        , style "z-index" (String.fromInt zIndex)
        ]
        []


entryDetailView : Entry -> Html Msg
entryDetailView { pos, example, tags } =
    div
        [ Help.classNames
            [ "text-grey-dark"
            , "py-4"
            , "leading-normal"
            , "text-left"
            , "rounded-b"
            ]
        ]
        [ section [ Help.classNames [ "mb-6" ] ]
            [ h3 [] [ text "Teil" ]
            , p [] [ text (PartOfSpeech.toString pos) ]
            ]
        , section [ Help.classNames [ "mb-6" ] ]
            [ h3 [] [ text "Beispiel" ]
            , p [ Help.classNames [ "whitespace-prewrap" ] ]
                [ text
                    (example
                        |> Maybe.map Entry.censorExample
                        |> Maybe.withDefault "-"
                    )
                ]
            ]
        , section [ Help.classNames [ "mb-6" ] ]
            [ h3 [] [ text "Etikett" ]
            , p []
                [ text
                    (if List.length tags > 0 then
                        tags |> String.join ", "

                     else
                        "-"
                    )
                ]
            ]
        ]


buttons : Entry -> GlobalQueryParams -> Html Msg
buttons entry globalParams =
    div
        [ Help.classNames
            [ "fixed"
            , "pin-r"
            , "pin-b"
            , "m-4"
            , "z-30"
            , "flex"
            , "justify-center"
            , "items-center"
            , "flex-col"
            ]
        ]
        [ a
            [ Help.classNames
                [ "rounded-full"
                , if globalParams.shuffle then
                    "bg-green"

                  else
                    "bg-grey"
                , "text-xl"
                , "shadow-md"
                , "flex"
                , "w-8"
                , "h-8"
                , "justify-center"
                , "items-center"
                , "mb-2"
                ]
            , href
                (AppUrl.card entry.index globalParams
                    |> AppUrl.withShuffle (not globalParams.shuffle)
                    |> AppUrl.toString
                )
            ]
            [ Icon.shuffle "width: .4em; height: .4em;" ]
        , a
            [ Help.classNames
                [ "rounded-full"
                , "bg-green"
                , "text-white"
                , "text-xl"
                , "p-2"
                , "w-16"
                , "h-16"
                , "flex"
                , "justify-center"
                , "items-center"
                , "shadow-lg"
                , "no-underline"
                ]
            , href (AppUrl.newEntry Nothing globalParams |> AppUrl.toString)
            ]
            [ Icon.add "width: .6em; height: .6em;" ]
        ]


translateSearchFieldMsg : Model -> SearchField.Msg -> Msg
translateSearchFieldMsg model msg =
    let
        params =
            model.session.globalParams
    in
    (case msg of
        SearchField.ClearSearchText ->
            AppUrl.card model.entry.index { params | filters = [] }

        SearchField.Focus ->
            AppUrl.search params

        SearchField.SearchInput txt ->
            AppUrl.search { params | filters = Filter.parse txt }

        SearchField.ToggleSearchResults ->
            AppUrl.search model.session.globalParams
    )
        |> NavigateTo
