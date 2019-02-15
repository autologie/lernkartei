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
    | BackToPrevPage


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

                updatedDict =
                    model.session.dict
                        |> Array.map (Help.replaceEntry entry updatedEntry)
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

        BackToPrevPage ->
            ( model, Browser.Navigation.back model.session.navigationKey 1 )


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
            ]
        ]
        [ ( "search"
          , SearchField.view results
                (Filter.toString filters)
                filters
                False
                |> Html.map (translateSearchFieldMsg model)
          )
        , ( "card", cardView model results )
        , ( "buttons", buttons model.entry model.session.globalParams )
        ]


cardView : Model -> Dictionary -> Html Msg
cardView model results =
    let
        hasNext =
            Array.length results > 1
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
            (Help.flatten
                [ Help.V <| cardBehindView 1.6 5 -1
                , Help.V <| cardBehindView 2 10 -2
                , Help.V <| cardBodyView model.entry model.session.globalParams results model.textDisposition
                , Help.O hasNext <| \_ -> nextButton model.session.globalParams
                , Help.V <| prevButton
                ]
            )
        , entryDetailView model.session.globalParams model.entry
        ]


nextButton globalParams =
    a
        [ Help.classNames
            [ "absolute"
            , "rounded-full"
            , "bg-blue"
            , "pin-r"
            , "pin-t"
            , "shadow-md"
            ]
        , style "margin" "7rem -1em 0 0"
        , href (AppUrl.nextCard globalParams |> AppUrl.toString)
        ]
        [ Icon.next "width: 3em; height: 3em" ]


prevButton =
    a
        [ Help.classNames
            [ "absolute"
            , "rounded-full"
            , "bg-blue"
            , "pin-l"
            , "pin-t"
            , "shadow-md"
            ]
        , style "margin" "7rem 0 0 -1em"
        , onClick BackToPrevPage
        ]
        [ Icon.prev "width: 3em; height: 3em" ]


cardBodyView entry globalParams results textDisposition =
    let
        textToShow =
            if globalParams.translate then
                entry.translation

            else
                entry.index
    in
    div
        [ Help.classNames
            [ "select-none"
            , "h-64"
            , "text-grey-darkest"
            , "relative"
            ]
        , onClick
            (NavigateTo
                (AppUrl.card entry.index globalParams
                    |> AppUrl.withTranslate (not globalParams.translate)
                )
            )
        ]
        [ div
            ([ id "text"
             , attribute "data-text" textToShow
             ]
                ++ (case textDisposition of
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
        , starView entry.starred
        , linksView entry globalParams globalParams.filters
        ]


starView : Bool -> Html Msg
starView starred =
    div
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
                    ++ (if starred then
                            [ "text-orange" ]

                        else
                            [ "text-grey" ]
                       )
                )
            , if starred then
                style "text-shadow" "0 0 .4em rgba(0,0,0,.1)"

              else
                style "" ""
            ]
            [ text
                (if starred then
                    "★"

                 else
                    "☆︎"
                )
            ]
        ]


linksView : Entry -> GlobalQueryParams -> List Filter -> Html Msg
linksView entry globalParams appliedFilters =
    let
        simpleDe =
            Entry.withoutArticle entry
    in
    div
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
                (AppUrl.editorFor entry.index globalParams
                    |> AppUrl.withFilters appliedFilters
                    |> AppUrl.toString
                )
            , Help.classNames [ "text-blue", "no-underline" ]
            ]
            [ text "Edit" ]
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


entryDetailView : GlobalQueryParams -> Entry -> Html Msg
entryDetailView globalParams { pos, example, tags } =
    div
        [ Help.classNames
            [ "text-grey-light"
            , "leading-normal"
            , "text-left"
            , "rounded"
            , "bg-grey-darkest"
            , "shadow-md"
            , "p-3"
            ]
        ]
        [ section [ Help.classNames [ "mb-8" ] ]
            [ h3 [ Help.classNames [ "my-4", "text-xs" ] ] [ text "Teil" ]
            , p [] [ text (PartOfSpeech.toString pos) ]
            ]
        , section [ Help.classNames [ "mb-8" ] ]
            [ h3 [ Help.classNames [ "my-4", "text-xs" ] ] [ text "Beispiel" ]
            , p [ Help.classNames [ "whitespace-pre-wrap" ] ]
                [ text
                    (example
                        |> Maybe.map Entry.censorExample
                        |> Maybe.withDefault "-"
                    )
                ]
            ]
        , section [ Help.classNames [ "mb-8" ] ]
            [ h3 [ Help.classNames [ "my-4", "text-xs" ] ] [ text "Etikett" ]
            , if List.length tags > 0 then
                ul [ Help.classNames [ "list-reset" ] ]
                    (tags
                        |> List.map
                            (\tag ->
                                li
                                    [ Help.classNames
                                        [ "bg-grey-light"
                                        , "text-grey-darker"
                                        , "px-2"
                                        , "py-1"
                                        , "mr-2"
                                        , "mb-1"
                                        , "inline-block"
                                        , "rounded"
                                        , "text-xs"
                                        ]
                                    , onClick (NavigateTo (AppUrl.nextCard globalParams |> AppUrl.withFilters [ HasTag tag ]))
                                    ]
                                    [ text tag ]
                            )
                    )

              else
                p [] [ text "-" ]
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
            [ Icon.shuffle "width: .4em; height: .4em;"
                (if globalParams.shuffle then
                    "#38c172"

                 else
                    "#b8c2cc"
                )
            ]
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
            AppUrl.entries model.session.globalParams
    )
        |> NavigateTo
