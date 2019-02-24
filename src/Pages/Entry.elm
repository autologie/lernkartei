module Pages.Entry exposing
    ( Model
    , Msg(..)
    , initialModel
    , subscriptions
    , update
    , view
    )

import Array
import Browser.Dom as Dom
import Browser.Navigation
import Components.Button as Button
import Components.Icon as Icon exposing (add)
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl, GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.Google as Google
import Data.PartOfSpeech as PartOfSpeech
import Data.Session as Session exposing (Language, Session)
import Help
import Html exposing (Html, a, button, div, h3, input, li, p, section, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, style, target, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Html.Keyed
import Json.Decode as Decode
import Ports


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
    | BackToPrevPage
    | CopyToClipboard


initialModel : Session -> Entry -> Model
initialModel session entry =
    { entry = entry
    , textElementSize = Nothing
    , textWrapperElementSize = Nothing
    , session = session
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


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

        CopyToClipboard ->
            ( model, Ports.copyToClipboard model.entry.index )


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
        [ class "container max-w-md p-5 pb-0" ]
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
            [ class "rounded bg-white shadow-lg relative mt-4 mb-8" ]
            (Help.flatten
                [ Help.V <| cardBodyView model results
                , Help.O hasNext <| \_ -> nextButton model.session.globalParams
                , Help.V <| prevButton
                ]
            )
        , linksView model.entry model.session.globalParams model.session.language
        , entryDetailView model.session.globalParams model.entry
        ]


nextButton : GlobalQueryParams -> Html Msg
nextButton globalParams =
    a
        [ class "absolute rounded-full bg-blue pin-r pin-t shadow-md"
        , style "margin" "6.4rem -1em 0 0"
        , href (AppUrl.nextEntry globalParams |> AppUrl.toString)
        ]
        [ Icon.next "width: 3em; height: 3em" ]


prevButton : Html Msg
prevButton =
    span
        [ class "absolute rounded-full bg-blue pin-l pin-t shadow-md"
        , style "margin" "6.4rem 0 0 -1em"
        , onClick BackToPrevPage
        ]
        [ Icon.prev "width: 3em; height: 3em" ]


cardBodyView : Model -> Dictionary -> Html Msg
cardBodyView { entry, session, textElementSize, textWrapperElementSize } results =
    let
        textToShow =
            if session.globalParams.translate then
                entry.translation

            else
                entry.index

        textDisposition =
            Maybe.map2
                computeTextDisposition
                textElementSize
                textWrapperElementSize
    in
    div
        [ id "text-wrapper"
        , class "select-none h-64 text-grey-darkest relative"
        , onClick
            (NavigateTo
                (AppUrl.entry entry.index session.globalParams
                    |> AppUrl.withTranslate (not session.globalParams.translate)
                )
            )
        ]
        [ div
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
        , starView entry.starred
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


linksView : Entry -> GlobalQueryParams -> Language -> Html Msg
linksView entry globalParams userLanguage =
    let
        simpleDe =
            Entry.withoutArticle entry
    in
    div
        [ class "flex justify-around flex-wrap pb-4 px-4" ]
        [ linkView True ("https://www.google.com/search?q=" ++ simpleDe ++ "&tbm=isch") "Bilder" Icon.image
        , linkView True ("https://de.wiktionary.org/wiki/" ++ simpleDe) "Untersuchen" Icon.detail
        , linkView True (Google.translationAppUrl simpleDe userLanguage) "Hören" Icon.sound
        , linkView False
            (AppUrl.editorFor entry.index globalParams
                |> AppUrl.withFilters globalParams.filters
                |> AppUrl.toString
            )
            "Bearbeiten"
            Icon.edit
        , div [ class "flex items-center py-1" ]
            [ span
                [ class "bg-grey-light rounded-full mx-1 shadow p-3 text-grey-darker cursor-pointer"
                , onClick CopyToClipboard
                ]
                [ Icon.copy "" ]
            ]
        ]


linkView : Bool -> String -> String -> (String -> Html Msg) -> Html Msg
linkView external url label icon =
    div [ class "flex items-center py-1" ]
        [ a
            (Help.flatten
                [ Help.V <| href url
                , Help.V <| class "bg-grey-light rounded-full mx-1 shadow p-3 text-grey-darker"
                , Help.O external (\_ -> target "_blank")
                ]
            )
            [ icon "" ]
        ]


entryDetailView : GlobalQueryParams -> Entry -> Html Msg
entryDetailView globalParams { pos, example, tags } =
    div
        [ class "text-grey-light leading-normal text-left bg-grey-darkest shadow-md rounded"
        , style "padding" "10em 2em 2em 2em"
        , style "margin-top" "-10em -2em auto -2em"
        ]
        [ section [ class "mb-8" ]
            [ h3 [ class "my-4 text-xs" ] [ text "Teil" ]
            , p [] [ text (PartOfSpeech.toString pos) ]
            ]
        , section [ class "mb-8" ]
            [ h3 [ class "my-4 text-xs" ] [ text "Beispiel" ]
            , p [ class "whitespace-pre-wrap" ]
                [ text
                    (example
                        |> Maybe.map Entry.censorExample
                        |> Maybe.withDefault "-"
                    )
                ]
            ]
        , section [ class "mb-8" ]
            [ h3 [ class "my-4 text-xs" ] [ text "Etikett" ]
            , if List.length tags > 0 then
                ul [ class "list-reset" ]
                    (tags
                        |> List.map
                            (\tag ->
                                li
                                    [ class "bg-grey-light text-grey-darker px-2 py-1 mr-2 mb-1 inline-block rounded text-xs"
                                    , onClick (NavigateTo (AppUrl.nextEntry globalParams |> AppUrl.withFilters [ HasTag tag ]))
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
    Button.floatingGroup
        [ a
            [ class "rounded-full text-xl shadow-md flex w-8 h-8 justify-center items-center mb-2"
            , class
                (if globalParams.shuffle then
                    "bg-green"

                 else
                    "bg-grey"
                )
            , href
                (AppUrl.entry entry.index globalParams
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
        , Button.addNewEntry (AppUrl.newEntry Nothing globalParams)
        ]


translateSearchFieldMsg : Model -> SearchField.Msg -> Msg
translateSearchFieldMsg model msg =
    let
        params =
            model.session.globalParams
    in
    (case msg of
        SearchField.ClearSearchText ->
            AppUrl.entry model.entry.index { params | filters = [] }

        SearchField.Focus ->
            AppUrl.search params

        SearchField.SearchInput txt ->
            AppUrl.search { params | filters = Filter.parse txt }

        SearchField.ToggleSearchResults ->
            AppUrl.entries model.session.globalParams
    )
        |> NavigateTo
