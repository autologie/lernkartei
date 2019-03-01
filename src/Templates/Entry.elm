module Templates.Entry exposing (Model, layout)

import Array
import Components.Button as Button
import Components.Icon as Icon
import Components.SearchField as SearchField
import Data.AppUrl as AppUrl exposing (AppUrl)
import Data.Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Duration(..), Filter(..))
import Data.Google as Google
import Data.Session exposing (Session)
import Help
import Html exposing (Html, a, div, h3, p, section, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Html.Events exposing (onClick)
import Html.Keyed
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


type alias Model msg =
    { session : Session
    , results : Dictionary
    , entry : Entry
    , partOfSpeech : List (Html msg)
    , example : List (Html msg)
    , tags : List (Html msg)
    , cardContent : String -> Html msg
    , extraContent : Html msg
    , actions : Html msg
    , onNavigationRequested : AppUrl -> msg
    , onBackLinkClicked : Maybe msg
    , onCopyToClipboardClicked : msg
    }


layout : Model msg -> Html msg
layout ({ session, results } as vm) =
    Html.Keyed.node "div"
        [ class "container max-w-md p-5 pb-0" ]
        [ ( "search"
          , SearchField.view
                results
                (Filter.toString session.globalParams.filters)
                session.globalParams.filters
                False
                |> Html.map (translateSearchFieldMsg vm)
          )
        , ( "body", bodyView vm )
        , ( "extra", vm.extraContent )
        ]


entryDetailView : Model msg -> Html msg
entryDetailView { entry, session, partOfSpeech, actions, example, tags } =
    div
        [ class "text-grey-light leading-normal text-left bg-grey-darkest shadow-md rounded"
        , style "padding" "10em 2em 2em 2em"
        , style "margin" "-10em -2em auto -2em"
        ]
        [ dateView session.zone session.zoneName entry.addedAt entry.updatedAt
        , entryDetailRowView "Tile" partOfSpeech
        , entryDetailRowView "Beispiel" example
        , entryDetailRowView "Etikett" tags
        , actions
        ]


entryDetailRowView : String -> List (Html msg) -> Html msg
entryDetailRowView title body =
    section [ class "mb-8" ]
        ([ h3 [ class "my-4 text-xs" ] [ text title ] ] ++ body)


linksView : Model msg -> Html msg
linksView { session, entry, onCopyToClipboardClicked } =
    let
        simpleDe =
            Entry.withoutArticle entry
    in
    div
        [ class "flex justify-around flex-wrap pb-4 px-4" ]
        [ linkView True ("https://www.google.com/search?q=" ++ simpleDe ++ "&tbm=isch") "Bilder" Icon.image
        , linkView True ("https://de.wiktionary.org/wiki/" ++ simpleDe) "Untersuchen" Icon.detail
        , linkView True (Google.translationAppUrl simpleDe session.language) "Hören" Icon.sound
        , linkView False
            (AppUrl.editorFor entry.index session.globalParams
                |> AppUrl.withFilters session.globalParams.filters
                |> AppUrl.toString
            )
            "Bearbeiten"
            Icon.edit
        , div [ class "flex items-center py-1" ]
            [ span
                [ class "bg-grey-light rounded-full mx-1 shadow p-3 text-grey-darker cursor-pointer"
                , onClick onCopyToClipboardClicked
                ]
                [ Icon.copy "" ]
            ]
        ]


linkView : Bool -> String -> String -> (String -> Html msg) -> Html msg
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


bodyView : Model msg -> Html msg
bodyView ({ session, entry, cardContent, results, onBackLinkClicked } as vm) =
    div []
        [ div
            [ class "rounded bg-white shadow-lg relative mt-4 mb-8" ]
            (Help.flatten
                [ Help.V <| cardContent "block select-none h-64 text-grey-darkest relative"
                , Help.O (Array.length results > 1) <| \_ -> nextButton vm
                , Help.M <| (onBackLinkClicked |> Maybe.map (\oblc -> prevButton oblc))
                ]
            )
        , linksView vm
        , entryDetailView vm
        ]


nextButton : Model msg -> Html msg
nextButton { session } =
    a
        [ class "absolute rounded-full bg-blue pin-r pin-t shadow-md"
        , style "margin" "6.4rem -1em 0 0"
        , href (AppUrl.nextEntry session.globalParams |> AppUrl.toString)
        ]
        [ Icon.next "width: 3em; height: 3em" ]


prevButton : msg -> Html msg
prevButton onBackLinkClicked =
    span
        [ class "absolute rounded-full bg-blue pin-l pin-t shadow-md"
        , style "margin" "6.4rem 0 0 -1em"
        , onClick onBackLinkClicked
        ]
        [ Icon.prev "width: 3em; height: 3em" ]


translateSearchFieldMsg : Model msg -> SearchField.Msg -> msg
translateSearchFieldMsg { entry, session, onNavigationRequested } msg =
    let
        params =
            session.globalParams
    in
    (case msg of
        SearchField.ClearSearchText ->
            AppUrl.entry entry.index { params | filters = [] }

        SearchField.Focus ->
            AppUrl.search params

        SearchField.SearchInput txt ->
            AppUrl.search { params | filters = Filter.parse txt }

        SearchField.ToggleSearchResults ->
            AppUrl.entries session.globalParams
    )
        |> onNavigationRequested


dateView : Zone -> ZoneName -> Posix -> Posix -> Html msg
dateView zone zoneName addedAt updatedAt =
    let
        addedAtExpr =
            describeDate zone zoneName addedAt

        updatedAtExpr =
            describeDate zone zoneName updatedAt
    in
    div
        [ class "text-grey-light text-center my-2" ]
        [ p [] [ text ("Added on: " ++ addedAtExpr) ]
        , p [] [ text ("Updated on: " ++ updatedAtExpr) ]
        ]


describeDate : Zone -> ZoneName -> Posix -> String
describeDate zone zoneName posix =
    if Time.posixToMillis posix == 0 then
        "-"

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
