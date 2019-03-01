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
import Html exposing (Html, a, div, h3, section, span, text)
import Html.Attributes exposing (class, href, id, style, target)
import Html.Events exposing (onClick)
import Html.Keyed


type alias Model msg =
    { session : Session
    , results : Dictionary
    , entry : Entry
    , partOfSpeech : List (Html msg)
    , example : List (Html msg)
    , tags : List (Html msg)
    , cardContents : List (Html msg)
    , onNavigationRequested : AppUrl -> msg
    , onBackLinkClicked : msg
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
        , ( "card", cardView vm )
        , ( "buttons", buttons vm )
        ]


entryDetailView : Model msg -> Html msg
entryDetailView { partOfSpeech, example, tags } =
    div
        [ class "text-grey-light leading-normal text-left bg-grey-darkest shadow-md rounded"
        , style "padding" "10em 2em 2em 2em"
        , style "margin" "-10em -2em auto -2em"
        ]
        [ entryDetailRowView "Tile" partOfSpeech
        , entryDetailRowView "Beispiel" example
        , entryDetailRowView "Etikett" tags
        ]


entryDetailRowView : String -> List (Html msg) -> Html msg
entryDetailRowView title body =
    section [ class "mb-8" ]
        ([ h3 [ class "my-4 text-xs" ] [ text title ] ] ++ body)


nextButton : Model msg -> Html msg
nextButton { session } =
    a
        [ class "absolute rounded-full bg-blue pin-r pin-t shadow-md"
        , style "margin" "6.4rem -1em 0 0"
        , href (AppUrl.nextEntry session.globalParams |> AppUrl.toString)
        ]
        [ Icon.next "width: 3em; height: 3em" ]


prevButton : Model msg -> Html msg
prevButton { onBackLinkClicked } =
    span
        [ class "absolute rounded-full bg-blue pin-l pin-t shadow-md"
        , style "margin" "6.4rem 0 0 -1em"
        , onClick onBackLinkClicked
        ]
        [ Icon.prev "width: 3em; height: 3em" ]


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


cardView : Model msg -> Html msg
cardView ({ results } as vm) =
    div []
        [ div
            [ class "rounded bg-white shadow-lg relative mt-4 mb-8" ]
            (Help.flatten
                [ Help.V <| cardBodyView vm
                , Help.O (Array.length results > 1) <| \_ -> nextButton vm
                , Help.V <| prevButton vm
                ]
            )
        , linksView vm
        , entryDetailView vm
        ]


cardBodyView : Model msg -> Html msg
cardBodyView { entry, session, cardContents } =
    a
        [ id "text-wrapper"
        , class "block select-none h-64 text-grey-darkest relative"
        , href
            (AppUrl.entry entry.index session.globalParams
                |> AppUrl.withTranslate (not session.globalParams.translate)
                |> AppUrl.toString
            )
        ]
        cardContents


buttons : Model msg -> Html msg
buttons { session, entry } =
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
        , Button.addNewEntry (AppUrl.newEntry Nothing session.globalParams)
        ]


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
