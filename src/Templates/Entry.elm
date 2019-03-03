module Templates.Entry exposing (ButtonState(..), Model, layout)

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
import Html.Attributes exposing (attribute, class, classList, href, style, target)
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
    , onBackLinkClicked : msg
    , onCopyToClipboardClicked : msg
    , onEditButtonClicked : msg
    , buttons : ButtonsModel
    }


type alias ButtonsModel =
    { imageSearchResults : ButtonState
    , wiktionary : ButtonState
    , googleTranslation : ButtonState
    , edit : ButtonState
    , copyToClipboard : ButtonState
    , prevLink : ButtonState
    , nextLink : ButtonState
    }


type ButtonState
    = Enabled
    | Disabled
    | Hidden


layout : Model msg -> Html msg
layout vm =
    Html.Keyed.node "div"
        [ class "container max-w-md p-5 pb-0" ]
        [ ( "search"
          , SearchField.view
                vm.results
                (Filter.toString vm.session.globalParams.filters)
                vm.session.globalParams.filters
                False
                |> Html.map (translateSearchFieldMsg vm)
          )
        , ( "body", bodyView vm )
        , ( "extra", vm.extraContent )
        ]


bodyView : Model msg -> Html msg
bodyView vm =
    div []
        [ div
            [ class "rounded bg-white shadow-lg relative mt-4 mb-8" ]
            [ vm.cardContent "block select-none h-64 text-grey-darkest relative"
            , nextButton vm |> Maybe.withDefault (text "")
            , prevButton vm |> Maybe.withDefault (text "")
            ]
        , linksView vm
        , entryDetailView vm
        ]


nextButton : Model msg -> Maybe (Html msg)
nextButton { session, buttons } =
    case buttons.nextLink of
        Enabled ->
            Just
                (a
                    [ class "absolute rounded-full bg-blue pin-r pin-t shadow-md"
                    , style "margin" "6.4rem -1em 0 0"
                    , href (AppUrl.nextEntry session.globalParams |> AppUrl.toString)
                    ]
                    [ Icon.next "width: 3em; height: 3em" ]
                )

        _ ->
            Nothing


prevButton : Model msg -> Maybe (Html msg)
prevButton { onBackLinkClicked, buttons } =
    case buttons.prevLink of
        Enabled ->
            Just
                (span
                    [ class "absolute rounded-full bg-blue pin-l pin-t shadow-md cursor-pointer"
                    , style "margin" "6.4rem 0 0 -1em"
                    , onClick onBackLinkClicked
                    ]
                    [ Icon.prev "width: 3em; height: 3em" ]
                )

        _ ->
            Nothing


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
        (h3 [ class "my-4 text-xs" ] [ text title ] :: body)


type LinkAction msg
    = InternalLink AppUrl
    | ExternalLink String
    | Action msg


linksView : Model msg -> Html msg
linksView { session, entry, onCopyToClipboardClicked, onEditButtonClicked, buttons } =
    let
        simpleDe =
            Entry.withoutArticle entry
    in
    div
        [ class "flex justify-around flex-wrap pb-4 px-4" ]
        ([ linkView buttons.imageSearchResults (ExternalLink ("https://www.google.com/search?q=" ++ simpleDe ++ "&tbm=isch")) "photo"
         , linkView buttons.wiktionary (ExternalLink ("https://de.wiktionary.org/wiki/" ++ simpleDe)) "list"
         , linkView buttons.googleTranslation (ExternalLink (Google.translationAppUrl simpleDe session.language)) "volume_up"
         , linkView buttons.edit (Action onEditButtonClicked) "edit"
         , linkView buttons.copyToClipboard (Action onCopyToClipboardClicked) "file_copy"
         ]
            |> List.concatMap (Maybe.map List.singleton >> Maybe.withDefault [])
        )


linkView : ButtonState -> LinkAction msg -> String -> Maybe (Html msg)
linkView state linkAction icon =
    let
        ( tag, actionAttribute, external ) =
            case linkAction of
                InternalLink appUrl ->
                    ( a, href (AppUrl.toString appUrl), False )

                ExternalLink theUrl ->
                    ( a, href theUrl, True )

                Action msg ->
                    ( div, onClick msg, False )

        button enabled =
            tag
                [ if enabled then
                    actionAttribute

                  else
                    attribute "data-dummy" ""
                , class "z-10 flex items-center no-underline material-icons bg-grey-light rounded-full mx-1 shadow p-3 text-grey-darker"
                , classList [ ( "cursor-pointer", enabled ), ( "opacity-50 disabled", not enabled ) ]
                , target
                    (if external then
                        "_blank"

                     else
                        "_self"
                    )
                ]
                [ text icon ]
    in
    case state of
        Hidden ->
            Nothing

        Enabled ->
            Just (button True)

        Disabled ->
            Just (button False)


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
        [ class "text-grey-light text-center my-2 mx-6 py-3 border-t border-b" ]
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
