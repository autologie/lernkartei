module Data.Google exposing (translationAppUrl)

import Data.Session


translationAppUrl : String -> Data.Session.Language -> String
translationAppUrl entry toLanguage =
    let
        toLanguageName =
            case toLanguage of
                Data.Session.Japanese ->
                    "ja"
    in
    "https://translate.google.co.jp/m/translate?hl=translation#view=home&op=translate&sl=de&tl=" ++ toLanguageName ++ "&text=" ++ entry
