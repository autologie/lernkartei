module Data.Session exposing
    ( AccumulatingSession
    , Language(..)
    , Session
    , toAccumulatingSession
    , toSession
    , withDict
    )

import Browser.Navigation exposing (Key)
import Data.AppUrl exposing (GlobalQueryParams)
import Data.Dictionary exposing (Dictionary)
import Time exposing (Month(..), Posix, Zone, ZoneName(..))


type alias Session =
    { navigationKey : Key
    , userId : String
    , dict : Dictionary
    , zone : Zone
    , zoneName : ZoneName
    , globalParams : GlobalQueryParams
    , startTime : Posix
    , language : Language
    }


type alias AccumulatingSession =
    { navigationKey : Key
    , userId : Maybe String
    , dict : Maybe Dictionary
    , zone : Maybe Zone
    , zoneName : Maybe ZoneName
    , startTime : Posix
    , language : Language
    }


type Language
    = Japanese


withDict : Dictionary -> Session -> Session
withDict dict session =
    { session | dict = dict }


toAccumulatingSession : Session -> AccumulatingSession
toAccumulatingSession { navigationKey, userId, dict, zone, zoneName, startTime, language } =
    { navigationKey = navigationKey
    , userId = Just userId
    , dict = Just dict
    , zone = Just zone
    , zoneName = Just zoneName
    , startTime = startTime
    , language = language
    }


toSession : AccumulatingSession -> Maybe Session
toSession session =
    Maybe.map2
        (\userId dict ->
            { navigationKey = session.navigationKey
            , userId = userId
            , dict = dict
            , zone = Time.utc
            , zoneName = Offset 0
            , globalParams =
                { filters = []
                , shuffle = False
                , translate = False
                }
            , startTime = session.startTime
            , language = session.language
            }
        )
        session.userId
        session.dict
