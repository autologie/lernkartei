module Data.Session exposing (AccumulatingSession, Session, toAccumulatingSession, toSession, withDict)

import AppUrl exposing (GlobalQueryParams)
import Browser.Navigation exposing (Key)
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
    }


type alias AccumulatingSession =
    { navigationKey : Key
    , userId : Maybe String
    , dict : Maybe Dictionary
    , zone : Maybe Zone
    , zoneName : Maybe ZoneName
    , startTime : Posix
    }


withDict : Dictionary -> Session -> Session
withDict dict session =
    { session | dict = dict }


toAccumulatingSession : Session -> AccumulatingSession
toAccumulatingSession { navigationKey, userId, dict, zone, zoneName, globalParams, startTime } =
    { navigationKey = navigationKey
    , userId = Just userId
    , dict = Just dict
    , zone = Just zone
    , zoneName = Just zoneName
    , startTime = startTime
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
                { filters = Nothing
                , shuffle = False
                , translate = False
                }
            , startTime = session.startTime
            }
        )
        session.userId
        session.dict
