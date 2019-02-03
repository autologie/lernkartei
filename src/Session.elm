module Session exposing (AccumulatingSession, Session, toAccumulatingSession, toSession, withDict)

import Browser.Navigation exposing (Key)
import Dictionary exposing (Dictionary)
import Time exposing (Month(..), Zone, ZoneName(..))


type alias Session =
    { navigationKey : Key
    , userId : String
    , dict : Dictionary
    , zone : Zone
    , zoneName : ZoneName
    }


type alias AccumulatingSession =
    { navigationKey : Key
    , userId : Maybe String
    , dict : Maybe Dictionary
    , zone : Maybe Zone
    , zoneName : Maybe ZoneName
    }


withDict : Dictionary -> Session -> Session
withDict dict session =
    { session | dict = dict }


toAccumulatingSession : Session -> AccumulatingSession
toAccumulatingSession { navigationKey, userId, dict, zone, zoneName } =
    { navigationKey = navigationKey
    , userId = Just userId
    , dict = Just dict
    , zone = Just zone
    , zoneName = Just zoneName
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
            }
        )
        session.userId
        session.dict
