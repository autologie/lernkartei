module Data.Session exposing
    ( AccumulatingSession
    , Language(..)
    , Session
    , toAccumulatingSession
    , toSession
    , update
    , withDict
    )

import Browser.Navigation exposing (Key)
import Data.AppUrl as AppUrl exposing (GlobalQueryParams)
import Data.Dictionary as Dictionary exposing (Dictionary)
import Data.Entry as Entry exposing (Entry)
import Data.Filter as Filter exposing (Filter)
import Data.Progress as Progress exposing (Progress)
import Random exposing (Seed)
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
    , progress : Progress
    , seed : Seed
    }


type alias AccumulatingSession =
    { navigationKey : Key
    , userId : Maybe String
    , dict : Maybe Dictionary
    , zone : Maybe Zone
    , zoneName : Maybe ZoneName
    , startTime : Posix
    , language : Language
    , seed : Seed
    }


type Language
    = Japanese


withDict : Dictionary -> Session -> Session
withDict dict session =
    { session | dict = dict }


toAccumulatingSession : Session -> AccumulatingSession
toAccumulatingSession { navigationKey, userId, dict, zone, zoneName, startTime, language, seed } =
    { navigationKey = navigationKey
    , userId = Just userId
    , dict = Just dict
    , zone = Just zone
    , zoneName = Just zoneName
    , startTime = startTime
    , language = language
    , seed = seed
    }


toSession : AccumulatingSession -> Maybe Session
toSession session =
    Maybe.map2
        (\userId dict ->
            let
                shuffle =
                    False

                filters =
                    []

                ( updatedProgress, updatedSeed ) =
                    Progress.init
                        dict
                        filters
                        shuffle
                        session.startTime
                        session.seed
            in
            { navigationKey = session.navigationKey
            , userId = userId
            , dict = dict
            , zone = Time.utc
            , zoneName = Offset 0
            , globalParams =
                { filters = filters
                , shuffle = shuffle
                , translate = False
                }
            , startTime = session.startTime
            , language = session.language
            , progress = updatedProgress
            , seed = session.seed
            }
        )
        session.userId
        session.dict


update : Maybe String -> Maybe Int -> Maybe Int -> Session -> Session
update filter shuffle translate session =
    let
        parsedGlobalParams =
            AppUrl.buildQueryParams filter shuffle translate
    in
    { session
        | globalParams = parsedGlobalParams
        , progress =
            if parsedGlobalParams.filters == session.globalParams.filters then
                session.progress

            else
                let
                    ( updatedProgress, updatedSeed ) =
                        Progress.init
                            session.dict
                            parsedGlobalParams.filters
                            parsedGlobalParams.shuffle
                            session.startTime
                            session.seed
                in
                updatedProgress
    }
