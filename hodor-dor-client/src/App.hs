{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving           #-}
{-# LANGUAGE LambdaCase                 #-}
-- |
-- Module      : App
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Client querying application and associated helper functions.
--

module App where

-- Base imports
import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Error.Class (catchError, throwError)
import           Control.Monad.Except
import           Data.Int (Int16)
import           Data.Time
import           Data.Traversable          (mapAccumL)

-- Composite and related
import           Composite.Record
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)

-- Servant and related
import           Network.HTTP.Types.Status  (forbidden403)
import           Servant.Client             (ServantError (..))

-- Local imports
import           External.Dor.Client        hiding (bearerToken, refreshToken)
import           Extras.Servant             (MonadClient)
import           Hodor.Model

--------------------------------------------------------------------------------

data Config
  = Config
  { bearerTVar   :: TVar BearerToken -- ^ TVar for the application bearer token
  , client       :: DorClient        -- ^ Dor API query record
  , connPool     :: Pool Connection  -- ^ PostgreSQL Connection pool
  , refreshToken :: RefreshToken     -- ^ Global application refresh token
  }

-- | Type alias for the client's query context and its operating environment
type ClientApp = ReaderT Config (ExceptT ServantError IO)

-- | Convenience function to run a '@ClientApp'@
runClientApp
  :: MonadBaseControl IO m
  => Config
  -> ClientApp a
  -> m (Either ServantError a)
runClientApp cfg = liftBase . runExceptT . (flip runReaderT cfg)

--------------------------------------------------------------------------------

createHourlyRecord
  :: TimeZone -> Text -> Text
  -> (Int16, Int16) -> HourlyOccupancy
  -> ((Int16, Int16), (Record DorWriteRec))
createHourlyRecord tz building location (accIn, accOut) hourOcc =
  let hourIn    = fromIntegral (hourOcc ^. hoInCount)  :: Int16
      hourOut   = fromIntegral (hourOcc ^. hoOutCount) :: Int16
      sumIn     = accIn  + hourIn
      sumOut    = accOut + hourOut
      timestamp = localTimeToUTC tz (hourOcc ^. hoDateTime)
  in do
    let record = (  Nothing
                :*: building
                :*: location
                :*: hourIn
                :*: hourOut
                :*: sumIn
                :*: sumOut
                :*: timestamp
                :*: Nothing
                :*: RNil
                )
    (,) (sumIn, sumOut) record

foldHourlyOccupancies
  :: Traversable t
  => t HourlyOccupancy
  -> t (Record DorWriteRec)
foldHourlyOccupancies hourOccs =
  -- FIXME: `hoursToTimeZone` should be changed to some `TimeZone` from the DB
  -- FIXME: "FooBuilding" and "BarLocation" should come from the DB as well
  let helper = createHourlyRecord (hoursToTimeZone 0) "FooBuilding" "BarLocation"
      (_, records) = mapAccumL helper (0, 0) hourOccs
  in records

--------------------------------------------------------------------------------

-- Test the functions below in the REPL with this
{-
do' = decode dailyOccupancyJSON :: Maybe DailyOccupancy
ho = over _Just (^. doHours) do'

day = ModifiedJulianDay 57827
ho' = flip mkDayHours day <$> ho

((Prelude..) fmap fmap) (^. hoDateTime) newHo
-}

type TeamId  = Int -- ^ Type alias for Dor "team_id"
type StoreId = Int -- ^ Type alias for Dor "store_id"

-- | Get the daily occupancy for a location, padded to 24 hours' worth of entries
getDailyOccupancy
  :: ( MonadBaseControl IO m
     , MonadClient m
     , MonadReader Config m
     )
  => TeamId
  -> StoreId
  -> Day
  -> m DailyOccupancy
getDailyOccupancy teamId storeId day =
  pure . padDailyOccupancy storeId day =<< reqWithReAuth getOcc
  where
    getOcc c t =
      let dateTxt = pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
      in  runGetDailyOccupancy c t teamId storeId dateTxt

-- | Pad a given daily occupancy for a specific day up to 24 hours
padDailyOccupancy :: StoreId -> Day -> DailyOccupancy -> DailyOccupancy
padDailyOccupancy storeId day occ =
  let hourOccs       = occ ^. doHours
      hours          = LocalTime day <$> (\h -> TimeOfDay h 0 0) <$> [0..23]
      paddedHours    = (mkHourlyOccupancy (toInteger storeId) day) <$> hours
      paddedHourOccs = go paddedHours hourOccs

  in occ & doHours .~ paddedHourOccs

  where
    go :: [HourlyOccupancy] -> [HourlyOccupancy] -> [HourlyOccupancy]
    go []          _  = []
    go paddedHours [] = paddedHours

    go (paddedHour:paddedHours) occs@(occHour:occHours) =
      if (occHour ^. hoDateTime == paddedHour ^. hoDateTime)
        then occHour    : go paddedHours occHours
        else paddedHour : go paddedHours occs

--------------------------------------------------------------------------------

reqWithReAuth
  :: ( MonadClient m
     , MonadReader Config m
     )
  => (  DorClient
     -> BearerToken
     -> m a
     )
  -> m a
reqWithReAuth action = do
  bearerTV <- asks bearerTVar
  bearer   <- atomically $ readTVar bearerTV
  cli      <- asks client
  refresh  <- asks refreshToken

  let req = action cli bearer

  catchError req $ \case

    -- If the service responds with a 403, reauth and retry the request
    e@(FailureResponse status _ _) ->
      if (status == forbidden403)
        then do
          freshBearer <- runGetBearerToken cli refresh
          atomically $ writeTVar bearerTV freshBearer
          action cli freshBearer

        else throwError e

    e -> throwError e
