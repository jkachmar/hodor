{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
-- |
-- Module      : External.Dor.Occupancy
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Dor Daily Occupancy API spec and JSON marshalling.
--
module External.Dor.Occupancy where

-- Base imports
import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.Types  (typeMismatch)
import           Data.Time
import           Text.RawString.QQ

-- Servant and related
import           Servant.API

-- Lens
import           Control.Lens.TH   (makeLenses)

-- Local module imports
import           External.Dor.Auth (BearerToken)

--------------------------------------------------------------------------------

--
-- Servant Client Types
--

-- | Dor Occupancy API endpoint as a Servant type.
type OccupancyApi =
  Header "Authorization" BearerToken
  :> "teams"  :> Capture "team_id"  Int
  :> "stores" :> Capture "store_id" Int
  :> "days"   :> Capture "date"     Text
  :> Get '[JSON] DailyOccupancy

--------------------------------------------------------------------------------

--
-- Aeson Deserialization Types
--

data DailyOccupancy
  = DailyOccupancy
  { _doStoreId  :: Integer
  , _doDate     :: Day
  , _doInCount  :: Integer
  , _doOutCount :: Integer
  , _doHours    :: [HourlyOccupancy]
  } deriving Show

instance FromJSON DailyOccupancy where
  parseJSON = withObject "daily occupancy" $ \o -> do
    -- unwrap the top level "data" object
    dataO <- o .: "data"

    -- parse the values out of "data"
    _doStoreId  <- dataO .: "store_id"
    _doDate     <- dataO .: "date"
    _doInCount  <- dataO .: "in_count"
    _doOutCount <- dataO .: "out_count"

    -- unwrap the "hours" object
    hoursArr <- dataO .: "hours"

    -- parse the array of hourly occupancies out of "hours"
    _doHours <- parseJSON hoursArr
    pure DailyOccupancy{..}

data HourlyOccupancy
  = HourlyOccupancy
  { _hoStoreId  :: Integer
  , _hoDate     :: Day
  , _hoDateTime :: LocalTime
  , _hoInCount  :: Integer
  , _hoOutCount :: Integer
  } deriving Show

mkHourlyOccupancy :: Integer -> Day -> LocalTime -> HourlyOccupancy
mkHourlyOccupancy storeId day hour = HourlyOccupancy storeId day hour 0 0

instance FromJSON HourlyOccupancy where
  parseJSON (Object v) = HourlyOccupancy
    <$>  v .: "store_id"
    <*>  v .: "date"
    <*> (v .: "datetime" >>= dorDateTime)
    <*>  v .: "in_count"
    <*>  v .: "out_count"

    where dorDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d %H"

  parseJSON invalid = typeMismatch "HourlyOccupancy" invalid

--
-- Lenses
--

makeLenses ''DailyOccupancy
makeLenses ''HourlyOccupancy

--
-- Aeson Deserialization Tests
--

dailyOccupancyJSON :: IsString a => a
dailyOccupancyJSON = [r|
{
  "data":
  {
    "store_id": 1000,
    "date": "2017-03-15",
    "in_count": 133,
    "out_count": 215,
    "hours": [
      {
        "store_id": 1000,
        "date": "2017-03-15",
        "datetime": "2017-03-15 00",
        "in_count": 1,
        "out_count": 0
      },
      {
        "store_id": 1000,
        "date": "2017-03-15",
        "datetime": "2017-03-15 01",
        "in_count": 2,
        "out_count": 3
      }
    ]
  }
}
|]

hourlyOccupancyJSON :: IsString a => a
hourlyOccupancyJSON = [r|
{
  "store_id": 1280,
  "date": "2017-03-15",
  "datetime": "2017-03-15 00",
  "in_count": 1,
  "out_count": 0
}
|]
