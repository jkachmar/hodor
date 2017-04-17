{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : External.Dor.Api
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Dor API specification.
--
module External.Dor.Api
  ( -- * Servant API specification
    DorApi

    -- * Servant Client API query functions
  , dorApi

    -- * Re-exports
  , BearerToken(..), RefreshToken(..)
  , DailyOccupancy(..), HourlyOccupancy(..)

    -- * Lenses
  , doStoreId, doDate, doInCount, doOutCount, doHours
  , hoStoreId, hoDate, hoDateTime, hoInCount, hoOutCount

    -- * Empty HourlyOccupancy constructor
  , mkHourlyOccupancy
  ) where

-- Servant
import           Data.Proxy             (Proxy (..))
import           Servant.API

-- Local module imports
import           External.Dor.Auth
import           External.Dor.Occupancy

--------------------------------------------------------------------------------

-- | Servant API type describing the "v1" routes we're interested in
type DorApi = "v1" :> DorApi'
type DorApi' = AuthApi :<|> OccupancyApi

-- | Dor Occupancy API client query function
dorApi :: Proxy DorApi
dorApi = Proxy
