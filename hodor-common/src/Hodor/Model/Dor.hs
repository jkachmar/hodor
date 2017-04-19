{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Hodor.Model.Dor
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Opaleye layer for Dor tables.
--
module Hodor.Model.Dor
  ( -- * Dor Haskell value and Postgres column Records
    DorViewRec, DorWriteRec, DorViewRecCols, DorWriteRecCols

    -- * Dor Haskell value lenses
  , fDorId, fDorLocationId, fDorInCount, fDorOutCount, fDorSumInCount
  , fDorSumOutCount, fDorTimestamp, fDorCreated
  , fDorIdMay, fDorCreatedMay

    -- * Dor Postgres column lenses
  , cDorId, cDorLocationId, cDorInCount, cDorOutCount, cDorSumInCount
  , cDorSumOutCount, cDorTimestamp, cDorCreated
  , cDorIdMay, cDorCreatedMay

  , dorTbl', dorQ'
  ) where

--------------------------------------------------------------------------------

-- Base imports
import           ClassyPrelude     hiding (optional)
import           Data.Int          (Int16)

-- Composite
import           Composite.Opaleye (defaultRecTable)
import           Composite.Record  ((:->), Record)
import           Composite.TH      (withLensesAndProxies)

-- Database imports
import           Opaleye           hiding (null)

--------------------------------------------------------------------------------

--
-- Types
--

-- | An hourly snapshot of Dor device analytics.
withLensesAndProxies [d|
  type FDorId = "id" :-> Int32
  type CDorId = "id" :-> Column PGInt4
  -- ^ Auto-incrementing integer ID.

  type FDorLocationId = "location_id" :-> Int32
  type CDorLocationId = "location_id" :-> Column PGInt4
  -- ^ Building location id, referencing "locations" table.

  type FDorInCount = "in_count" :-> Int16
  type CDorInCount = "in_count" :-> Column PGInt2
  -- ^ Hourly ingress counts.

  type FDorOutCount = "out_count" :-> Int16
  type CDorOutCount = "out_count" :-> Column PGInt2
  -- ^ Hourly egress counts.

  type FDorSumInCount = "cumulative_in_count" :-> Int16
  type CDorSumInCount = "cumulative_in_count" :-> Column PGInt2
  -- ^ Cumulative, daily ingress counts up to an hour.

  type FDorSumOutCount = "cumulative_out_count" :-> Int16
  type CDorSumOutCount = "cumulative_out_count" :-> Column PGInt2
  -- ^ Cumulative, daily egress counts up to an hour.

  type FDorTimestamp = "timestamp" :-> UTCTime
  type CDorTimestamp = "timestamp" :-> Column PGTimestamptz
  -- ^ Timestamp of device's hourly egress/ingress count reporting.

  type FDorCreated = "created_at" :-> UTCTime
  type CDorCreated = "created_at" :-> Column PGTimestamptz
  -- ^ Timestamp of daily harvesting/DB insertion.
  |]

-- | Types for columns that have PostgreSQL defaults.
withLensesAndProxies [d|
  type FDorIdMay = "id" :-> Maybe Int32
  type CDorIdMay = "id" :-> Maybe (Column PGInt4)
  -- ^ Auto-incrementing integer ID.

  type FDorCreatedMay = "created_at" :-> Maybe UTCTime
  type CDorCreatedMay = "created_at" :-> Maybe (Column PGTimestamptz)
  -- ^ Timestamp of daily harvesting/DB insertion.
  |]

-- | Record containing Haskell-level @'Dor\''@ view representation.
type DorViewRec =
  '[ FDorId
   , FDorLocationId
   , FDorInCount
   , FDorOutCount
   , FDorSumInCount
   , FDorSumOutCount
   , FDorTimestamp
   , FDorCreated
   ]

-- | Record containing Haskell-level @'Dor\''@ write representation.
type DorWriteRec =
  '[ FDorIdMay
   , FDorLocationId
   , FDorInCount
   , FDorOutCount
   , FDorSumInCount
   , FDorSumOutCount
   , FDorTimestamp
   , FDorCreatedMay
   ]

-- | Record containing Postgres-level @'Dor\''@ view representation.
type DorViewRecCols =
  '[ CDorId
   , CDorLocationId
   , CDorInCount
   , CDorOutCount
   , CDorSumInCount
   , CDorSumOutCount
   , CDorTimestamp
   , CDorCreated
   ]

-- | Record containing Postgres-level @'Dor\''@ write representation.
type DorWriteRecCols =
  '[ CDorIdMay
   , CDorLocationId
   , CDorInCount
   , CDorOutCount
   , CDorSumInCount
   , CDorSumOutCount
   , CDorTimestamp
   , CDorCreatedMay
   ]

--
-- Tables
--

-- | Database table for @'Dor'@s.
dorTbl'
  :: (  String
     -> TableProperties (Record DorWriteRecCols) (Record DorViewRecCols)
     -> Table           (Record DorWriteRecCols) (Record DorViewRecCols)
     )
  -> Table (Record DorWriteRecCols) (Record DorViewRecCols)
dorTbl' schema = schema "dor" $ defaultRecTable

--
-- Queries
--

-- | Query to select '@Dor'@s from the database.
dorQ'
  :: Table    (Record DorWriteRecCols) (Record DorViewRecCols)
  -> QueryArr ()                       (Record DorViewRecCols)
dorQ' = queryTable
