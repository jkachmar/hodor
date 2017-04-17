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
  ( -- * Dor Haskell value and Postgres column records
    DorViewRec, DorWriteRec, DorViewRecCols, DorWriteRecCols

    -- * Dor Haskell value lenses
  , fDorId, fDorShortCode, fDorFloor, fDorInCount, fDorOutCount, fDorSumInCount
  , fDorSumOutCount, fDorTimestamp, fDorCreated
  , fDorIdMay, fDorCreatedMay

    -- * Dor Postgres column lenses
  , cDorId, cDorShortCode, cDorFloor, cDorInCount, cDorOutCount, cDorSumInCount
  , cDorSumOutCount, cDorTimestamp, cDorCreated
  , cDorIdMay, cDorCreatedMay

  , dorTbl', dorQ'
  ) where

--------------------------------------------------------------------------------

-- Base imports
import           ClassyPrelude     hiding (optional)
import           Data.Int          (Int16)
import           Data.Text         (Text)

-- Composite
import           Composite.Opaleye (defaultRecTable)
import           Composite.TH      (withLensesAndProxies)
import           Composite.Record  ((:->), Record)

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

  type FDorShortCode = "short_code" :-> Text
  type CDorShortCode = "short_code" :-> Column PGText
  -- ^ Building identifier.

  type FDorFloor = "location" :-> Text
  type CDorFloor = "location" :-> Column PGText
  -- ^ Human readable description of the location.

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

-- | Frame containing Haskell-level @'Dor\''@ representation.
type DorViewRec =
  '[ FDorId
   , FDorShortCode
   , FDorFloor
   , FDorInCount
   , FDorOutCount
   , FDorSumInCount
   , FDorSumOutCount
   , FDorTimestamp
   , FDorCreated
   ]

type DorWriteRec =
  '[ FDorIdMay
   , FDorShortCode
   , FDorFloor
   , FDorInCount
   , FDorOutCount
   , FDorSumInCount
   , FDorSumOutCount
   , FDorTimestamp
   , FDorCreatedMay
   ]

-- | Frame containing Postgres-level @'Dor\''@ read representation.
type DorViewRecCols =
  '[ CDorId
   , CDorShortCode
   , CDorFloor
   , CDorInCount
   , CDorOutCount
   , CDorSumInCount
   , CDorSumOutCount
   , CDorTimestamp
   , CDorCreated
   ]

-- | Frame containing Postgres-level @'Dor\''@ write representation.
type DorWriteRecCols =
  '[ CDorIdMay
   , CDorShortCode
   , CDorFloor
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
