{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Hodor.Model.Locations
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Composite layer for location information.
--
module Hodor.Model.Locations
  ( -- * Location Haskell value and Postgres column Records
    LocViewRec, LocWriteRec, LocViewRecCols, LocWriteRecCols

    -- * Location Haskell value lenses
  , fLocId, fLocTeamId, fLocStoreId, fLocShortCode, fLocDesc, fLocCreated
  , fLocIdMay, fLocCreatedMay

    -- * Location Postgres column lenses
  , cLocId, cLocTeamId, cLocStoreId, cLocShortCode, cLocDesc, cLocCreated
  , cLocIdMay, cLocCreatedMay

  , locTbl', locQ'
  ) where

--------------------------------------------------------------------------------

-- Base imports
import           ClassyPrelude     hiding (optional)

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

-- | A listing of location information necessary to query the Dor API.
withLensesAndProxies [d|
  type FLocId = "id" :-> Int32
  type CLocId = "id" :-> Column PGInt4
  -- ^ Auto-incrementing integer ID.

  type FLocTeamId = "team_id" :-> Text
  type CLocTeamId = "team_id" :-> Column PGText
  -- ^ Dor team ID.

  type FLocStoreId = "store_id" :-> Text
  type CLocStoreId = "store_id" :-> Column PGText
  -- ^ Dor store ID.

  type FLocShortCode = "short_code" :-> Text
  type CLocShortCode = "short_code" :-> Column PGText
  -- ^ Building identifier.

  type FLocDesc = "description" :-> Text
  type CLocDesc = "description" :-> Column PGText
  -- ^ Human readable description of the location.

  type FLocCreated = "created_at" :-> UTCTime
  type CLocCreated = "created_at" :-> Column PGTimestamptz
  -- ^ Timestamp recording location's creation.
  |]


-- | Types for columns that have PostgreSQL defaults.
withLensesAndProxies [d|
  type FLocIdMay = "id" :-> Maybe Int32
  type CLocIdMay = "id" :-> Maybe (Column PGInt4)
  -- ^ Auto-incrementing integer ID.

  type FLocCreatedMay = "created_at" :-> Maybe UTCTime
  type CLocCreatedMay = "created_at" :-> Maybe (Column PGTimestamptz)
  -- ^ Timestamp recording location's creation.
  |]


-- | Record containing Haskell-level @'Location\''@ view representation.
type LocViewRec =
  '[ FLocId
   , FLocShortCode
   , FLocDesc
   , FLocCreated
   ]

-- | Record containing Haskell-level @'Location\''@ write representation.
type LocWriteRec =
  '[ FLocIdMay
   , FLocShortCode
   , FLocDesc
   , FLocCreatedMay
   ]

-- | Record containing Postgres-level @'Location\''@ view representation.
type LocViewRecCols =
  '[ CLocId
   , CLocShortCode
   , CLocDesc
   , CLocCreated
   ]

-- | Record containing Postgres-level @'Location\''@ write representation.
type LocWriteRecCols =
  '[ CLocIdMay
   , CLocShortCode
   , CLocDesc
   , CLocCreatedMay
   ]

--
-- Tables
--

-- | Database table for @'Dor'@s.
locTbl'
  :: (  String
     -> TableProperties (Record LocWriteRecCols) (Record LocViewRecCols)
     -> Table           (Record LocWriteRecCols) (Record LocViewRecCols)
     )
  -> Table (Record LocWriteRecCols) (Record LocViewRecCols)
locTbl' schema = schema "locations" $ defaultRecTable

--
-- Queries
--

-- | Query to select '@Dor'@s from the database.
locQ'
  :: Table    (Record LocWriteRecCols) (Record LocViewRecCols)
  -> QueryArr ()                       (Record LocViewRecCols)
locQ' = queryTable
