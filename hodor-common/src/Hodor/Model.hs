{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Module      : Hodor.Model
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Opaleye layer for tables within this project.
--
module Hodor.Model
  ( -- * Table definitions
    dorTbl

    -- * Query builders
  , dorQ

    -- * Re-exports
  , module X
  ) where

--------------------------------------------------------------------------------

-- Base imports
import           ClassyPrelude     hiding (optional)
import           Composite.Record  (Record)

-- Database imports
import           Opaleye           hiding (null)

-- Local imports
import           Hodor.Model.Dor

-- Re-exports
import           Hodor.Model.Dor   as X hiding (dorQ', dorTbl')

--------------------------------------------------------------------------------

--
-- Tables
--

-- | Database schema for this project.
schema :: String -> TableProperties write view -> Table write view
schema = TableWithSchema "hodor"

-- | Database table for @'Dor'@s.
dorTbl :: Table (Record DorWriteRecCols) (Record DorViewRecCols)
dorTbl = dorTbl' schema

--------------------------------------------------------------------------------

--
-- Queries
--

-- | Query to select '@Dor'@s from the database.
dorQ :: QueryArr () (Record DorViewRecCols)
dorQ = dorQ' dorTbl
