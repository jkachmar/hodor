{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- |
-- Module      : Extras.Servant
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Radioactive dumping ground for Servant instances, type aliases, and helpers.
--
module Extras.Servant (ClientM, MonadClient) where

import           ClassyPrelude
import           Control.Monad.Except
import           Servant.Client       hiding (ClientM)

type ClientM a = forall m . MonadClient m => m a
type MonadClient m = (MonadIO m, MonadError ServantError m)
