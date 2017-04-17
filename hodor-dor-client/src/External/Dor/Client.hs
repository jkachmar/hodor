{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- |
-- Module      : External.Dor.Client
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Dor API client.
--
module External.Dor.Client
  ( -- * API client record and smart constructor
    DorClient(..), mkDorClient

    -- * Re-exports
  , module X
  ) where

-- Base imports
import           ClassyPrelude
import           Control.Monad.Except

-- Servant and related
import           Network.HTTP.Client  (Manager)
import           Servant.API
import           Servant.Client       hiding (ClientM)
import qualified Servant.Client

-- Local imports
import           External.Dor.Api (dorApi)
import           Extras.Servant   (ClientM, MonadClient)

-- Re-exporting
import           External.Dor.Api     as X hiding (DorApi, dorApi)

--------------------------------------------------------------------------------

data DorClient
  = DorClient
  { runGetBearerToken
      :: RefreshToken
      -> ClientM BearerToken

  , runGetDailyOccupancy
      :: BearerToken
      -> Int
      -> Int
      -> Text
      -> ClientM DailyOccupancy
  }

mkDorClient :: Manager -> DorClient
mkDorClient manager =
    DorClient
    { runGetBearerToken    = run . getBearer
    , runGetDailyOccupancy =
        \tok tid sid -> run . getDailyOccupancy (Just $ "Bearer " <> tok) tid sid
    }
  where
    getBearer :<|> getDailyOccupancy = client dorApi

    baseUrl = BaseUrl Https "api.getdor.com" 443 ""

    run :: MonadClient m => Servant.Client.ClientM a -> m a
    run x = do
      res <- liftIO $ runClientM x (ClientEnv manager baseUrl)
      case res of
        Left  e -> throwError e
        Right v -> pure v
