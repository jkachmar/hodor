{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
-- |
-- Module      : External.Dor.Auth
-- Copyright   : (c) Joe Kachmar 2017
-- License     : MIT
--
-- Maintainer  :
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Dor Authentication API spec and JSON marshalling.
--
module External.Dor.Auth where

-- Base imports
import           ClassyPrelude
import           Data.Aeson

-- Servant
import           Servant.API

--------------------------------------------------------------------------------

--
-- Servant Client Types
--

-- | Dor "days" API endpoint as a Servant type.
type AuthApi =
     "tokens"
  :> ReqBody '[JSON] RefreshToken
  :> Post    '[JSON] BearerToken

--------------------------------------------------------------------------------

--
-- Aeson Deserialization Types
--

newtype RefreshToken = RefreshToken { refreshToken :: Text } deriving Show

instance ToJSON RefreshToken where
  toJSON RefreshToken{..} = object [ "refresh_token" .= refreshToken ]

newtype BearerToken
  = BearerToken { bearerToken :: Text }
  deriving (IsString, Semigroup, Show, ToHttpApiData)

instance FromJSON BearerToken where
  parseJSON = withObject "access token" $ \o -> do
    -- unwrap the top level "data" object
    dataO <- o .: "data"

    -- parse the token out of "data"
    bearerToken <- dataO .: "token"
    pure BearerToken{..}
