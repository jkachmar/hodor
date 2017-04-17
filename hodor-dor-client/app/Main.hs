module Main where

import           ClassyPrelude
import           Control.Lens
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           App
import           External.Dor.Client     hiding (bearerToken, refreshToken)

refreshToken' :: RefreshToken
refreshToken' = RefreshToken ""

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let client' = mkDorClient mgr

  bearerTVar' <- newTVarIO $ BearerToken  ""
  let cfg = Config { client       = client'
                   , bearerTVar   = bearerTVar'
                   , refreshToken = refreshToken'
                   }

  let day = ModifiedJulianDay 57847

  occ <- runClientApp cfg $ getDailyOccupancy 1000 1500 day

  case occ of
    Left e  -> print e

    Right v -> do
      let hours = v ^. doHours
      print $ foldHourlyOccupancies hours
