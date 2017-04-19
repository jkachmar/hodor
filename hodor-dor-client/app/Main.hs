module Main where

import           ClassyPrelude
import           Control.Lens
import           Data.Time
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

  -- Get the previous day's date, given the current machine's local time
  day <- (addDays (-1)) . localDay . zonedTimeToLocalTime <$> getZonedTime
  occ <- runClientApp cfg $ getDailyOccupancy 1124 1362 day

  case occ of
    Left e  -> print e
    Right v ->
      print $ foldHourlyOccupancies $ view doHours v
