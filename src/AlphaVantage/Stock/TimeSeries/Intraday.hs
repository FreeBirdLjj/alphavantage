#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.Stock.TimeSeries.Intraday
  ( Intraday
  , intradayMetadata
  , intradayTimeSeries
  , getIntraday
  ) where

import qualified AlphaVantage.Config
import qualified AlphaVantage.TimeOrDay
import           Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import qualified Data.Map.Lazy
import qualified Data.Text
import qualified Data.Time.LocalTime
import qualified Network.HTTP.Client
import qualified Network.HTTP.Simple

data OutputSizeOption = Compact
                      | Full
                        deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON OutputSizeOption where
  parseJSON = Data.Aeson.withText "OutputSizeOption" f
    where f "Compact" = return Compact
          f "Full"    = return Full
          f _         = mempty

data MetaData = MetaData
  { information :: String
  , symbol :: String
  , lastRefreshed :: AlphaVantage.TimeOrDay.TimeOrDay
  , interval :: String
  , outputSize :: OutputSizeOption
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON MetaData where
  parseJSON (Data.Aeson.Object v) = MetaData
    <$> v .: "1. Information"
    <*> v .: "2. Symbol"
    <*> v .: "3. Last Refreshed"
    <*> v .: "4. Interval"
    <*> v .: "5. Output Size"
    <*> v .: "6. Time Zone"

data TimePoint = TimePoint
  { open :: Float
  , high :: Float
  , low :: Float
  , close :: Float
  , volume :: Integer
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON TimePoint where
  parseJSON (Data.Aeson.Object v) = TimePoint
    <$> (fmap read $ v .: "1. open")
    <*> (fmap read $ v .: "2. high")
    <*> (fmap read $ v .: "3. low")
    <*> (fmap read $ v .: "4. close")
    <*> (fmap read $ v .: "5. volume")

data Intraday = Intraday
  { metadata :: MetaData
  , timeSeries :: Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
  } deriving (Eq, Read, Show)

instance Data.Aeson.FromJSON Intraday where
  parseJSON = Data.Aeson.withObject "Intraday" $ \o -> do
    metadata   <- o .: "Meta Data"
    timeSeries <- o .: (Data.Text.pack $ "Time Series (" ++ interval metadata ++ ")")
    return $ Intraday metadata timeSeries

intradayMetadata :: Intraday -> MetaData
intradayMetadata = metadata

intradayTimeSeries :: Intraday -> Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
intradayTimeSeries = timeSeries

-- API Parameters
-- Required: function
--   The time series of your choice. In this case, function=TIME_SERIES_INTRADAY
-- Required: symbol
--   The name of the equity of your choice. For example: symbol=MSFT
-- Required: interval
--   Time interval between two consecutive data points in the time series. The following values are supported: 1min, 5min, 15min, 30min, 60min
-- Optional: outputsize
--   By default, outputsize=compact. Strings compact and full are accepted with the following specifications: compact returns only the latest 100 data points in the intraday time series; full returns the full-length intraday time series. The "compact" option is recommended if you would like to reduce the data size of each API call.
-- Optional: datatype
--   By default, datatype=json. Strings json and csv are accepted with the following specifications: json returns the intraday time series in JSON format; csv returns the time series as a CSV (comma separated value) file.
-- Required: apikey
--   Your API key.
getIntraday :: String -> String -> Network.HTTP.Client.Manager -> IO (Either String Intraday)
getIntraday equityName apiKey manager = do
  let equityNameByteString = Data.ByteString.Char8.pack equityName
  let apiKeyByteString = Data.ByteString.Char8.pack apiKey
  let request = Network.HTTP.Simple.setRequestMethod "GET"
              $ Network.HTTP.Simple.setRequestSecure True
              $ Network.HTTP.Simple.setRequestPort 443
              $ Network.HTTP.Simple.setRequestQueryString
                  [ ("function", Just "TIME_SERIES_INTRADAY")
                  , ("symbol", Just equityNameByteString)
                  , ("interval", Just "1min")
                  , ("apikey", Just apiKeyByteString)
                  ]
              $ AlphaVantage.Config.baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String Intraday)
