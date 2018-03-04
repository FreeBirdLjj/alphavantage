#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.Stock.TimeSeries.MonthlyAdjusted
  ( MonthlyAdjusted
  , monthlyAdjustedMetadata
  , monthlyAdjustedTimeSeries
  , getMonthlyAdjusted
  ) where

import qualified AlphaVantage.Config
import           Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import qualified Data.Map.Lazy
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Network.HTTP.Client
import qualified Network.HTTP.Simple

data MetaData = MetaData
  { information :: String
  , symbol :: String
  , lastRefreshed :: Data.Time.Calendar.Day
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON MetaData where
  parseJSON (Data.Aeson.Object v) = MetaData
    <$> v .: "1. Information"
    <*> v .: "2. Symbol"
    <*> v .: "3. Last Refreshed"
    <*> v .: "4. Time Zone"

data TimePoint = TimePoint
  { open :: Float
  , high :: Float
  , low :: Float
  , close :: Float
  , adjustedClose :: Float
  , volume :: Integer
  , dividendAmount :: Float
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON TimePoint where
  parseJSON (Data.Aeson.Object v) = TimePoint
    <$> (fmap read $ v .: "1. open")
    <*> (fmap read $ v .: "2. high")
    <*> (fmap read $ v .: "3. low")
    <*> (fmap read $ v .: "4. close")
    <*> (fmap read $ v .: "5. adjusted close")
    <*> (fmap read $ v .: "6. volume")
    <*> (fmap read $ v .: "7. dividend amount")

data MonthlyAdjusted = MonthlyAdjusted
  { metadata :: MetaData
  , timeSeries :: Data.Map.Lazy.Map Data.Time.Calendar.Day TimePoint
  } deriving (Eq, Read, Show)

instance Data.Aeson.FromJSON MonthlyAdjusted where
  parseJSON = Data.Aeson.withObject "MonthlyAdjusted" $ \o -> do
    metadata   <- o .: "Meta Data"
    timeSeries <- o .: "Monthly Adjusted Time Series"
    return $ MonthlyAdjusted metadata timeSeries

monthlyAdjustedMetadata :: MonthlyAdjusted -> MetaData
monthlyAdjustedMetadata = metadata

monthlyAdjustedTimeSeries :: MonthlyAdjusted -> Data.Map.Lazy.Map Data.Time.Calendar.Day TimePoint
monthlyAdjustedTimeSeries = timeSeries

-- API Parameters
-- Required: function
--   The time series of your choice. In this case, function=TIME_SERIES_DAILY_ADJUSTED
-- Required: symbol
--   The name of the equity of your choice. For example: symbol=MSFT
-- Optional: outputsize
--   By default, outputsize=compact. Strings compact and full are accepted with the following specifications: compact returns only the latest 100 data points; full returns the full-length time series of up to 20 years of historical data. The "compact" option is recommended if you would like to reduce the data size of each API call.
-- Optional: datatype
--   By default, datatype=json. Strings json and csv are accepted with the following specifications: json returns the monthly time series in JSON format; csv returns the time series as a CSV (comma separated value) file.
-- Required: apikey
--   Your API key.
getMonthlyAdjusted :: String -> String -> Network.HTTP.Client.Manager -> IO (Either String MonthlyAdjusted)
getMonthlyAdjusted equityName apiKey manager = do
  let equityNameByteString = Data.ByteString.Char8.pack equityName
  let apiKeyByteString = Data.ByteString.Char8.pack apiKey
  let request = Network.HTTP.Simple.setRequestMethod "GET"
              $ Network.HTTP.Simple.setRequestSecure True
              $ Network.HTTP.Simple.setRequestPort 443
              $ Network.HTTP.Simple.setRequestQueryString
                  [ ("function", Just "TIME_SERIES_MONTHLY_ADJUSTED")
                  , ("symbol", Just equityNameByteString)
                  , ("apikey", Just apiKeyByteString)
                  ]
              $ AlphaVantage.Config.baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String MonthlyAdjusted)
