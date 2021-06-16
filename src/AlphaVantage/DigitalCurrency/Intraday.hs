#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.DigitalCurrency.Intraday
  ( Intraday
  , intradayMetadata
  , intradayTimeSeries
  , getIntraday
  ) where

import qualified AlphaVantage.Config
import           Data.Aeson
import qualified Data.ByteString.Char8
import qualified Data.HashMap.Strict
import qualified Data.List
import qualified Data.Map.Lazy
import qualified Data.Text
import qualified Data.Time.LocalTime
import qualified Network.HTTP.Client
import qualified Network.HTTP.Simple

data MetaData = MetaData
  { information :: String
  , digitalCurrencyCode :: String
  , digitalCurrencyName :: String
  , marketCode :: String
  , marketName :: String
  , lastRefreshed :: Data.Time.LocalTime.LocalTime
  , interval :: String
  , outputSize :: String
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON MetaData where
  parseJSON (Data.Aeson.Object v) = MetaData
    <$> v .: "1. Information"
    <*> v .: "2. Digital Currency Code"
    <*> v .: "3. Digital Currency Name"
    <*> v .: "4. Market Code"
    <*> v .: "5. Market Name"
    <*> v .: "6. Last Refreshed"
    <*> v .: "7. Interval"
    <*> v .: "8. Output Size"
    <*> v .: "9. Time Zone"

data TimePoint = TimePoint
  { open :: Float
  , high :: Float
  , low :: Float
  , close :: Float
  , volume :: Int
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON TimePoint where
  parseJSON (Data.Aeson.Object v) = TimePoint
    <$> (fmap read $ v .: "1. open")
    <*> (fmap read $ v .: "2. high")
    <*> (fmap read $ v .: "3. low")
    <*> (fmap read $ v .: "4. close")
    <*> v .: "5. volume"

data Intraday = Intraday
  { metadata :: MetaData
  , timeSeries :: Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
  } deriving (Eq, Read, Show)

instance Data.Aeson.FromJSON Intraday where
  parseJSON (Data.Aeson.Object v) = Intraday
    <$> v .: "Meta Data"
    <*> v .: "Time Series Crypto (5min)"

intradayMetadata :: Intraday -> MetaData
intradayMetadata = metadata

intradayTimeSeries :: Intraday -> Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
intradayTimeSeries = timeSeries

-- API Parameters
-- Required: function
--   The time series of your choice. In this case, function=CRYPTO_INTRADAY
-- Required: symbol
--   The digital/crypto currency of your choice. It can be any of the currencies in the digital currency list. For example: symbol=BTC.
-- Required: market
--   The exchange market of your choice. It can be any of the market in the market list. For example: market=CNY.
-- Required: interval
--   Time interval between two consecutive data points in the time series. The following values are supported: 1min, 5min, 15min, 30min, 60min
-- Required: apikey
--   Your API key.
getIntraday :: String -> String -> String -> Network.HTTP.Client.Manager -> IO (Either String Intraday)
getIntraday digitalCurrencyName marketName apiKey manager = do
  let digitalCurrencyNameByteString = Data.ByteString.Char8.pack digitalCurrencyName
  let marketNameByteString = Data.ByteString.Char8.pack marketName
  let apiKeyByteString = Data.ByteString.Char8.pack apiKey
  let request = Network.HTTP.Simple.setRequestMethod "GET"
              $ Network.HTTP.Simple.setRequestSecure True
              $ Network.HTTP.Simple.setRequestPort 443
              $ Network.HTTP.Simple.setRequestQueryString
                [ ("function", Just "CRYPTO_INTRADAY")
                , ("symbol", Just digitalCurrencyNameByteString)
                , ("market", Just marketNameByteString)
                , ("interval", Just "5min")
                , ("apikey", Just apiKeyByteString)
                ]
              $ AlphaVantage.Config.baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String Intraday)
