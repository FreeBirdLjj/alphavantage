#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.DigitalCurrency.Intraday
  ( Intraday
  , intradayMetadata
  , intradayTimeSeries
  , getIntraday
  ) where

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
  , interval :: String
  , lastRefreshed :: Data.Time.LocalTime.LocalTime
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON MetaData where
  parseJSON (Data.Aeson.Object v) = MetaData
    <$> v .: "1. Information"
    <*> v .: "2. Digital Currency Code"
    <*> v .: "3. Digital Currency Name"
    <*> v .: "4. Market Code"
    <*> v .: "5. Market Name"
    <*> v .: "6. Interval"
    <*> v .: "7. Last Refreshed"
    <*> v .: "8. Time Zone"

data TimePoint = TimePoint
  { price :: Float
  , volume :: Float
  , marketCap :: Float
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON TimePoint where
  parseJSON = Data.Aeson.withObject "TimePoint" $ \o -> do
    let hm = Data.HashMap.Strict.toList o
    price <- case
      Data.List.find
        (Data.Text.isPrefixOf (Data.Text.pack "1a. price") . fst)
        hm
      of
        Just (k, _) -> fmap read $ o .: k
        Nothing -> fail "key for price not present"
    volume <- fmap read $ o .: "2. volume"
    marketCap <- fmap read $ o .: "3. market cap (USD)"
    return $ TimePoint price volume marketCap

data Intraday = Intraday
  { metadata :: MetaData
  , timeSeries :: Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
  } deriving (Eq, Read, Show)

instance Data.Aeson.FromJSON Intraday where
  parseJSON (Data.Aeson.Object v) = Intraday
    <$> v .: "Meta Data"
    <*> v .: "Time Series (Digital Currency Intraday)"

intradayMetadata :: Intraday -> MetaData
intradayMetadata = metadata

intradayTimeSeries :: Intraday -> Data.Map.Lazy.Map Data.Time.LocalTime.LocalTime TimePoint
intradayTimeSeries = timeSeries

baseURL = "https://www.alphavantage.co/query"

-- API Parameters
-- Required: function
--   The time series of your choice. In this case, function=DIGITAL_CURRENCY_INTRADAY
-- Required: symbol
--   The digital/crypto currency of your choice. It can be any of the currencies in the digital currency list. For example: symbol=BTC.
-- Required: market
--   The exchange market of your choice. It can be any of the market in the market list. For example: market=CNY.
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
                [ ("function", Just "DIGITAL_CURRENCY_INTRADAY")
                , ("symbol", Just digitalCurrencyNameByteString)
                , ("market", Just marketNameByteString)
                , ("apikey", Just apiKeyByteString)
                ]
                baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String Intraday)
