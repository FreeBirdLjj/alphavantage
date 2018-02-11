#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.DigitalCurrency.Daily
  ( Daily
  , dailyMetadata
  , dailyTimeSeries
  , getDaily
  ) where

import           Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import qualified Data.HashMap.Strict
import qualified Data.List
import qualified Data.Map.Lazy
import qualified Data.Text
import qualified Data.Time.Calendar
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
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON MetaData where
  parseJSON (Data.Aeson.Object v) = MetaData
    <$> v .: "1. Information"
    <*> v .: "2. Digital Currency Code"
    <*> v .: "3. Digital Currency Name"
    <*> v .: "4. Market Code"
    <*> v .: "5. Market Name"
    <*> (fmap (read . Data.Text.unpack . Data.Text.replace (Data.Text.pack "(end of day)") (Data.Text.pack "23:59:59")) $ v .: "6. Last Refreshed")
    <*> v .: "7. Time Zone"

data TimePoint = TimePoint
  { open :: Float
  , high :: Float
  , low :: Float
  , close :: Float
  , volume :: Integer
  , marketCap :: Float
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON TimePoint where
  parseJSON = Data.Aeson.withObject "TimePoint" $ \o -> do
    let hm = Data.HashMap.Strict.toList o
    open <- case
      Data.List.find
        (Data.Text.isPrefixOf (Data.Text.pack "1a. open") . fst)
        hm
      of
        Just (k, _) -> fmap read $ o .: k
        Nothing -> fail "key for open not present"
    high <- case
      Data.List.find
        (Data.Text.isPrefixOf (Data.Text.pack "2a. high") . fst)
        hm
      of
        Just (k, _) -> fmap read $ o .: k
        Nothing -> fail "key for high not present"
    low <- case
      Data.List.find
        (Data.Text.isPrefixOf (Data.Text.pack "3a. low") . fst)
        hm
      of
        Just (k, _) -> fmap read $ o .: k
        Nothing -> fail "key for low not present"
    close <- case
      Data.List.find
        (Data.Text.isPrefixOf (Data.Text.pack "4a. close") . fst)
        hm
      of
        Just (k, _) -> fmap read $ o .: k
        Nothing -> fail "key for close not present"
    volume <- fmap read $ o .: "5. volume"
    marketCap <- fmap read $ o .: "6. market cap (USD)"
    return $ TimePoint open high low close volume marketCap

data Daily = Daily
  { metadata :: MetaData
  , timeSeries :: Data.Map.Lazy.Map Data.Time.Calendar.Day TimePoint
  } deriving (Eq, Read, Show)

instance Data.Aeson.FromJSON Daily where
  parseJSON (Data.Aeson.Object v) = Daily
    <$> v .: "Meta Data"
    <*> v .: "Time Series (Digital Currency Daily)"

dailyMetadata :: Daily -> MetaData
dailyMetadata = metadata

dailyTimeSeries :: Daily -> Data.Map.Lazy.Map Data.Time.Calendar.Day TimePoint
dailyTimeSeries = timeSeries

baseURL = "https://www.alphavantage.co/query"

-- API Parameters
-- Required: function
--   The time series of your choice. In this case, function=DIGITAL_CURRENCY_DAILY
-- Required: symbol
--   The digital/crypto currency of your choice. It can be any of the currencies in the digital currency list. For example: symbol=BTC.
-- Required: market
--   The exchange market of your choice. It can be any of the market in the market list. For example: market=CNY.
-- Required: apikey
--   Your API key.
getDaily :: String -> String -> String -> Network.HTTP.Client.Manager -> IO (Either String Daily)
getDaily digitalCurrencyName marketName apiKey manager = do
  let digitalCurrencyNameByteString = Data.ByteString.Char8.pack digitalCurrencyName
  let marketNameByteString = Data.ByteString.Char8.pack marketName
  let apiKeyByteString = Data.ByteString.Char8.pack apiKey
  let request = Network.HTTP.Simple.setRequestMethod "GET"
              $ Network.HTTP.Simple.setRequestSecure True
              $ Network.HTTP.Simple.setRequestPort 443
              $ Network.HTTP.Simple.setRequestQueryString
                  [ ("function", Just "DIGITAL_CURRENCY_DAILY")
                  , ("symbol", Just digitalCurrencyNameByteString)
                  , ("market", Just marketNameByteString)
                  , ("apikey", Just apiKeyByteString)
                  ]
              $ baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String Daily)
