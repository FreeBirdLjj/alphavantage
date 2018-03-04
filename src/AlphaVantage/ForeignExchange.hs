#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.ForeignExchange
  ( CurrencyExchangeRate
  , realtimeCurrencyExchangeRateValue
  , currencyExchangeRateRealtimeData
  , getCurrencyExchangeRate
  ) where

import qualified AlphaVantage.Config
import           Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime
import qualified Network.HTTP.Client
import qualified Network.HTTP.Simple

data RealtimeCurrencyExchangeRate = RealtimeCurrencyExchangeRate
  { fromCurrencyCode :: String
  , fromCurrencyName :: String
  , toCurrencyCode :: String
  , toCurrencyName :: String
  , exchangeRate :: Float
  , lastRefreshed :: Data.Time.LocalTime.LocalTime
  , timeZone :: String
  } deriving (Eq, Ord, Read, Show)

realtimeCurrencyExchangeRateValue :: RealtimeCurrencyExchangeRate -> Float
realtimeCurrencyExchangeRateValue = exchangeRate

instance Data.Aeson.FromJSON RealtimeCurrencyExchangeRate where
  parseJSON (Data.Aeson.Object v) = RealtimeCurrencyExchangeRate
    <$> v .: "1. From_Currency Code"
    <*> v .: "2. From_Currency Name"
    <*> v .: "3. To_Currency Code"
    <*> v .: "4. To_Currency Name"
    <*> (fmap read $ v .: "5. Exchange Rate")
    <*> v .: "6. Last Refreshed"
    <*> v .: "7. Time Zone"

data CurrencyExchangeRate = CurrencyExchangeRate
  { realtimeData :: RealtimeCurrencyExchangeRate
  } deriving (Eq, Ord, Read, Show)

instance Data.Aeson.FromJSON CurrencyExchangeRate where
  parseJSON (Data.Aeson.Object v) = CurrencyExchangeRate
    <$> v .: "Realtime Currency Exchange Rate"

currencyExchangeRateRealtimeData :: CurrencyExchangeRate -> RealtimeCurrencyExchangeRate
currencyExchangeRateRealtimeData = realtimeData

-- API Parameters
-- Required: function
--   The function of your choice. In this case, function=CURRENCY_EXCHANGE_RATE
-- Required: from_currency
--   The currency you would like to get the exchange rate for. It can either be a physical currency or digital/crypto currency. For example: from_currency=USD or from_currency=BTC.
-- Required: to_currency
--   The destination currency for the exchange rate. It can either be a physical currency or digital/crypto currency. For example: to_currency=USD or to_currency=BTC.
-- Required: apikey
--   Your API key.
getCurrencyExchangeRate :: String -> String -> String -> Network.HTTP.Client.Manager -> IO (Either String CurrencyExchangeRate)
getCurrencyExchangeRate fromCurrencyCode toCurrencyCode apiKey manager = do
  let fromCurrencyCodeByteString = Data.ByteString.Char8.pack fromCurrencyCode
  let toCurrencyCodeByteString = Data.ByteString.Char8.pack toCurrencyCode
  let apiKeyByteString = Data.ByteString.Char8.pack apiKey
  let request = Network.HTTP.Simple.setRequestMethod "GET"
              $ Network.HTTP.Simple.setRequestSecure True
              $ Network.HTTP.Simple.setRequestPort 443
              $ Network.HTTP.Simple.setRequestQueryString
                  [ ("function", Just "CURRENCY_EXCHANGE_RATE")
                  , ("from_currency", Just fromCurrencyCodeByteString)
                  , ("to_currency", Just toCurrencyCodeByteString)
                  , ("apikey", Just apiKeyByteString)
                  ]
              $ AlphaVantage.Config.baseURL

  response <- Network.HTTP.Client.httpLbs request manager
  return (Data.Aeson.eitherDecode (Network.HTTP.Client.responseBody response) :: Either String CurrencyExchangeRate)
