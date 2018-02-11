module AlphaVantage.ForeignExchange.JsonSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Map
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.ForeignExchange

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "should parse" $ do

    it "CurrencyExchangeRate" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/CurrencyExchangeRate.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.ForeignExchange.CurrencyExchangeRate
      case result' of
        Left errMsg -> fail errMsg
        Right result -> do
          let currencyData = AlphaVantage.ForeignExchange.currencyExchangeRateRealtimeData result
          let currencyRate = AlphaVantage.ForeignExchange.realtimeCurrencyExchangeRateValue currencyData
          (currencyRate > 0.0) `shouldBe` True
