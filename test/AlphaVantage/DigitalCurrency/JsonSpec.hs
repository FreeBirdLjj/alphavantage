module AlphaVantage.DigitalCurrency.JsonSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Map
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.DigitalCurrency

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "should parse" $ do

    it "Intraday" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/DigitalCurrency/Intraday.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.DigitalCurrency.Intraday
      case result' of
        Left errMsg -> fail errMsg
        Right result -> do
          ((Data.Map.size $ AlphaVantage.DigitalCurrency.intradayTimeSeries result) > 0) `shouldBe` True

    it "Daily" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/DigitalCurrency/Daily.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.DigitalCurrency.Daily
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.DigitalCurrency.dailyTimeSeries result) > 0) `shouldBe` True

    it "Weekly" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/DigitalCurrency/Weekly.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.DigitalCurrency.Weekly
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.DigitalCurrency.weeklyTimeSeries result) > 0) `shouldBe` True

    it "Monthly" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/DigitalCurrency/Monthly.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.DigitalCurrency.Monthly
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.DigitalCurrency.monthlyTimeSeries result) > 0) `shouldBe` True
