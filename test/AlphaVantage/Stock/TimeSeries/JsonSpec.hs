module AlphaVantage.Stock.TimeSeries.JsonSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Data.Aeson
import Data.ByteString.Lazy
import Data.Map
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.Stock.TimeSeries

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "should parse" $ do

    it "Intraday" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/Intraday.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.Intraday
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.intradayTimeSeries result) > 0) `shouldBe` True

    it "Daily" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/Daily.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.Daily
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.dailyTimeSeries result) > 0) `shouldBe` True

    it "DailyAdjusted" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/DailyAdjusted.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.DailyAdjusted
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.dailyAdjustedTimeSeries result) > 0) `shouldBe` True

    it "Weekly" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/Weekly.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.Weekly
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.weeklyTimeSeries result) > 0) `shouldBe` True

    it "WeeklyAdjusted" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/WeeklyAdjusted.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.WeeklyAdjusted
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.weeklyAdjustedTimeSeries result) > 0) `shouldBe` True

    it "Monthly" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/Monthly.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.Monthly
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.monthlyTimeSeries result) > 0) `shouldBe` True

    it "MonthlyAdjusted" $ do
      respData <- Data.ByteString.Lazy.readFile "testdata/AlphaVantage/Stock/TimeSeries/MonthlyAdjusted.json"
      let result' = Data.Aeson.eitherDecode respData :: Either String AlphaVantage.Stock.TimeSeries.MonthlyAdjusted
      case result' of
        Left errMsg -> fail errMsg
        Right result ->
          ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.monthlyAdjustedTimeSeries result) > 0) `shouldBe` True
