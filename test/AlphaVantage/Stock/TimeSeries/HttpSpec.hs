module AlphaVantage.Stock.TimeSeries.HttpSpec
  ( main
  , spec
  ) where

import Data.Map
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.Stock.TimeSeries

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

createManager :: IO Manager
createManager = newManager tlsManagerSettings

spec :: Spec
spec = do
  beforeAll createManager $ do
    parallel $ do
      describe "HTTP" $ do

        it "Intraday" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getIntraday "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.intradayTimeSeries result) > 0) `shouldBe` True

        it "Daily" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getDaily "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.dailyTimeSeries result) > 0) `shouldBe` True

        it "DailyAdjusted" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getDailyAdjusted "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.dailyAdjustedTimeSeries result) > 0) `shouldBe` True

        it "Weekly" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getWeekly "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.weeklyTimeSeries result) > 0) `shouldBe` True

        it "WeeklyAdjusted" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getWeeklyAdjusted "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.weeklyAdjustedTimeSeries result) > 0) `shouldBe` True

        it "Monthly" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getMonthly "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.monthlyTimeSeries result) > 0) `shouldBe` True

        it "MonthlyAdjusted" $ \manager -> do
          result' <- AlphaVantage.Stock.TimeSeries.getMonthlyAdjusted "MSFT" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.Stock.TimeSeries.monthlyAdjustedTimeSeries result) > 0) `shouldBe` True
