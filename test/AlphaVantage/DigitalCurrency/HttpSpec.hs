module AlphaVantage.DigitalCurrency.HttpSpec
  ( main
  , spec
  ) where

import Data.Map
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.DigitalCurrency

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

createManager :: IO Manager
createManager = newManager tlsManagerSettings

spec :: Spec
spec =
  beforeAll createManager $
    parallel $ do
      describe "HTTP" $ do

        it "Intraday" $ \manager -> do
          result' <- AlphaVantage.DigitalCurrency.getIntraday "ETH" "USD" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result -> do
              ((Data.Map.size $ AlphaVantage.DigitalCurrency.intradayTimeSeries result) > 0) `shouldBe` True

        it "Daily" $ \manager -> do
          result' <- AlphaVantage.DigitalCurrency.getDaily "BTC" "CNY" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.DigitalCurrency.dailyTimeSeries result) > 0) `shouldBe` True

        it "Weekly" $ \manager -> do
          result' <- AlphaVantage.DigitalCurrency.getWeekly "BTC" "CNY" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.DigitalCurrency.weeklyTimeSeries result) > 0) `shouldBe` True

        it "Monthly" $ \manager -> do
          result' <- AlphaVantage.DigitalCurrency.getMonthly "BTC" "CNY" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result ->
              ((Data.Map.size $ AlphaVantage.DigitalCurrency.monthlyTimeSeries result) > 0) `shouldBe` True
