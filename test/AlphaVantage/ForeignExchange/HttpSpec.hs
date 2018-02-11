module AlphaVantage.ForeignExchange.HttpSpec
  ( main
  , spec
  ) where

import Data.Map
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Test.Hspec
import Test.QuickCheck

import AlphaVantage.ForeignExchange

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

        it "CurrencyExchangeRate" $ \manager -> do
          result' <- AlphaVantage.ForeignExchange.getCurrencyExchangeRate "USD" "JPY" "demo" manager
          case result' of
            Left errMsg -> fail errMsg
            Right result -> do
              let currencyData = AlphaVantage.ForeignExchange.currencyExchangeRateRealtimeData result
              let currencyRate = AlphaVantage.ForeignExchange.realtimeCurrencyExchangeRateValue currencyData
              (currencyRate > 0.0) `shouldBe` True
