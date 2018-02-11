#!/usr/bin/env stack
-- stack script

module AlphaVantage.DigitalCurrency
  ( Intraday
  , intradayMetadata
  , intradayTimeSeries
  , getIntraday

  , Daily
  , dailyMetadata
  , dailyTimeSeries
  , getDaily

  , Weekly
  , weeklyMetadata
  , weeklyTimeSeries
  , getWeekly

  , Monthly
  , monthlyMetadata
  , monthlyTimeSeries
  , getMonthly
  ) where

import AlphaVantage.DigitalCurrency.Intraday (Intraday, intradayMetadata, intradayTimeSeries, getIntraday)
import AlphaVantage.DigitalCurrency.Daily (Daily, dailyMetadata, dailyTimeSeries, getDaily)
import AlphaVantage.DigitalCurrency.Weekly (Weekly, weeklyMetadata, weeklyTimeSeries, getWeekly)
import AlphaVantage.DigitalCurrency.Monthly (Monthly, monthlyMetadata, monthlyTimeSeries, getMonthly)
