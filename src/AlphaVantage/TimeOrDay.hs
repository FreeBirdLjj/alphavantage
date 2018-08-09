#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.TimeOrDay
  ( TimeOrDay
  ) where

import qualified Data.Aeson
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Data.Time.LocalTime

-- AlphaVantage returns the last refreshed time as a time point or a day
-- randomly, so we need a compliant type to process it with aeson.
data TimeOrDay = Time Data.Time.LocalTime.LocalTime
               | Day Data.Time.Calendar.Day
  deriving (Eq, Ord, Read, Show)

-- For example, "2018-08-08" has 10 chars.
instance Data.Aeson.FromJSON TimeOrDay where
  parseJSON = Data.Aeson.withText "TimeOrDay" $ \s ->
    if Data.Text.length s == 10
    then Day <$> (Data.Aeson.parseJSON $ Data.Aeson.String s)
    else Time <$> (Data.Aeson.parseJSON $ Data.Aeson.String s)
