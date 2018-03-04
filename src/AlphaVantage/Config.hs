#!/usr/bin/env stack
-- stack script
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.Config
  ( baseURL
  ) where

import qualified Network.HTTP.Simple

baseURL :: Network.HTTP.Simple.Request
baseURL = "https://www.alphavantage.co/query"
