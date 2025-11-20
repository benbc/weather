{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Forecast (inshoreWatersUrl, parse, AreaForecast (..), ForecastPeriod (..)) where

import GHC.Generics (Generic)
import Text.HTML.Scalpel

data ForecastPeriod = ForecastPeriod
  { wind :: String,
    sea :: String,
    weather :: String,
    visibility :: String
  }
  deriving (Show, Eq, Generic)

data AreaForecast = AreaForecast
  { areaName :: String,
    current24Hours :: ForecastPeriod,
    next24Hours :: ForecastPeriod
  }
  deriving (Show, Eq, Generic)

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

parse :: String -> Maybe AreaForecast
parse page = scrapeStringLike page forecastScraper

forecastScraper :: Scraper String AreaForecast
forecastScraper = chroot (areas // area8) $ do
  name <- text "h2"
  [current, next] <- chroots forecastInfo forecastPeriodScraper
  return $ AreaForecast name current next
  where
    areas = "div" @: ["id" @= "inshore-waters-areas"]
    area8 = "section" @: ["aria-labelledby" @= "area8"]
    forecastInfo = "div" @: ["class" @= "forecast-info"]

forecastPeriodScraper :: Scraper String ForecastPeriod
forecastPeriodScraper = do
  [wind, sea, weather, visibility] <- texts "dd"
  return $ ForecastPeriod wind sea weather visibility
