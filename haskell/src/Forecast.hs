{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Forecast (inshoreWatersUrl, parse, AreaForecast (..), ForecastPeriod (..)) where

import GHC.Generics (Generic)
import Text.HTML.Scalpel hiding (match)
import Text.Regex.TDFA ((=~))

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
  title <- text "h2"
  [current, next] <- chroots forecastInfo forecastPeriodScraper
  return $ AreaForecast (stripAreaNumber title) current next
  where
    areas = "div" @: ["id" @= "inshore-waters-areas"]
    area8 = "section" @: ["aria-labelledby" @= "area8"]
    forecastInfo = "div" @: ["class" @= "forecast-info"]
    stripAreaNumber title = name
      where
        (name, _ :: String, _ :: String) = title =~ (" \\([0-9]+\\)$" :: String)

forecastPeriodScraper :: Scraper String ForecastPeriod
forecastPeriodScraper = do
  [wind, sea, weather, visibility] <- texts "dd"
  return $ ForecastPeriod wind sea weather visibility
