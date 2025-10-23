{-# LANGUAGE OverloadedStrings #-}

module Forecast (inshoreWatersUrl, parse) where

import Text.HTML.Scalpel

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

parse :: String -> Maybe String
parse page = scrapeStringLike page forecastScraper

forecastScraper :: Scraper String String
forecastScraper = html $ areas // area8
  where
    areas = "div" @: ["id" @= "inshore-waters-areas"]
    area8 = "section" @: ["aria-labelledby" @= "area8"]
