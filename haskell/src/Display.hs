{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Display (formatHtml) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Format qualified as TimeFormat
import Forecast qualified
import Text.Mustache qualified as Mustache

instance Aeson.ToJSON Forecast.ForecastPeriod

instance Aeson.ToJSON Forecast.AreaForecast

formatHtml :: UTCTime -> Forecast.AreaForecast -> Either String String
formatHtml lastUpdated forecast = bimap show (bind values) compiled
  where
    compiled = Mustache.compileTemplate "forecast" forecastTemplate
    bind values' template = Text.unpack $ Mustache.substitute template values'
    values = Aeson.object ["forecast" .= forecast, "lastUpdated" .= formatted lastUpdated]
    formatted = TimeFormat.formatTime TimeFormat.defaultTimeLocale TimeFormat.rfc822DateFormat

forecastTemplate :: Text
forecastTemplate =
  """
  <!DOCTYPE html>
  <html>
      <head>
          <meta charset="UTF-8">
          <title>Weather</title>
      </head>
      <body>
          <h1>{{forecast.areaName}}</h1>
          <h2>24 Hour Forecast</h2>
          <dl>
              <dt>Wind</dt>
              <dd>{{forecast.current24Hours.wind}}</dd>
              <dt>Sea State</dt>
              <dd>{{forecast.current24Hours.sea}}</dd>
              <dt>Weather</dt>
              <dd>{{forecast.current24Hours.weather}}</dd>
              <dt>Visibility</dt>
              <dd>{{forecast.current24Hours.visibility}}</dd>
          </dl>
          <h2>Next 24 Hours</h2>
          <dl>
              <dt>Wind</dt>
              <dd>{{forecast.next24Hours.wind}}</dd>
              <dt>Sea State</dt>
              <dd>{{forecast.next24Hours.sea}}</dd>
              <dt>Weather</dt>
              <dd>{{forecast.next24Hours.weather}}</dd>
              <dt>Visibility</dt>
              <dd>{{forecast.next24Hours.visibility}}</dd>
          </dl>
          <p>Last updated: {{lastUpdated}}</p>
      </body>
  </html>
  """
