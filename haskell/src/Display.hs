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
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>Weather Aggregator</title>
          <style>
              body {
                  font-family: Arial, sans-serif;
                  max-width: 1200px;
                  margin: 0 auto;
                  padding: 15px;
                  line-height: 1.4;
                  font-size: 14px;
              }
              .header {
                  text-align: center;
                  margin-bottom: 20px;
              }
              .header h1 {
                  margin: 0 0 5px 0;
                  font-size: 1.8em;
                  color: #2c3e50;
              }
              .forecast-content {
                  background: #f9f9f9;
                  padding: 15px;
                  border-radius: 4px;
                  margin-bottom: 15px;
                  border-left: 4px solid #3498db;
              }
              .forecast-content h2 {
                  color: #2c3e50;
                  margin: 0 0 8px 0;
                  font-size: 1.4em;
              }
              .forecast-content h3 {
                  color: #2c3e50;
                  margin: 12px 0 8px 0;
                  font-size: 1.1em;
              }
              .forecast-content p {
                  margin: 6px 0;
                  color: #34495e;
              }
              .forecast-content strong {
                  color: #2c3e50;
              }
              .last-updated {
                  text-align: center;
                  font-size: 0.75em;
                  color: #888;
                  margin-top: 20px;
              }
              @media (max-width: 768px) {
                  body {
                      padding: 10px;
                      font-size: 13px;
                  }
                  .header h1 {
                      font-size: 1.6em;
                  }
                  .forecast-content {
                      padding: 12px;
                      margin-bottom: 12px;
                  }
              }
          </style>
      </head>
      <body>
          <div class="header">
              <h1>Weather Aggregator</h1>
          </div>
          <div class="forecast-content">
              <h2>{{forecast.areaName}}</h2>
              <h3>24 Hour Forecast</h3>
              <p><strong>Wind:</strong> {{forecast.current24Hours.wind}}</p>
              <p><strong>Sea state:</strong> {{forecast.current24Hours.sea}}</p>
              <p>
                <strong>Weather:</strong> {{forecast.current24Hours.weather}}
                <strong>Visibility:</strong> {{forecast.current24Hours.visibility}}
              </p>
              <h3>Next 24 Hours</h3>
              <p><strong>Wind:</strong> {{forecast.next24Hours.wind}}</p>
              <p><strong>Sea state:</strong> {{forecast.next24Hours.sea}}</p>
              <p>
                <strong>Weather:</strong> {{forecast.next24Hours.weather}}
                <strong>Visibility:</strong> {{forecast.next24Hours.visibility}}
              </p>
          </div>
          <div class="last-updated">Last updated: {{lastUpdated}}</div>
      </body>
  </html>
  """
