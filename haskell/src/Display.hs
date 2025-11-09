{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Display (formatHtml) where

import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap)
import Data.Text qualified as Text
import Data.Text (Text)
import Forecast qualified
import Text.Mustache qualified as Mustache

instance Aeson.ToJSON Forecast.ForecastPeriod
instance Aeson.ToJSON Forecast.AreaForecast

formatHtml :: Forecast.AreaForecast -> Either String String
formatHtml forecast = bimap show (bind forecast) compiled
    where
        compiled = Mustache.compileTemplate "forecast" forecastTemplate
        bind values template = Text.unpack $ Mustache.substitute template $ Aeson.toJSON values

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
            <h1>{{areaName}}</h1>
            <h2>24 Hour Forecast</h2>
            <dl>
                <dt>Wind</dt>
                <dd>{{current24Hours.wind}}</dd>
                <dt>Sea State</dt>
                <dd>{{current24Hours.sea}}</dd>
                <dt>Weather</dt>
                <dd>{{current24Hours.weather}}</dd>
                <dt>Visibility</dt>
                <dd>{{current24Hours.visibility}}</dd>
            </dl>
            <h2>Next 24 Hours</h2>
            <dl>
                <dt>Wind</dt>
                <dd>{{next24Hours.wind}}</dd>
                <dt>Sea State</dt>
                <dd>{{next24Hours.sea}}</dd>
                <dt>Weather</dt>
                <dd>{{next24Hours.weather}}</dd>
                <dt>Visibility</dt>
                <dd>{{next24Hours.visibility}}</dd>
            </dl>
        </body>
    </html>
    """
