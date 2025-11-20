{-# LANGUAGE MultilineStrings #-}

module ForecastSpec where

import Forecast
import Test.Hspec

spec = do
  describe "forecast" $ do
    it "extracts complete forecast data" $ do
      parse html `shouldBe` Just expected
    it "fails when there is only one forecast period" $ do
      parse htmlWithOnePeriod `shouldBe` Nothing
  where
    expected =
      AreaForecast
        { areaName = "Lyme Regis to Lands End including the Isles of Scilly",
          current24Hours =
            ForecastPeriod
              { wind = "Southwest 5",
                sea = "Moderate",
                weather = "Rain",
                visibility = "Good"
              },
          next24Hours =
            ForecastPeriod
              { wind = "West 4",
                sea = "Slight",
                weather = "Fair",
                visibility = "Good"
              }
        }

html =
  """
  <div id='inshore-waters-areas'>
  <section aria-labelledby="area8">
  <h2>Lyme Regis to Lands End including the Isles of Scilly (8)</h2>
  <div class="forecast-info">
  <dl>
  <dt>Wind</dt>
  <dd>Southwest 5</dd>
  <dt>Sea state</dt>
  <dd>Moderate</dd>
  <dt>Weather</dt>
  <dd>Rain</dd>
  <dt>Visibility</dt>
  <dd>Good</dd>
  </dl>
  </div>
  <div class="forecast-info">
  <dl>
  <dt>Wind</dt>
  <dd>West 4</dd>
  <dt>Sea state</dt>
  <dd>Slight</dd>
  <dt>Weather</dt>
  <dd>Fair</dd>
  <dt>Visibility</dt>
  <dd>Good</dd>
  </dl>
  </div>
  </section>
  </div>
  """

htmlWithOnePeriod =
  """
  <div id='inshore-waters-areas'>
  <section aria-labelledby="area8">
  <h2>Lyme Regis to Lands End including the Isles of Scilly (8)</h2>
  <div class="forecast-info">
  <dl>
  <dt>Wind</dt>
  <dd>Southwest 5</dd>
  <dt>Sea state</dt>
  <dd>Moderate</dd>
  <dt>Weather</dt>
  <dd>Rain</dd>
  <dt>Visibility</dt>
  <dd>Good</dd>
  </dl>
  </div>
  </section>
  </div>
  """
