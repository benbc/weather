{-# LANGUAGE MultilineStrings #-}

module MainSpec where

import Data.List qualified
import Data.Map qualified as Map
import Test.Hspec

import Forecast qualified
import Program

spec :: Spec
spec = do
    describe "integration test" $ do
        it "runs the program with test data" $ do
            case runPure websites program of
                Left err -> expectationFailure $ "Error: " ++ err
                Right (traces, fileWrites, ()) -> do
                    traces
                        `shouldBe` [ "Starting"
                                   , "AreaForecast {areaName = \"Lyme Regis to Lands End including the Isles of Scilly (8)\", current24Hours = ForecastPeriod {wind = \"West or southwest 5 to 7, occasionally 4 at first and gale 8 later.\", sea = \"Moderate or rough in east, rough or very rough in west.\", weather = \"Showers, squally or thundery at times.\", visibility = \"Moderate or good, occasionally poor.\"}, next24Hours = ForecastPeriod {wind = \"West backing southwest 5 or 6, occasionally 4 at first and 7 later.\", sea = \"Moderate or rough, occasionally very rough near the Isles of Scilly.\", weather = \"Showers.\", visibility = \"Good, occasionally moderate.\"}}"
                                   ]
                    case fileWrites of
                        [write] -> do
                            write `shouldSatisfy` \w ->
                                "output/index.html:" `Data.List.isPrefixOf` w
                                    && "Lyme Regis to Lands End including the Isles of Scilly (8)" `Data.List.isInfixOf` w
                        _ -> expectationFailure $ "Expected exactly 1 file write, got: " ++ show (length fileWrites)
  where
    websites =
        Map.fromList
            [
                ( Forecast.inshoreWatersUrl
                , realisticHtml
                )
            ]
    realisticHtml =
        """
        <div id='inshore-waters-areas'>
        <section aria-labelledby="area8" class="marine-card warning" data-value="inshore-waters-7">
        <h2 id="area8" class="card-name warning">Lyme Regis to Lands End including the Isles of Scilly (8)</h2>
        <div class="card-content">
        <p>Strong winds are forecast</p>
        <h3>24 hour forecast:</h3>
        <div class="forecast-info">
        <dl>
        <dt>Wind</dt>
        <dd>West or southwest 5 to 7, occasionally 4 at first and gale 8 later.</dd>
        <dt>Sea state</dt>
        <dd>Moderate or rough in east, rough or very rough in west.</dd>
        <dt>Weather</dt>
        <dd>Showers, squally or thundery at times.</dd>
        <dt>Visibility</dt>
        <dd>Moderate or good, occasionally poor.</dd>
        </dl>
        </div>
        <h3>Outlook for the following 24 hours:</h3>
        <div class="forecast-info">
        <dl>
        <dt>Wind</dt>
        <dd>West backing southwest 5 or 6, occasionally 4 at first and 7 later.</dd>
        <dt>Sea state</dt>
        <dd>Moderate or rough, occasionally very rough near the Isles of Scilly.</dd>
        <dt>Weather</dt>
        <dd>Showers.</dd>
        <dt>Visibility</dt>
        <dd>Good, occasionally moderate.</dd>
        </dl>
        </div>
        </div>
        </section>
        </div>
        """
