{-# LANGUAGE MultilineStrings #-}

module IntegrationSpec where

import Data.Map qualified as Map
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime)
import System.Directory (doesFileExist, withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec hiding (shouldSatisfy)
import Test.Predicates
import Test.Predicates.HUnit (shouldSatisfy)

import Forecast qualified
import Program

spec = do
    describe "running in-memory" $ do
        it "writes the forecast to an html file" $ do
            runPure fixedTime websites program
                `shouldSatisfy` right (elemsAre [zipP (eq "../out/new/index.html") (hasSubstr "Lyme Regis to Lands End")])

    describe "running for real" $ do
        around_ inTempDir $ do
            it "writes the forecast to an html file" $ do
                runAll program

                fileExists <- doesFileExist "../out/new/index.html"
                fileExists `shouldBe` True

                content <- readFile "../out/new/index.html"
                content `shouldSatisfy` (hasSubstr "<!DOCTYPE html>" `andP` hasSubstr "Lyme Regis to Lands End")

inTempDir action = withSystemTempDirectory "weather-test" $ \dir -> withCurrentDirectory dir action

fixedTime :: UTCTime
fixedTime = UTCTime (fromOrdinalDate 0 1) (secondsToDiffTime 0)

websites = Map.fromList [(Forecast.inshoreWatersUrl, realisticHtml)]
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
