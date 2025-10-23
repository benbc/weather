module ForecastSpec where

import Forecast
import Test.Hspec

spec :: Spec
spec = do
    describe "forecast" $ do
        it "does a thing" $ do
            parse "<div id='inshore-waters-areas'><section aria-labelledby='area8'>some forecast</section></div>" `shouldBe` Just "<section aria-labelledby=\"area8\">some forecast</section>"
