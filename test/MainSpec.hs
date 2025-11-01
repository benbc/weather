module MainSpec where

import qualified Data.Map as Map
import Test.Hspec

import qualified Forecast
import Program

spec :: Spec
spec = do
    describe "integration test" $ do
        it "runs the program with test data" $ do
            case runPure websites program of
                Left err -> expectationFailure $ "Error: " ++ err
                Right (traces, ()) -> traces `shouldBe` ["Starting", "<section aria-labelledby=\"area8\">some forecast</section>"]
  where
    websites =
        Map.fromList
            [
                ( Forecast.inshoreWatersUrl
                , "<div id='inshore-waters-areas'><section aria-labelledby='area8'>some forecast</section></div>"
                )
            ]
