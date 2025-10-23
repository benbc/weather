module Main where

import qualified Data.Map as Map

import qualified Forecast

import Program

main :: IO ()
main = do
    runAll program
    case runPure websites program of
        Left err -> putStrLn $ "Error: " ++ err
        Right (traces, ()) -> putStrLn $ show traces
  where
    websites =
        Map.fromList
            [
                ( Forecast.inshoreWatersUrl
                , "<div id='inshore-waters-areas'><section aria-labelledby='area8'>some forecast</section></div>"
                )
            ]
