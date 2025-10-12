{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import Polysemy (Embed, Member, Sem, run, runM)
import Polysemy.Error.Extended (Error, errorToIOError, runError)
import Polysemy.Reader (Reader, runReader)
import Polysemy.Trace (Trace, runTraceList, trace, traceToStderr)

import Effects.Curl (Curl, curl, curlFromMap, curlToIO)

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

program :: (Member Trace r, Member Curl r) => Sem r ()
program = do
    trace "Starting"
    page <- curl inshoreWatersUrl
    trace $ take 150 $ show page
    return ()

runAll :: Sem [Trace, Curl, Error String, Embed IO] a -> IO a
runAll = runM . errorToIOError . curlToIO . traceToStderr

runPure :: Map String ByteString -> Sem [Trace, Curl, Reader (Map String ByteString), Error String] a -> Either String ([String], a)
runPure websites = run . runError . runReader websites . curlFromMap . runTraceList

main :: IO ()
main = do
    runAll program
    case runPure websites program of
        Left err -> putStrLn $ "Error: " ++ err
        Right (traces, ()) -> putStrLn $ show traces
  where
    websites = Map.fromList [(inshoreWatersUrl, "bungo")]
