{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import Polysemy (Embed, Member, Sem, run, runM)
import Polysemy.Error.Extended (Error)
import qualified Polysemy.Error.Extended as Error
import Polysemy.Reader.Extended (Reader)
import qualified Polysemy.Reader.Extended as Reader
import Polysemy.Trace.Extended (Trace, trace)
import qualified Polysemy.Trace.Extended as Trace

import Effects.Curl (Curl, curl)
import qualified Effects.Curl as Curl

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

program :: (Member Trace r, Member Curl r) => Sem r ()
program = do
    trace "Starting"
    page <- curl inshoreWatersUrl
    trace $ take 150 $ show page
    return ()

runAll :: Sem [Trace, Curl, Error String, Embed IO] a -> IO a
runAll = runM . Error.runToIO . Curl.runToIOError . Trace.runToIO

runPure :: Map String ByteString -> Sem [Trace, Curl, Reader (Map String ByteString), Error String] a -> Either String ([String], a)
runPure websites = run . Error.runToEither . Reader.run websites . Curl.runToReaderError . Trace.runToList

main :: IO ()
main = do
    runAll program
    case runPure websites program of
        Left err -> putStrLn $ "Error: " ++ err
        Right (traces, ()) -> putStrLn $ show traces
  where
    websites = Map.fromList [(inshoreWatersUrl, "bungo")]
