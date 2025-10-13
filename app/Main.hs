{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
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

import Text.HTML.Scalpel

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

program :: (Member Trace r, Member Curl r, Member (Error String) r) => Sem r ()
program = do
    trace "Starting"
    page <- curl inshoreWatersUrl
    -- TODO: cleaner handling of different string types -- maybe use TagSoup's Text.StringLike? c.f. Scalpel's decoders
    let forecast = scrapeStringLike (unpack page) forecastScraper
    forecast' <- Error.note "couldn't parse forecast" forecast
    trace forecast'
    return ()

runAll :: Sem [Trace, Curl, Error String, Embed IO] a -> IO a
runAll = Trace.runToIO >>> Curl.runToIOError >>> Error.runToIO >>> runM

runPure :: Map String ByteString -> Sem [Trace, Curl, Reader (Map String ByteString), Error String] a -> Either String ([String], a)
runPure websites = Trace.runToList >>> Curl.runToReaderError >>> Reader.run websites >>> Error.runToEither >>> run

main :: IO ()
main = do
    runAll program
    case runPure websites program of
        Left err -> putStrLn $ "Error: " ++ err
        Right (traces, ()) -> putStrLn $ show traces
  where
    websites = Map.fromList [(inshoreWatersUrl, "<div id='inshore-waters-areas'><section aria-labelledby='area8'>some forecast</section></div>")]

forecastScraper :: Scraper String String
forecastScraper = text $ areas // area8
  where
    areas = "div" @: ["id" @= "inshore-waters-areas"]
    area8 = "section" @: ["aria-labelledby" @= "area8"]
