{-# LANGUAGE DataKinds #-}

module Program (program, runAll, runPure) where

import Control.Arrow ((>>>))
import Data.Map (Map)
import Prelude hiding (writeFile)

import Polysemy (Embed, Member, Sem, run, runM)
import Polysemy.Error.Extended (Error)
import Polysemy.Error.Extended qualified as Error
import Polysemy.Reader.Extended (Reader)
import Polysemy.Reader.Extended qualified as Reader
import Polysemy.Trace.Extended (Trace, trace)
import Polysemy.Trace.Extended qualified as Trace

import Effects.Curl (Curl, curl)
import Effects.Curl qualified as Curl
import Effects.WriteFile (WriteFile, writeFile)
import Effects.WriteFile qualified as WriteFile

import Forecast qualified

formatHtml :: Forecast.AreaForecast -> String
formatHtml forecast =
    "<!DOCTYPE html>\n"
        ++ "<html>\n"
        ++ "<head>\n"
        ++ "<meta charset=\"UTF-8\">\n"
        ++ "<title>Weather</title>\n"
        ++ "</head>\n"
        ++ "<body>\n"
        ++ "<h1>"
        ++ Forecast.areaName forecast
        ++ "</h1>\n"
        ++ "<h2>24 Hour Forecast</h2>\n"
        ++ "<dl>\n"
        ++ "<dt>Wind</dt>\n"
        ++ "<dd>"
        ++ Forecast.wind (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Sea State</dt>\n"
        ++ "<dd>"
        ++ Forecast.sea (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Weather</dt>\n"
        ++ "<dd>"
        ++ Forecast.weather (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Visibility</dt>\n"
        ++ "<dd>"
        ++ Forecast.visibility (Forecast.current24Hours forecast)
        ++ "</dd>\n"
        ++ "</dl>\n"
        ++ "<h2>Next 24 Hours</h2>\n"
        ++ "<dl>\n"
        ++ "<dt>Wind</dt>\n"
        ++ "<dd>"
        ++ Forecast.wind (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Sea State</dt>\n"
        ++ "<dd>"
        ++ Forecast.sea (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Weather</dt>\n"
        ++ "<dd>"
        ++ Forecast.weather (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "<dt>Visibility</dt>\n"
        ++ "<dd>"
        ++ Forecast.visibility (Forecast.next24Hours forecast)
        ++ "</dd>\n"
        ++ "</dl>\n"
        ++ "</body>\n"
        ++ "</html>\n"

program :: (Member Trace r, Member WriteFile r, Member Curl r, Member (Error String) r) => Sem r ()
program = do
    trace "Starting"
    page <- curl Forecast.inshoreWatersUrl
    let forecast = Forecast.parse page
    forecast' <- Error.note "couldn't parse forecast" forecast
    trace $ show forecast'
    writeFile "output/index.html" (formatHtml forecast')
    return ()

runAll :: Sem [Trace, WriteFile, Curl, Error String, Embed IO] a -> IO a
runAll = Trace.runToIO >>> WriteFile.runToIO >>> Curl.runToIOError >>> Error.runToIO >>> runM

runPure :: Map String String -> Sem [Trace, WriteFile, Curl, Reader (Map String String), Error String] a -> Either String ([String], [String], a)
runPure websites =
    Trace.runToList
        >>> WriteFile.runToList
        >>> Curl.runToReaderError
        >>> Reader.run websites
        >>> Error.runToEither
        >>> run
        >>> fmap (\(fileWrites, (traces, result)) -> (traces, fileWrites, result))
