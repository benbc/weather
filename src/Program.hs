{-# LANGUAGE DataKinds #-}

module Program (program, runAll, runPure) where

import Control.Arrow ((>>>))
import Data.Map (Map)

import Polysemy (Embed, Member, Sem, run, runM)
import Polysemy.Error.Extended (Error)
import qualified Polysemy.Error.Extended as Error
import Polysemy.Reader.Extended (Reader)
import qualified Polysemy.Reader.Extended as Reader
import Polysemy.Trace.Extended (Trace, trace)
import qualified Polysemy.Trace.Extended as Trace

import Effects.Curl (Curl, curl)
import qualified Effects.Curl as Curl

import qualified Forecast

program :: (Member Trace r, Member Curl r, Member (Error String) r) => Sem r ()
program = do
    trace "Starting"
    page <- curl Forecast.inshoreWatersUrl
    let forecast = Forecast.parse page
    forecast' <- Error.note "couldn't parse forecast" forecast
    trace forecast'
    return ()

runAll :: Sem [Trace, Curl, Error String, Embed IO] a -> IO a
runAll = Trace.runToIO >>> Curl.runToIOError >>> Error.runToIO >>> runM

runPure :: Map String String -> Sem [Trace, Curl, Reader (Map String String), Error String] a -> Either String ([String], a)
runPure websites = Trace.runToList >>> Curl.runToReaderError >>> Reader.run websites >>> Error.runToEither >>> run
