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

import Display qualified
import Forecast qualified

program :: (Member Trace r, Member WriteFile r, Member Curl r, Member (Error String) r) => Sem r ()
program = do
    trace "Starting"
    page <- curl Forecast.inshoreWatersUrl
    trace "Got forecast"
    forecast <- Error.note "couldn't parse forecast" $ Forecast.parse page
    trace "Parsed forecast"
    output <- Error.fromEither $ Display.formatHtml forecast
    writeFile "output/index.html" output
    trace "Finished"

runAll :: Sem [Trace, WriteFile, Curl, Error String, Embed IO] () -> IO ()
runAll = Trace.runToIO >>> WriteFile.runToIO >>> Curl.runToIOError >>> Error.runToIO >>> runM

runPure :: Map String String -> Sem [Trace, WriteFile, Curl, Reader (Map String String), Error String] () -> Either String [(FilePath, String)]
runPure websites =
    Trace.ignore
        >>> WriteFile.runToList
        >>> Curl.runToReaderError
        >>> Reader.run websites
        >>> Error.runToEither
        >>> run
        >>> fmap fst
