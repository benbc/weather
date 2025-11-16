{-# LANGUAGE DataKinds #-}

module Program (program, runAll, runPure) where

import Control.Arrow ((>>>))
import Data.Map (Map)
import Data.Time.Clock (UTCTime)
import Prelude hiding (writeFile)

import Polysemy (Embed, Member, Sem, run, runM)
import Polysemy.Error.Extended (Error)
import Polysemy.Error.Extended qualified as Error
import Polysemy.Reader.Extended (Reader)
import Polysemy.Reader.Extended qualified as Reader

import Effects.Curl (Curl, curl)
import Effects.Curl qualified as Curl
import Effects.GetTime (GetTime, getCurrentTime)
import Effects.GetTime qualified as GetTime
import Effects.WriteFile (WriteFile, writeFile)
import Effects.WriteFile qualified as WriteFile

import Display qualified
import Forecast qualified

program :: (Member WriteFile r, Member Curl r, Member GetTime r, Member (Error String) r) => Sem r ()
program = do
    page <- curl Forecast.inshoreWatersUrl
    forecast <- Error.note "couldn't parse forecast" $ Forecast.parse page
    currentTime <- getCurrentTime
    output <- Error.fromEither $ Display.formatHtml currentTime forecast
    writeFile "../out/new/index.html" output

runAll :: Sem [WriteFile, Curl, GetTime, Error String, Embed IO] () -> IO ()
runAll = WriteFile.runToIO >>> Curl.runToIOError >>> GetTime.runToIO >>> Error.runToIO >>> runM

runPure :: UTCTime -> Map String String -> Sem [WriteFile, Curl, GetTime, Reader (Map String String), Error String] () -> Either String [(FilePath, String)]
runPure fixedTime websites =
    WriteFile.runToList
        >>> Curl.runToReaderError
        >>> GetTime.runToFixed fixedTime
        >>> Reader.run websites
        >>> Error.runToEither
        >>> run
        >>> fmap fst
