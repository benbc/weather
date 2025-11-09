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

import Effects.Curl (Curl, curl)
import Effects.Curl qualified as Curl
import Effects.WriteFile (WriteFile, writeFile)
import Effects.WriteFile qualified as WriteFile

import Display qualified
import Forecast qualified

program :: (Member WriteFile r, Member Curl r, Member (Error String) r) => Sem r ()
program = do
    page <- curl Forecast.inshoreWatersUrl
    forecast <- Error.note "couldn't parse forecast" $ Forecast.parse page
    output <- Error.fromEither $ Display.formatHtml forecast
    writeFile "../out/new/index.html" output

runAll :: Sem [WriteFile, Curl, Error String, Embed IO] () -> IO ()
runAll = WriteFile.runToIO >>> Curl.runToIOError >>> Error.runToIO >>> runM

runPure :: Map String String -> Sem [WriteFile, Curl, Reader (Map String String), Error String] () -> Either String [(FilePath, String)]
runPure websites =
    WriteFile.runToList
        >>> Curl.runToReaderError
        >>> Reader.run websites
        >>> Error.runToEither
        >>> run
        >>> fmap fst
