module Polysemy.Error.Extended (Error, runError, errorToIOError, note, throw) where

import Polysemy (Sem)
import Polysemy.Error (Error, note, runError, throw)

errorToIOError :: Sem (Error String ': r) a -> Sem r a
errorToIOError sem = do
    result <- runError sem
    case result of
        Left err -> error err
        Right val -> return val
