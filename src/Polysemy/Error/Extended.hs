module Polysemy.Error.Extended (Error, fromEither, runToEither, runToIO, note, throw) where

import Polysemy (Sem)
import Polysemy.Error (Error, fromEither, note, runError, throw)

runToIO :: Sem (Error String ': r) a -> Sem r a
runToIO sem = do
    result <- runError sem
    case result of
        Left err -> error err
        Right val -> return val

runToEither :: Sem (Error e : r) a -> Sem r (Either e a)
runToEither = runError
