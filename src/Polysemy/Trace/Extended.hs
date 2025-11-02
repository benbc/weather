module Polysemy.Trace.Extended (Trace, ignore, trace, runToIO) where

import Polysemy (Embed, Member, Sem)
import Polysemy.Trace (Trace, ignoreTrace, trace, traceToStderr)

ignore :: Sem (Trace : r) a -> Sem r a
ignore = ignoreTrace

runToIO :: (Member (Embed IO) r) => Sem (Trace ': r) a -> Sem r a
runToIO = traceToStderr
