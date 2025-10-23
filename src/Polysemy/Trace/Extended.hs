module Polysemy.Trace.Extended (Trace, runToList, trace, runToIO) where

import Polysemy (Embed, Member, Sem)
import Polysemy.Trace (Trace, runTraceList, trace, traceToStderr)

runToList :: Sem (Trace : r) a -> Sem r ([String], a)
runToList = runTraceList

runToIO :: (Member (Embed IO) r) => Sem (Trace ': r) a -> Sem r a
runToIO = traceToStderr
