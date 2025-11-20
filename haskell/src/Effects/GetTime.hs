{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module Effects.GetTime (GetTime, getCurrentTime, runToIO, runToFixed) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as Time
import Polysemy (Embed, Member, Sem, embed, interpret, send)

data GetTime m a where
  GetCurrentTime :: GetTime m UTCTime

getCurrentTime :: (Member GetTime r) => Sem r UTCTime
getCurrentTime = send (GetCurrentTime :: GetTime (Sem r) UTCTime)

runToIO :: (Member (Embed IO) r) => Sem (GetTime ': r) a -> Sem r a
runToIO = interpret \GetCurrentTime -> embed Time.getCurrentTime

runToFixed :: UTCTime -> Sem (GetTime ': r) a -> Sem r a
runToFixed fixedTime = interpret \GetCurrentTime -> return fixedTime
