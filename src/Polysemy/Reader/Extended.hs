module Polysemy.Reader.Extended (Reader, run, ask) where

import Polysemy (Sem)
import Polysemy.Reader (Reader, ask, runReader)

run :: i -> Sem (Reader i : r) a -> Sem r a
run = runReader
