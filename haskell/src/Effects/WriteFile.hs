{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}

module Effects.WriteFile (WriteFile, writeFile, runToIO, runToList) where

import Polysemy (Embed, Member, Sem, embed, interpret, reinterpret, send)
import qualified Polysemy.Writer as Writer
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude hiding (writeFile)
import qualified Prelude

data WriteFile m a where
    WriteFile :: FilePath -> String -> WriteFile m ()

writeFile :: (Member WriteFile r) => FilePath -> String -> Sem r ()
writeFile path content = Polysemy.send (WriteFile path content :: WriteFile (Sem r) ())

runToIO :: (Member (Embed IO) r) => Sem (WriteFile ': r) a -> Sem r a
runToIO = interpret \(WriteFile path content) -> embed do
    createDirectoryIfMissing True (takeDirectory path)
    Prelude.writeFile path content

runToList :: Sem (WriteFile ': r) a -> Sem r ([(FilePath, String)], a)
runToList =
    Writer.runWriter . reinterpret \(WriteFile path content) ->
        Writer.tell [(path, content)]
