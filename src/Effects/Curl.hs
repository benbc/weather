{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Effects.Curl (Curl, curl, runToReaderError, runToIOError) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Client (Response, httpLbs, newManager, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status200)

import Polysemy (Embed, Member, Sem, embed, interpret, send)
import Polysemy.Error.Extended (Error)
import qualified Polysemy.Error.Extended as Error
import Polysemy.Reader.Extended (Reader)
import qualified Polysemy.Reader.Extended as Reader

data Curl m a where
    Curl :: String -> Curl m String

curl :: (Member Curl r) => String -> Sem r String
curl url = send (Curl url :: Curl (Sem r) String)

runToIOError :: (Member (Embed IO) r, Member (Error String) r) => Sem (Curl ': r) a -> Sem r a
runToIOError = interpret \case
    Curl url -> do
        resp <- embed $ fetch url
        -- TOOD: can we replace this case with a guarded failure?
        case responseStatus resp of
            s | s == status200 -> return $ Char8.unpack $ responseBody resp
            s -> Error.throw $ show s

runToReaderError :: (Member (Reader (Map String String)) r, Member (Error String) r) => Sem (Curl ': r) a -> Sem r a
runToReaderError = interpret \case
    Curl url -> do
        websites <- Reader.ask
        Error.note "no such page" $ Map.lookup url websites

fetch :: String -> IO (Response ByteString)
fetch url = do
    req <- parseRequest url
    let req' = req{requestHeaders = [("User-Agent", "fishsticks")]}
    man <- newManager tlsManagerSettings
    httpLbs req' man
