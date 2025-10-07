{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Client (httpLbs, newManager, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Network.HTTP.Types.Status (status200)
import Polysemy (Embed, Member, Sem, embed, interpret, run, runM, send)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.Trace (Trace, runTraceList, trace, traceToStderr)

data Curl m a where
    Curl :: String -> Curl m (Maybe ByteString)

curl :: (Member Curl r) => String -> Sem r (Maybe ByteString)
curl url = send (Curl url :: Curl (Sem r) (Maybe ByteString))

curlToIO :: (Member (Embed IO) r) => Sem (Curl ': r) a -> Sem r a
curlToIO = interpret \case
    Curl url -> embed $ do
        req <- parseRequest url
        let req' = req{requestHeaders = [("User-Agent", "fishsticks")]}
        man <- newManager tlsManagerSettings
        resp <- httpLbs req' man
        return case responseStatus resp of
            s | s == status200 -> Just (responseBody resp)
            _ -> Nothing

curlFromMap :: (Member (Reader (Map String ByteString)) r) => Sem (Curl ': r) a -> Sem r a
curlFromMap = interpret \case
    Curl url -> fmap (Map.lookup url) ask

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

program :: (Member Trace r, Member Curl r) => Sem r ()
program = do
    trace "Starting"
    page <- curl inshoreWatersUrl
    trace $ take 150 $ show page
    return ()

runAll :: Sem [Trace, Curl, Embed IO] a -> IO a
runAll = runM . curlToIO . traceToStderr

runPure :: Map String ByteString -> Sem [Trace, Curl, Reader (Map String ByteString)] a -> ([String], a)
runPure websites = run . runReader websites . curlFromMap . runTraceList

main :: IO ()
main = do
    runAll program
    let (traces, ()) = runPure websites program
    putStrLn $ show traces
  where
    websites = Map.fromList [(inshoreWatersUrl, "bungo")]
