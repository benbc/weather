{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP.Client (Response, httpLbs, newManager, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (status200)

import Polysemy (Embed, Member, Sem, embed, interpret, run, runM, send)
import Polysemy.Error (Error, note, runError, throw)
import Polysemy.Reader (Reader, ask, runReader)
import Polysemy.Trace (Trace, runTraceList, trace, traceToStderr)

data Curl m a where
    Curl :: String -> Curl m ByteString

curl :: (Member Curl r) => String -> Sem r ByteString
curl url = send (Curl url :: Curl (Sem r) ByteString)

fetch :: String -> IO (Response ByteString)
fetch url = do
    req <- parseRequest url
    let req' = req{requestHeaders = [("User-Agent", "fishsticks")]}
    man <- newManager tlsManagerSettings
    httpLbs req' man

curlToIO :: (Member (Embed IO) r, Member (Error String) r) => Sem (Curl ': r) a -> Sem r a
curlToIO = interpret \case
    Curl url -> do
        resp <- embed $ fetch url
        case responseStatus resp of
            s | s == status200 -> return $ responseBody resp
            s -> throw $ show s

curlFromMap :: (Member (Reader (Map String ByteString)) r, Member (Error String) r) => Sem (Curl ': r) a -> Sem r a
curlFromMap = interpret \case
    Curl url -> do
        websites <- ask
        note "no such page" $ Map.lookup url websites

errorToIOError :: Sem (Error String ': r) a -> Sem r a
errorToIOError sem = do
    result <- runError sem
    case result of
        Left err -> error err
        Right val -> return val

inshoreWatersUrl :: String
inshoreWatersUrl = "https://weather.metoffice.gov.uk/specialist-forecasts/coast-and-sea/inshore-waters-forecast"

program :: (Member Trace r, Member Curl r) => Sem r ()
program = do
    trace "Starting"
    page <- curl inshoreWatersUrl
    trace $ take 150 $ show page
    return ()

runAll :: Sem [Trace, Curl, Error String, Embed IO] a -> IO a
runAll = runM . errorToIOError . curlToIO . traceToStderr

runPure :: Map String ByteString -> Sem [Trace, Curl, Reader (Map String ByteString), Error String] a -> Either String ([String], a)
runPure websites = run . runError . runReader websites . curlFromMap . runTraceList

main :: IO ()
main = do
    runAll program
    case runPure websites program of
        Left err -> putStrLn $ "Error: " ++ err
        Right (traces, ()) -> putStrLn $ show traces
  where
    websites = Map.fromList [(inshoreWatersUrl, "bungo")]
