{-# LANGUAGE OverloadedStrings #-}

module Main where

import Atuin.Planner
import Atuin.Server

import Control.Concurrent ( forkIO )
import Control.Monad ( void )
import Data.List ( intercalate )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )

app :: ServerConf -> Application
app = serve tputAPI . server

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    if null args
        then do
            putStrLn $ intercalate " " [ "usage:", progName, "UPLOAD_DIR" ]
            exitFailure
        else do
            let p = head args
            putStrLn $ concat [ "Running on port 8080 from ", p, "..." ]
            withStdoutLogger $ \aplogger -> do
              let settings = setPort 8080 $ setHost "127.0.0.1" $ setLogger aplogger defaultSettings
              conf <- makeDefaultServerConf
              go settings (conf { basedir = p})

go :: Settings -> ServerConf -> IO ()
go settings conf = do
  void $ forkIO $ planner (plannerPipe conf)
  runSettings settings $ app conf
