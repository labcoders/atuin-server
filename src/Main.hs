{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server

import Data.List                ( intercalate )
import Data.Maybe               ( fromMaybe )
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment       ( getArgs, getProgName )
import System.Exit              ( exitFailure )

app conf = serve tputAPI (server $ fromMaybe defaultServerConf conf)

main = do
    args <- getArgs
    progName <- getProgName

    if null args
        then do
            putStrLn $ intercalate " " [ "usage:", progName, "UPLOAD_DIR" ]
            exitFailure
        else do
            let basedir = head args
            putStrLn $ concat [ "Running on port 8080 from ", basedir, "..." ]
            let settings = setPort 8080 $ setHost "127.0.0.1" defaultSettings
            runSettings settings $ app (Just ServerConf { basedir = basedir })
