{-# LANGUAGE OverloadedStrings #-}

module Main where

import Atuin.Server

import Data.List ( intercalate )
import Network.Wai ( Application )
import Network.Wai.Handler.Warp
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
            let settings = setPort 8080 $ setHost "127.0.0.1" defaultSettings
            conf <- makeDefaultServerConf
            runSettings settings $ app conf { basedir = p }