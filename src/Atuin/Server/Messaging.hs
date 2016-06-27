{-|
 - Description: Atuin messaging subsystem
 -}

{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server.Messaging where

import Prelude hiding ( readFile, writeFile )

import Atuin.Types

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.IO ( readFile, writeFile )
import Servant

-- | Poll for a message on the queue identified with a given 'ComputerID'.
recv
  -- | Shared in-memory database of messages
  :: MVar (Map.Map ComputerID [Message])
  -- | Name of the queue to read from
  -> ComputerID
  -> EitherT ServantErr IO Message
recv mv id = do
  msgs <- liftIO $ takeMVar mv
  case Map.lookup id msgs of
    Just a -> case a of
      [] -> liftIO $ putMVar mv msgs >> return ""
      (x:xs) -> do
          let newMsgs = Map.adjust tail id msgs
          liftIO $ putMVar mv newMsgs
          liftIO $ putStrLn $ T.unpack id ++ " -> # = " ++ T.unpack x
          return x
    Nothing -> liftIO $ do
      putStrLn $ "no messages for " ++ show id
      putMVar mv msgs >> return ""

-- | Record a message destinted for a given 'ComputerID'.
send
  -- | Shared in-memory database of messages
  :: MVar (Map.Map ComputerID [Message])
  -- | Name of the queue to write to
  -> ComputerID
  -- | Message to record
  -> Message
  -> EitherT ServantErr IO ()
send mv id msg = do
    liftIO $ putStrLn $ "# -> " ++ T.unpack id ++ " = " ++ T.unpack msg
    msgs <- liftIO $ takeMVar mv
    liftIO $ putMVar mv $ case Map.lookup id msgs of
        Nothing -> Map.insert id [msg] msgs
        Just oldMsgs -> Map.insert id (oldMsgs ++ [msg]) msgs
