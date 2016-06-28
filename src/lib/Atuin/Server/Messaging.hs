{-|
 - Description: Atuin messaging subsystem
 -}

{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server.Messaging where

import Prelude hiding ( readFile, writeFile )

import Atuin.Types

import Control.Monad.Except
import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Servant

-- | Poll for a message on the queue identified with a given 'ComputerID'.
recv
  -- | Shared in-memory database of messages
  :: MVar (Map.Map ComputerID [Message])
  -- | Name of the queue to read from
  -> ComputerID
  -> Handler Message
recv mv cid = do
  msgs <- liftIO $ takeMVar mv
  case Map.lookup cid msgs of
    Just a -> case a of
      [] -> liftIO $ putMVar mv msgs >> return ""
      (x:_) -> do
          let newMsgs = Map.adjust tail cid msgs
          liftIO $ putMVar mv newMsgs
          liftIO $ putStrLn $ T.unpack cid ++ " -> # = " ++ T.unpack x
          return x
    Nothing -> liftIO $ do
      putStrLn $ "no messages for " ++ show cid
      putMVar mv msgs >> return ""

-- | Record a message destinted for a given 'ComputerID'.
send
  -- | Shared in-memory database of messages
  :: MVar (Map.Map ComputerID [Message])
  -- | Name of the queue to write to
  -> ComputerID
  -- | Message to record
  -> Message
  -> Handler NoContent
send mv cid msg = do
    liftIO $ putStrLn $ "# -> " ++ T.unpack cid ++ " = " ++ T.unpack msg
    msgs <- liftIO $ takeMVar mv
    liftIO $ putMVar mv $ case Map.lookup cid msgs of
        Nothing -> Map.insert cid [msg] msgs
        Just oldMsgs -> Map.insert cid (oldMsgs ++ [msg]) msgs
    pure NoContent
