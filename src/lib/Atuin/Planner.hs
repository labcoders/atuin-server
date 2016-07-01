{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Atuin.Planner
( planner
, module Atuin.Planner.Types
) where

import Atuin.Planner.Types

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad ( forever )
import qualified Data.Text.IO as T

planner :: Chan JobReq -> IO a
planner chan = forever $ do
  JobReq {..} <- readChan chan
  code <- T.getLine
  putMVar jobReqResponse code
