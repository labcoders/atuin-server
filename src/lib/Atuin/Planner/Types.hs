{-# LANGUAGE DataKinds #-}

module Atuin.Planner.Types where

import Atuin.Types
import Atuin.Lua

import Control.Concurrent.Chan
import Control.Concurrent.MVar

-- | A request for a job. This is sent when a turtle becomes idle.
data JobReq
  = JobReq
    { jobReqTurtleId :: DeviceID 'Turtle
    , jobReqResponse :: MVar LuaText
    }
