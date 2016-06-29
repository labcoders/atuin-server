{-|
 - Description: Atuin blockdata consuming
 -}

{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server.BlockData where

import Control.Monad.Trans.Either

import Database.Persist

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Servant

toChunkCoords :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
  toChunkCoords x y z = (x `div` 8, y `div` 8, z `div` 8)

-- | Execute a SQL query to insert 3
-- chunk coordinates in the Chunk table.
-- Should do nothing if chunks already exist.
writeChunk chkX chkY chkZ = insertSelect $

-- | Execute a SQL query to write a block into the
-- Block tables. Should do nothing
-- if the block already exists.
writeBlock blkX blkY blkZ =

-- | Execute a SQL query to write mod_name,
-- block_name, and block_data JSON object
-- of a given block to the BlockData table.
writeBlockData blkX blkY blkZ mn bn bd =

-- Need world database read.

-- Need object creation database write.

