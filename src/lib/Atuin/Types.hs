{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
 - Description: Atuin internal types
 -}

module Atuin.Types where

import Data.Aeson
import Data.Int (Int32)
import qualified Data.Text as T

import GHC.Generics

-- | Messages are opaque strings. We do not require messages to adhere to any
-- particular format.
type Message
  = T.Text

-- | Although these will usually be computer IDs, which are numbers, we will
-- treat them as strings, since it is not inconceivable that we would like to
-- use human readable names such as @"lava-pump"@ as a destination.
type ComputerID
  = T.Text

data Block
  = Block
  { blockX :: Int32
  , blockY :: Int32
  , blockZ :: Int32
  , chunkType :: T.Text
  , modName :: T.Text
  , blockName :: T.Text
  , blockMetaData :: T.Text -- this is the whole table which contains the modname and blockname, but it's very dependent on the block and convenient to get it from JSON so we keep it here
  }
  deriving Generic

data BlockData
  = BlockData
  { block1 :: Block
  , block2 :: Block
  , block3 :: Block
  }
  deriving Generic

instance FromJSON Block
instance FromJSON BlockData
