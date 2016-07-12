{-|
 - Description: Atuin internal types
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Atuin.Types where

import Data.Aeson
import Data.Int (Int32)
import qualified Data.Text as T
import Web.HttpApiData ( FromHttpApiData(..), ToHttpApiData )

import GHC.Generics

-- | Messages are opaque strings. We do not require messages to adhere to any
-- particular format.
type Message
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

data Device
  = Computer
  | Turtle

-- | A device ID with a type-level tag specifying which device it's for.
newtype DeviceID (a :: Device)
  = DeviceID T.Text
  deriving (Eq, Ord, Generic, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data SomeDeviceID
  = forall a. SomeDeviceID (DeviceID a)

instance FromHttpApiData SomeDeviceID where
  parseUrlPiece = pure . SomeDeviceID . DeviceID

someDeviceID :: SomeDeviceID -> T.Text
someDeviceID (SomeDeviceID (DeviceID did)) = did

computerID :: T.Text -> DeviceID 'Computer
computerID = DeviceID

turtleID :: T.Text -> DeviceID 'Turtle
turtleID = DeviceID

instance Eq SomeDeviceID where
  SomeDeviceID (DeviceID x) == SomeDeviceID (DeviceID y) = x == y

instance Ord SomeDeviceID where
  SomeDeviceID (DeviceID x) `compare` SomeDeviceID (DeviceID y) = compare x y
