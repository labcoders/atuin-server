{-|
 - Description: Atuin internal types
 -}

module Atuin.Types where

import qualified Data.Text as T

-- | Messages are opaque strings. We do not require messages to adhere to any
-- particular format.
type Message
  = T.Text

-- | Although these will usually be computer IDs, which are numbers, we will
-- treat them as strings, since it is not inconceivable that we would like to
-- use human readable names such as @"lava-pump"@ as a destination.
type ComputerID
  = T.Text
