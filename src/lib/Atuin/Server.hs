{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server
( ServerConf(..)
, makeDefaultServerConf
, server
, tputAPI
) where

import Atuin.Server.Messaging ( send, recv )
import Atuin.Server.TPut ( down, up, ls )
import Atuin.Types

import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Servant

type TPutAPI
  = "files" -- Download a program (Lua).
    :> Capture "path" FilePath
    :> Get '[PlainText] T.Text
  :<|> "files" -- Upload a program (Lua).
    :> Capture "path" FilePath
    :> ReqBody '[PlainText] T.Text
    :> Post '[PlainText] NoContent
  :<|> "list" -- Lists all available programs.
    :> Get '[PlainText] T.Text
  :<|> "msg" -- Used to retrieve messages for a given ID.
    :> Capture "path" ComputerID
    :> Get '[PlainText] Message
  :<|> Capture "path" ComputerID -- Records a message for a given ID.
    :> ReqBody '[PlainText] Message
    :> Post '[PlainText] NoContent
  :<|> "blockdata" -- Accepts JSON blobs for world state.
    :> ReqBody '[PlainText] Message
    :> Post '[PlainText] NoContent
  :<|> "world" -- Gets the world in JSON format.
    :> Get '[PlainText] T.Text

-- | The readonly configuration of the server.
data ServerConf
  = ServerConf
    { basedir :: FilePath
    , messages :: MVar (Map.Map ComputerID [Message])
    , worlddb :: FilePath
    }

-- | Create the default server configuration. Requires @IO@ so that we can
-- initialize the 'MVar' inside the server conf.
makeDefaultServerConf :: IO ServerConf
makeDefaultServerConf = do
  m <- newMVar Map.empty
  pure ServerConf
    { basedir = "data"
    , messages = m
    , worlddb = "atuin"
    }

-- | A convenient way to write @Proxy TPutAPI@.
tputAPI :: Proxy TPutAPI
tputAPI = Proxy

-- | The server for the 'TPutAPI'.
server :: ServerConf -> Server TPutAPI
server conf
  = down (basedir conf)
    :<|> up (basedir conf)
    :<|> ls (basedir conf)
    :<|> recv (messages conf)
    :<|> send (messages conf)
    :<|> blockdata (worlddb conf)
    :<|> world (worlddb conf)


-- | Block data consumer.
--
-- /Unimplemented:/ will crash if used.
blockdata :: FilePath -> T.Text -> Handler NoContent
blockdata = error "unimplemented blockdata route" -- todo

world :: FilePath -> Handler T.Text
world = error "unimplemented world route" -- todo
