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

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Exception ( catch, SomeException )
import Data.List ( sort )
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Servant
import System.FilePath.Posix ( (</>) )
import System.Directory ( getDirectoryContents )

type TPutAPI
  = "files"
    :> Capture "path" FilePath
    :> Get '[PlainText] T.Text
  :<|> "files"
    :> Capture "path" FilePath
    :> ReqBody '[PlainText] T.Text
    :> Post '[PlainText] ()
  :<|> "list"
    :> Get '[PlainText] T.Text
  :<|> "msg"
    :> Capture "path" ComputerID
    :> Get '[PlainText] Message
  :<|> Capture "path" ComputerID
    :> ReqBody '[PlainText] Message
    :> Post '[PlainText] ()
  :<|> "blockdata"
    :> ReqBody '[PlainText] Message
    :> Post '[PlainText] ()

-- | The readonly configuration of the server.
data ServerConf
  = ServerConf
    { basedir :: FilePath
    , messages :: MVar (Map.Map ComputerID [Message])
    , worlddir :: FilePath
    }

-- | Create the default server configuration. Requires @IO@ so that we can
-- initialize the 'MVar' inside the server conf.
makeDefaultServerConf :: IO ServerConf
makeDefaultServerConf = do
  m <- newMVar Map.empty
  pure ServerConf
    { basedir = "data"
    , messages = m
    , worlddir = "world"
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
    :<|> blockdata (worlddir conf)

-- | Block data consumer.
--
-- /Unimplemented:/ will crash if used.
blockdata :: FilePath -> T.Text -> EitherT ServantErr IO ()
blockdata dir contents = error "unimplemented blockdata route" -- todo
