{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Atuin.Server
( ServerConf(..)
, makeDefaultServerConf
, server
, tputAPI
) where

import Atuin.Planner.Types ( JobReq )
import qualified Atuin.Server.DB as DB
import Atuin.Server.Messaging ( send, recv )
import Atuin.Server.TPut ( down, up, ls )
import Atuin.Types

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map
import GHC.Generics
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
    :> Capture "path" SomeDeviceID
    :> Get '[PlainText] Message
  :<|> Capture "path" SomeDeviceID -- Records a message for a given ID.
    :> ReqBody '[PlainText] Message
    :> Post '[PlainText] NoContent
  :<|> "blockdata" -- Accepts JSON blobs for world state.
    :> ReqBody '[JSON] BlockData
    :> Post '[JSON] NoContent
  :<|> "test"
    :> ReqBody '[JSON] TestData
    :> Post '[JSON] TestData

-- | The readonly configuration of the server.
data ServerConf
  = ServerConf
    { basedir :: FilePath
    , messages :: MVar (Map.Map SomeDeviceID [Message])
    , plannerPipe :: Chan JobReq
    }

-- | Create the default server configuration. Requires @IO@ so that we can
-- initialize the 'MVar' inside the server conf.
makeDefaultServerConf :: IO ServerConf
makeDefaultServerConf = do
  m <- newMVar Map.empty
  chan <- newChan
  pure ServerConf
    { basedir = "data"
    , messages = m
    , plannerPipe = chan
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
    :<|> DB.blockdata
    :<|> stdinBlockingEndpoint (plannerPipe conf)

data TestData
  = TestData
    { sampleString :: T.Text
    }
  deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

stdinBlockingEndpoint :: Chan JobReq -> TestData -> Handler TestData
stdinBlockingEndpoint _ (TestData s) = liftIO $ do
  putStrLn "blocking test"
  TestData <$> (T.append s <$> T.getLine)
