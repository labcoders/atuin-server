{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude hiding ( readFile, writeFile )
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Data.Text as T
import Data.Text.IO ( readFile, writeFile )
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Exception ( catch, SomeException )
import System.FilePath.Posix ( (</>) )

type TextFile = T.Text

type TPutAPI = "files"
                   :> Capture "path" FilePath
                   :> Get '[PlainText] TextFile
          :<|> "files"
                   :> Capture "path" FilePath
                   :> ReqBody '[PlainText] T.Text
                   :> Post '[PlainText] ()

type BaseDirPath = FilePath

data ServerConf = ServerConf { basedir :: BaseDirPath
                             }

defaultServerConf = ServerConf { basedir = "data"
                               }

tputAPI :: Proxy TPutAPI
tputAPI = Proxy

server :: ServerConf -> Server TPutAPI
server conf = down (basedir conf)
         :<|> up (basedir conf)

down :: BaseDirPath -> FilePath -> EitherT ServantErr IO T.Text
down db path = do
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` fail
    case text of
        Nothing -> left err404
        Just t -> return t
    where fail :: SomeException -> IO (Maybe T.Text)
          fail = const (return Nothing)

up :: BaseDirPath -> FilePath -> T.Text -> EitherT ServantErr IO ()
up db path content = liftIO $ writeFile (db </> path) content

app = serve tputAPI (server defaultServerConf)

main = run 8080 app
