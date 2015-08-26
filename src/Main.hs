{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Prelude                    hiding ( readFile, writeFile )

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Control.Exception          ( catch, SomeException )
import           Data.List                  ( intercalate )
import           Data.Maybe                 ( fromMaybe )
import qualified Data.Text                  as T
import           Data.Text.IO               ( readFile, writeFile )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment         ( getArgs, getProgName )
import           System.Exit                ( exitFailure )
import           System.FilePath.Posix      ( (</>) )

type TPutAPI = "files"
                   :> Capture "path" FilePath
                   :> Get '[PlainText] T.Text
          :<|> "files"
                   :> Capture "path" FilePath
                   :> ReqBody '[PlainText] T.Text
                   :> Post '[PlainText] ()

data ServerConf = ServerConf { basedir :: FilePath
                             }

defaultServerConf = ServerConf { basedir = "data"
                               }

tputAPI :: Proxy TPutAPI
tputAPI = Proxy

server :: ServerConf -> Server TPutAPI
server conf = down (basedir conf)
         :<|> up (basedir conf)

down :: FilePath -> FilePath -> EitherT ServantErr IO T.Text
down db path = do
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` fail
    case text of
        Nothing -> left err404
        Just t -> return t
    where fail :: SomeException -> IO (Maybe T.Text)
          fail = const (return Nothing)

up :: FilePath -> FilePath -> T.Text -> EitherT ServantErr IO ()
up db path content = liftIO $ writeFile (db </> path) content

app conf = serve tputAPI (server $ fromMaybe defaultServerConf conf)

main = do
    args <- getArgs
    progName <- getProgName

    if null args
        then do
            putStrLn $ intercalate " " [ "usage:", progName, "UPLOAD_DIR" ]
            exitFailure
        else do
            let basedir = head args
            putStrLn $ concat [ "Running on port 8080 from ", basedir, "..." ]
            run 8080 $ app (Just ServerConf { basedir = basedir })
