{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server
( ServerConf(..)
, makeDefaultServerConf
, server
, tputAPI
)
where

import           Prelude                    hiding ( readFile, writeFile )
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Control.Exception          ( catch, SomeException )
import           Data.List                  ( sort )
import qualified Data.Text                  as T
import           Data.Text.IO               ( readFile, writeFile )
import qualified Data.Map.Strict            as Map
import           Servant
import           System.FilePath.Posix      ( (</>) )
import           System.Directory           ( getDirectoryContents )

type TPutAPI = "files"
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
          :<|> "msg"
                   :> Capture "path" ComputerID
                   :> ReqBody '[PlainText] Message
                   :> Post '[PlainText] ()
          :<|> "blockdata"
                   :> ReqBody '[PlainText] Message
                   :> Post '[PlainText] ()

type Message = T.Text
type ComputerID = T.Text

data ServerConf = ServerConf { basedir :: FilePath,
                               messages :: MVar (Map.Map ComputerID [Message]),
                               worlddir :: FilePath
                             }

makeDefaultServerConf :: IO ServerConf
makeDefaultServerConf = do
    m <- newMVar Map.empty
    pure ServerConf
        { basedir = "data"
        , messages = m
        , worlddir = "world"
        }

tputAPI :: Proxy TPutAPI
tputAPI = Proxy

server :: ServerConf -> Server TPutAPI
server conf = down (basedir conf)
         :<|> up (basedir conf)
         :<|> ls (basedir conf)
         :<|> recv (messages conf)
         :<|> send (messages conf)
         :<|> blockdata (worlddir conf)

down :: FilePath -> FilePath -> EitherT ServantErr IO T.Text
down db path = do
    liftIO $ putStrLn "download"
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` fail
    case text of
        Nothing -> left err404
        Just t -> return t
    where fail :: SomeException -> IO (Maybe T.Text)
          fail = const (return Nothing)

up :: FilePath -> FilePath -> T.Text -> EitherT ServantErr IO ()
up db path content = liftIO $ do
    putStrLn "upload"
    writeFile (db </> path) content

ls :: FilePath -> EitherT ServantErr IO T.Text
ls db = do
    liftIO $ putStrLn "list"
    files <- liftIO $ sort . filter (\(c:_) -> c /= '.') <$> getDirectoryContents db
    pure $ T.pack $ unlines files

recv :: MVar (Map.Map ComputerID [Message]) -> ComputerID -> EitherT ServantErr IO Message
recv mv id = do
    msgs <- liftIO $ takeMVar mv
    case Map.lookup id msgs of
        Just a -> case a of
                    [] -> liftIO $ putMVar mv msgs >> return ""
                    (x:xs) -> do
                        let newMsgs = Map.adjust tail id msgs
                        liftIO $ putMVar mv newMsgs
                        liftIO $ putStrLn $ T.unpack id ++ " -> # = " ++ T.unpack x
                        return x
        Nothing -> liftIO $ do
            putStrLn $ "no messages for " ++ show id
            putMVar mv msgs >> return ""

send :: MVar (Map.Map ComputerID [Message]) -> ComputerID -> Message -> EitherT ServantErr IO ()
send mv id msg = do
    liftIO $ putStrLn $ "# -> " ++ T.unpack id ++ " = " ++ T.unpack msg
    msgs <- liftIO $ takeMVar mv
    liftIO $ putMVar mv $ case Map.lookup id msgs of
        Nothing -> Map.insert id [msg] msgs
        Just oldMsgs -> Map.insert id (oldMsgs ++ [msg]) msgs

blockdata :: FilePath -> T.Text -> EitherT ServantErr IO ()
blockdata dir contents = error "unimplemented blockdata route" -- todo
