{-|
 - Description: The TPut subsystem for Atuin
 -}

{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server.TPut where

import Prelude hiding ( readFile, writeFile )

import Control.Exception ( catch, SomeException )
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Data.List ( sort )
import qualified Data.Text as T
import Data.Text.IO ( readFile, writeFile )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>) )
import Servant

-- | Downloads a file.
down
  :: FilePath -- ^ The directory in which files live.
  -> FilePath -- ^ The path/name of the file the download.
  -> EitherT ServantErr IO T.Text
down db path = do
    liftIO $ putStrLn "download"
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` fail
    case text of
        Nothing -> left err404
        Just t -> return t
    where fail :: SomeException -> IO (Maybe T.Text)
          fail = const (return Nothing)

-- | Uploads a file.
up
  :: FilePath -- ^ The directory in which files live.
  -> FilePath -- ^ The path/name in the uploads directory where to record the file.
  -> T.Text -- ^ The file contents.
  -> EitherT ServantErr IO ()
up db path content = liftIO $ do
    putStrLn "upload"
    writeFile (db </> path) content

-- | Lists the files available for download.
ls
  :: FilePath -- ^ The directory in which files live.
  -> EitherT ServantErr IO T.Text
ls db = do
    liftIO $ putStrLn "list"
    -- ignore all files beginning with '.'
    files <- liftIO $ sort . filter (\(c:_) -> c /= '.') <$> getDirectoryContents db
    pure $ T.pack $ unlines files
