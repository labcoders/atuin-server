{-|
 - Description: The TPut subsystem for Atuin
 -}

{-# LANGUAGE OverloadedStrings #-}

module Atuin.Server.TPut where

import Prelude hiding ( readFile, writeFile )

import Control.Exception ( catch, SomeException )
import Control.Monad.Except
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
  -> Handler T.Text
down db path = do
    text <- liftIO $ (readFile (db </> path) >>= return . Just) `catch` handle
    case text of
        Nothing -> throwError err404
        Just t -> return t
    where handle :: SomeException -> IO (Maybe T.Text)
          handle = const (return Nothing)

-- | Uploads a file.
up
  :: FilePath -- ^ The directory in which files live.
  -> FilePath -- ^ The path/name in the uploads directory where to record the file.
  -> T.Text -- ^ The file contents.
  -> Handler NoContent
up db path content = liftIO $ do
    writeFile (db </> path) content
    pure NoContent

-- | Lists the files available for download.
ls
  :: FilePath -- ^ The directory in which files live.
  -> Handler T.Text
ls db = do
    -- ignore all files beginning with '.'
    files <- liftIO $ sort . filter (\(c:_) -> c /= '.') <$> getDirectoryContents db
    pure $ T.pack $ unlines files
