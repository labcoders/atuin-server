{-|
 - Description: Atuin database functions
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Atuin.Server.DB where

import qualified Atuin.Types as A

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)

import Data.Int (Int32)

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Data.Pool

import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Chunk
    chunkX Int32
    chunkY Int32
    chunkZ Int32
    chunkType T.Text
    UniqueChunkTriple chunkX chunkY chunkZ
    deriving Show

Object
    chunkId ChunkId
    objectName T.Text
    deriving Show

Block
    blockX Int32
    blockY Int32
    blockZ Int32
    chunkId ChunkId
    objectId ObjectId Maybe
    UniqueBlockTriple blockX blockY blockZ
    deriving Show

BlockData
    blockId BlockId
    modName T.Text
    blockName T.Text
    table T.Text -- can change to JSON
    UniqueBlockId blockId
    deriving Show
|]

connStr :: ConnectionString
connStr = "hostname=localhost port=5432 user=atuin"

toChunkCoords :: Int32 -> Int32 -> Int32 -> (Int32, Int32, Int32)
toChunkCoords x y z = (x `div` 8, y `div` 8, z `div` 8)

blkDataToList :: A.BlockData -> [A.Block]
blkDataToList bd = (A.block1 bd : A.block2 bd : A.block3 bd : [])

-- | Construct and migrate all tables.
buildDb :: IO ()
buildDb = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ runMigration migrateAll

-- | Inserts all chunk, block, and blockdata entries without an
-- associated object into DB using a connection pool.
insertRecvBlockData blk =
  let x = A.blockX blk
      y = A.blockY blk
      z = A.blockZ blk
      t = A.chunkType blk
      mn = A.modName blk
      bn = A.blockName blk
      bd = A.blockMetaData blk
  in withPostgresqlPool connStr 10 $ \pool ->
    liftIO $ do
      flip runSqlPersistMPool (pool :: Pool SqlBackend) $ do
        chunkRec <- insertChunk (x `div` 8) (y `div` 8) (z `div` 8) t
        blockRec <- insertBlock x y z (entityKey chunkRec) Nothing
        blockDataRec <- insertBlockData (entityKey blockRec) mn bn bd
        return blockDataRec

-- | Build a SQL query to insert 3
-- chunk coordinates in the Chunk table.
-- Should do nothing if chunks already exist.
-- Returns the entire Chunk record after the operation.
insertChunk x y z t = upsert (Chunk x y z t) []

-- | Build a SQL query to insert a block into the
-- Block tables. Should do nothing if the block already exists.
-- Returns the entire Block record after the operation.
insertBlock x y z chkId mObjId = upsert (Block x y z chkId mObjId) []

-- | Build a SQL query to insert mod_name,
-- block_name, and block_data JSON object
-- of a given block to the BlockData table.
-- If the blockdata ID already exists, then reassign the
-- modname, blockname, and blockdata table values.
-- Returns the BlockData record after the operation.
insertBlockData blkId mn bn bd = upsert (BlockData blkId mn bn bd)
                                    [ BlockDataModName =. mn
                                    , BlockDataBlockName =. bn
                                    , BlockDataTable =. bd
                                    ]

-- Need world database read.

-- Need object creation database write.
--
-- Need block data insert *with* an associated object
