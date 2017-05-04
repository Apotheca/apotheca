{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Blocks where

import           Control.Monad          (void, when)
import           Control.Monad.Identity

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.List              as L
import qualified Data.Map.Strict        as M

import           System.Directory       (doesFileExist, removeFile)
import           System.FilePath        ((</>))

import           Apotheca.Bytes
import           Apotheca.Repo.Internal (Block (..), BlockHeader (..),
                                         BlockId (..), BlockType (..),
                                         SplitStrategy (..))
import           Apotheca.Security.Hash



-- Original

blockDir :: BlockType -> FilePath
blockDir bt = case bt of
  LocalBlock -> "local"
  -- DistributedBlock -> "distributed"
  CacheBlock -> "cache"
  IncomingBlock -> "incoming"
  OutgoingBlock -> "outgoing"

blockDirs = map blockDir [LocalBlock, CacheBlock, IncomingBlock, OutgoingBlock]

blockPath :: BlockHeader -> FilePath
blockPath bh = blockDir (blockType bh) </> b64url (blockId bh)

fullBlockPath :: FilePath -> BlockHeader -> FilePath
fullBlockPath p bh = p </> blockPath bh

doesBlockExist :: FilePath -> BlockHeader -> IO Bool
doesBlockExist p bh = doesFileExist $ fullBlockPath p bh

fetchBlock :: FilePath -> BlockHeader -> IO (Maybe Block)
fetchBlock p bh = do
    exists <- doesBlockExist p bh
    if exists
      then Just <$> B.readFile (fullBlockPath p bh)
      else return Nothing

storeBlock :: FilePath -> BlockHeader -> Block -> IO ()
storeBlock p bh b = B.writeFile (fullBlockPath p bh) b

deleteBlock :: FilePath -> BlockHeader -> IO ()
deleteBlock p bh = do
  exists <- doesBlockExist p bh
  when exists $ removeFile (fullBlockPath p bh)


-- Assign headers

newBlockHeader :: BlockId -> BlockType -> BlockHeader
newBlockHeader bid bt = BlockHeader
  { blockId = bid
  , blockType = bt
  }

makeBlockHeader :: HashStrategy -> BlockType -> Block -> BlockHeader
makeBlockHeader h bt b = newBlockHeader (hashWith h b) bt

assignBlockHeader :: HashStrategy -> BlockType -> Block -> (BlockHeader, Block)
assignBlockHeader h bt b = (makeBlockHeader h bt b, b)

-- validateBlockWithHeader :: HashStrategy -> Block -> BlockHeader -> Bool
-- validateBlockWithHeader h b bh = (hashWith h b) == blockId bh


-- Splitting strategy

splitWith :: SplitStrategy -> ByteString -> [Block]
-- Constant block size split - given a number of bytes, splits into chunks of that size
--  NOTE: Last block is not padded
-- NOTE: Does not require power-of-two for now
splitWith (ExplicitSplit i) = chunksOf i
-- Takes largest square block possible where mn <= n <= mx
--  NOTE: Last block is not padded
splitWith (AdaptiveSplit (mn, mx)) = L.unfoldr $ \b -> if B.null b
    then Nothing
    else Just $ B.splitAt (npotf $ B.length b) b
  where
    -- npotf = nearest power of two floored, leaving any remainder
    -- npotc = nearest power of two ceiled, greedy with padding
    -- NOTE: The `min mx` is technically unnecessary because (npotf n <= n)
    -- NOTE: This is technically npotf-between
    npotf = max mn . min mx . (2^) . floor . logBase 2 . fromIntegral
splitWith NoSplit = (: []) -- Surprise!

-- Shorthand convenience
sws = splitWith

defaultSplitStrat ::SplitStrategy
defaultSplitStrat = AdaptiveSplit (4096, 1048576) -- From 4kb to 1mb



-- Blockstore experiment

data BlockStore m a = BlockStore
  { store    :: a
  , putBlock :: a -> BlockHeader -> Block -> m a
  , getBlock :: a -> BlockHeader -> m (Maybe Block)
  , delBlock :: a -> BlockHeader -> m a
  , hasBlock :: a -> BlockHeader -> m Bool
  }

getStoreM :: (Monad m) => BlockStore m a -> m a
getStoreM bs = return $ store bs

putBlockM :: (Monad m) => BlockHeader -> Block -> BlockStore m a -> m (BlockStore m a)
putBlockM bh b bs = do
  s <- getStoreM bs
  s' <- putBlock bs s bh b
  return $ bs { store = s' }

getBlockM :: (Monad m) => BlockHeader -> BlockStore m a -> m (Maybe Block)
getBlockM bh bs = do
  s <- getStoreM bs
  getBlock bs s bh

delBlockM :: (Monad m) => BlockHeader -> BlockStore m a -> m (BlockStore m a)
delBlockM bh bs = do
  s <- getStoreM bs
  s' <- delBlock bs s bh
  return $ bs { store = s' }

hasBlockM :: (Monad m) => BlockHeader -> BlockStore m a -> m Bool
hasBlockM bh bs = do
  s <- getStoreM bs
  hasBlock bs s bh



-- Experimental blockstores

type MemBlockStore = BlockStore Identity (M.Map BlockHeader Block)

newMemBlockStore :: MemBlockStore
newMemBlockStore = BlockStore
  { store = M.empty
  , putBlock = \s bh b -> return $ M.insert bh b s
  , getBlock = \s bh -> return $ M.lookup bh s
  , delBlock = \s bh -> return $ M.delete bh s
  , hasBlock = \s bh -> return $ M.member bh s
  }

type FileBlockStore = BlockStore IO FilePath

newFileBlockStore :: FilePath -> FileBlockStore
newFileBlockStore p = BlockStore
  { store = p
  , putBlock = \s bh b -> storeBlock s bh b >> return p
  , getBlock = fetchBlock
  , delBlock = \s bh -> deleteBlock s bh >> return p
  , hasBlock = doesBlockExist
  }
