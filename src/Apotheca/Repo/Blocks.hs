module Apotheca.Repo.Blocks where

import           Control.Monad          (when)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.List              as L

import           System.Directory       (doesFileExist, removeFile)
import           System.FilePath        ((</>))

import           Apotheca.Bytes
import           Apotheca.Repo.Types    (Block (..), BlockHeader (..),
                                         BlockId (..), BlockType (..),
                                         SplitStrategy (..))
import           Apotheca.Security.Hash



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
