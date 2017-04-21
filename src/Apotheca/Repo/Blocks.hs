module Apotheca.Repo.Blocks where

import           Control.Monad       (when)

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B
import qualified Data.List           as L

import           System.Directory    (doesFileExist, removeFile)
import           System.FilePath     ((</>))

import           Apotheca.Bytes
import           Apotheca.Repo.Types (Block (..), BlockId (..), BlockType (..),
                                      SplitStrategy (..))



blockDir :: BlockType -> FilePath
blockDir bt = case bt of
  LocalBlock -> "local"
  CacheBlock -> "cache"
  IncomingBlock -> "incoming"
  OutgoingBlock -> "outgoing"

blockDirs = map blockDir [LocalBlock, CacheBlock, IncomingBlock, OutgoingBlock]

-- NOTE: lots of FilePath -> BlockType -> Key -> ...
--  type BlockRef = (FilePath, BlockType, Key)

blockPath :: BlockType -> BlockId -> FilePath
blockPath bt bid = blockDir bt </> b64url bid

doesBlockExist :: FilePath -> BlockType -> BlockId -> IO Bool
doesBlockExist p bt bid = doesFileExist $ p </> blockPath bt bid

fetchBlock :: FilePath -> BlockType -> BlockId -> IO (Maybe Block)
fetchBlock p bt bid = do
    exists <- doesBlockExist p bt bid
    if exists
      then Just <$> B.readFile (p </> blockPath bt bid)
      else return Nothing

storeBlock :: FilePath -> BlockType -> BlockId -> Block -> IO ()
storeBlock p bt bid b = B.writeFile (p </> blockPath bt bid) b

deleteBlock :: FilePath -> BlockType -> BlockId -> IO ()
deleteBlock p bt bid = do
  exists <- doesBlockExist p bt bid
  when exists $ removeFile (p </> blockPath bt bid)


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
splitWith NoSplit = (: [])

-- Shorthand convenience
sws = splitWith
