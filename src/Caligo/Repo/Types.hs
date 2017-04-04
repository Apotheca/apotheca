{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Caligo.Repo.Types where

import           GHC.Generics

import           Data.ByteString      (ByteString)
import qualified Data.Map.Strict      as M

import qualified System.FilePath.Glob as G

import           Caligo.Encodable
import           Caligo.Logs          (Verbosity)
import           Caligo.Misc



-- Blocks

type Block = ByteString
type BlockId = ByteString

data BlockType
  = LocalBlock
  | CacheBlock
  | IncomingBlock
  | OutgoingBlock
  deriving (Show, Read, Eq)



-- Split strategy - how a datum is split into blocks

data SplitStrategy
  = ExplicitSz Int
  | AdaptiveSz (Int, Int)
  deriving (Show, Read, Eq)



data Config = Config
  { selectedManifest :: Maybe String
  , encryptManifest  :: Bool
  -- , defaultSplit    :: Maybe SplitStrategy
  -- , defaultHash     :: Maybe HashStrategy
  -- , defaultCipher   :: Maybe CipherStrategy
  -- , defaultExchange :: Maybe ExchangeStrategy
  -- , defaultSigning  :: Maybe SigningStrategy
  -- , watchedDirs     :: [(FilePath,WatchStrategy)]
  , watchedDirs      :: [WatchStrategy]
  } deriving (Read, Show, Generic)

instance Serialize Config
instance ToJSON Config
instance FromJSON Config
instance Encodable Config



-- Ignore

type Ignore = [(String, G.Pattern)]



-- Manifest
-- NOTE: This is basically an inode table, as the first version is modeled after
--  the POSIX / *nix inode structure. Right now it is a simplified version, as
--  it does not have metadata and other bits yet (see: Manifest p d f).

type EntryId = ByteString
type EntryMap = M.Map EntryId Entry
type DirMap = M.Map String EntryId

instance ToJSON (M.Map EntryId Entry) where
  toJSON m = toJSON $ M.elems m
instance FromJSON (M.Map EntryId Entry) where
  parseJSON v = do
    es <- parseJSON v
    return . M.fromList $ map (tagWith entryId) es

data Manifest = Manifest
  { topId   :: EntryId  -- Is always B.empty for now, can be manifestId later
                            -- Is always a directory for now
                            -- Utility of inline or single-file manifests?
  , entries :: EntryMap
  -- , blocks  :: [BlockId]  -- Cached list of all blocks owned by entries in
                              --  the manifest, for convenience
  } deriving (Show, Read, Generic)

instance Serialize Manifest
instance ToJSON Manifest
instance FromJSON Manifest
instance Encodable Manifest

data Entry = Entry
  { entryId       :: EntryId
  , parentId      :: EntryId
  , entryContents :: EntryContents
  -- , entryMetaData :: EntryMetadata
  } deriving (Show, Read, Generic)

instance Serialize Entry
instance ToJSON Entry
instance FromJSON Entry
instance Encodable Entry

data EntryContents
  = DirContents DirMap
  | FileContents [BlockId]
  -- | InlineEntry ByteString
  -- | SymlinkEntry EntryId
  -- | SubManifest Manifest
  deriving (Show, Read, Generic)

instance Serialize EntryContents
instance ToJSON EntryContents
instance FromJSON EntryContents
instance Encodable EntryContents



-- Path

type Path = [PathElement]
type PathElement = String
type Glob = String



-- WatchStrategy

data WatchStrategy = WatchStrategy
  { sourcePath   :: FilePath
  , targetPath   :: Path
  , watchMode    :: WatchMode
  , pollInterval :: Int -- Microseconds
  , forcePolling :: Bool
  , globFilter   :: Maybe [String]
  -- , watchIgnore  :: Maybe Ignore
  } deriving (Show, Read, Generic)

instance Serialize WatchStrategy
instance ToJSON WatchStrategy
instance FromJSON WatchStrategy
instance Encodable WatchStrategy

-- TODO: Rename ConsumeMode? - since this is used in the command-line as well
-- TODO: Do we need a 'ProduceMode' for 'pulling'?
-- Does this make sense for representing both:
--  push (local -> repo)
--  and
--  pull (repo -> local)
--  deaddrop pull would delete the file from the repo after pulling, but that
--  may be sensible behavior. Should also allow:
--  (repo -> repo), but (local -> local) makes no sense just like (remote -> remote)
--  doesn't for rsync
data WatchMode
  = SynchronizeMode -- Adds and deletes files to synchronize files
  | AdditiveMode Bool  -- Adds / overwrites files, does not delete -- AdditiveMode BoolOverwrite
  | DeadDropMode -- Like additive mode, but deletes src file afterwards
  deriving (Show, Read, Eq, Generic)

instance Serialize WatchMode
instance ToJSON WatchMode
instance FromJSON WatchMode
instance Encodable WatchMode
