{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Apotheca.Repo.Types where

import           GHC.Generics

import           Data.ByteString          (ByteString)
import qualified Data.Map.Strict          as M

import qualified System.FilePath.Glob     as G

import           Apotheca.Encodable
import           Apotheca.Logs            (Verbosity)
import           Apotheca.Misc
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash



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
  = ExplicitSplit Int
  | AdaptiveSplit (Int, Int)
  | NoSplit
  deriving (Show, Read, Eq, Generic)

instance Serialize SplitStrategy
instance ToJSON SplitStrategy
instance FromJSON SplitStrategy
instance Encodable SplitStrategy



data Config = Config
  { selectedManifest :: Maybe String
  , encryptManifest  :: Bool
  , defaultSplit     :: SplitStrategy
  , largeSplit       :: Maybe SplitStrategy
  , largeSplitLimit  :: Int
  , blockHash        :: HashStrategy
  , defaultCipher    :: Maybe CipherStrategy
  -- , defaultExchange :: Maybe ExchangeStrategy
  -- , defaultSigning  :: Maybe SigningStrategy
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



-- Modes

data SyncMode
  = SynchronizeMode -- Adds and deletes files to synchronize files
  | AdditiveMode
  | DeadDropMode -- Like additive mode, but deletes src file afterwards
  deriving (Show, Read, Eq, Generic)

instance Serialize SyncMode
instance ToJSON SyncMode
instance FromJSON SyncMode
instance Encodable SyncMode



data Transaction
  = Push
  | Pull
  | Transfer
  deriving (Show, Read, Eq, Generic)

instance Serialize Transaction
instance ToJSON Transaction
instance FromJSON Transaction
instance Encodable Transaction



-- Write mode for put / get
data WriteMode
  = Add -- Add if nonexistent, ignore if existent
  | Overwrite -- Add if nonexistent, overwrite if existent
  | Update -- Add if nonexistent, overwrite if more recent
  | Freshen -- Ignore if non-existent, overwrite if more recent
  deriving (Show, Read, Eq)



-- WatchStrategy

-- NOTE: Watched directories are assumed to be push, and not pull
data WatchStrategy = WatchStrategy
  { syncMode      :: SyncMode
  , syncDirection :: Transaction
  , globFilter    :: Maybe [String]
  , sourcePath    :: FilePath
  , destPath      :: FilePath
  , pollInterval  :: Int -- Microseconds
  , forcePolling  :: Bool
  } deriving (Show, Read, Eq, Generic)

instance Serialize WatchStrategy
instance ToJSON WatchStrategy
instance FromJSON WatchStrategy
instance Encodable WatchStrategy
