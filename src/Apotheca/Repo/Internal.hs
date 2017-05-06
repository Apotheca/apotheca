{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Apotheca.Repo.Internal where

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
  -- | DistributedBlock
  | CacheBlock
  | IncomingBlock
  | OutgoingBlock
  deriving (Show, Read, Eq, Ord, Generic)

instance Serialize BlockType
instance ToJSON BlockType
instance FromJSON BlockType
instance Encodable BlockType


data BlockHeader = BlockHeader
  { blockId   :: BlockId  -- Is also block hash, which is a per-repo setting
  , blockType :: BlockType
  } deriving (Show, Read, Eq, Ord, Generic)

instance Serialize BlockHeader
instance ToJSON BlockHeader
instance FromJSON BlockHeader
instance Encodable BlockHeader



-- Split strategy - how a datum is split into blocks

-- TODO: NoSplit is a split strategy, but NoCipher is not a cipher strategy
--  because it uses a Maybe CipherStrategy instead.
--  For consistency, should strategies be the form of NoFoo, or of Maybe Foo?
--  Viewed from a type perspective, should every FooStrategy have a NoStrategy?
--  Strategies with a NoStrategy == some sort of transformer?
--  Ie: type CipherTransformer = Maybe CipherStrategy

data SplitStrategy
  = ConstSplit Int
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

type EntryId = Integer
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
  -- , manifestTime :: Int -- Used to inform AccessHeader of current time
  , ctr     :: Integer -- Next available EntryId
  } deriving (Show, Read, Generic)

instance Serialize Manifest
instance ToJSON Manifest
instance FromJSON Manifest
instance Encodable Manifest

data Entry = Entry
  { entryId       :: EntryId
  , parentId      :: EntryId
  , accessHeader  :: AccessHeader
  , contentHeader :: ContentHeader
  } deriving (Show, Read, Generic)

instance Serialize Entry
instance ToJSON Entry
instance FromJSON Entry
instance Encodable Entry

-- TODO: Rename ContentHeader
data ContentHeader
  = DirContents DirMap -- TODO: DirContents DirHeader DirMap
  -- | FileContents [BlockId]
  | FileContents FileHeader
  -- | InlineEntry ByteString
  -- | SymlinkEntry Path -- Soft link, points to a path that may or may not exist
  -- | HardLink EntryId -- Hard link, points to an entry that must exist
  -- | SubManifest Manifest
  deriving (Show, Read, Generic)

instance Serialize ContentHeader
instance ToJSON ContentHeader
instance FromJSON ContentHeader
instance Encodable ContentHeader

data AccessHeader = AccessHeader
  { modifyTime :: Int
  -- , accessTime :: Int -- NOTE: Write-intensive, not useful at this time
  -- , createTime :: Int -- NOTE: Not ctime
  -- , metadata   :: [(String, String)] -- Future
  } deriving (Show, Read, Generic)

instance Serialize AccessHeader
instance ToJSON AccessHeader
instance FromJSON AccessHeader
instance Encodable AccessHeader

data FileHeader = FileHeader
  { fhSize         :: Int
  , fhCompression  :: GzipCompression
  , fhHashHeader   :: Maybe HashHeader -- Plaintext checksum; should this be in AccessHeader?
  , fhCipherHeader :: Maybe CipherHeader -- Ciphertext
  , fhBlockHeaders :: [BlockHeader]
  } deriving (Show, Read, Generic)

instance Serialize FileHeader
instance ToJSON FileHeader
instance FromJSON FileHeader
instance Encodable FileHeader



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

data PutFlags = PutFlags
  { pfWriteMode   :: WriteMode
  -- , pfTime        :: Maybe Int
  , pfHashStrat   :: Maybe HashStrategy -- Plaintext checksum
  , pfCompression :: Maybe GzipCompression
  , pfCipherStrat :: Maybe CipherStrategy -- Ciphertext
  } deriving (Show, Read, Eq)

data GetFlags = GetFlags
  { gfWriteMode :: WriteMode
  } deriving (Show, Read, Eq)

-- TODO: Delflags later?
-- data DelFlags = DelFlags
--   { dfForce :: Bool
--   } deriving (Show, Read, Eq)



-- New Style?

data WriteFlags = WriteFlags
  { wfWriteMode      :: WriteMode
  , wfAccessFlags    :: AccessFlags
  , wfTransformFlags :: TransformFlags
  } deriving (Show, Read, Eq)

data AccessFlags = AccessFlags
  { afModifyTime :: Int
  , afHashStrat  :: Maybe HashStrategy -- Plaintext checksum
  } deriving (Show, Read, Eq)

data TransformFlags = TransformFlags
  { tfCompression :: Maybe GzipCompression
  , tfCipherStrat :: Maybe CipherStrategy -- Ciphertext
  } deriving (Show, Read, Eq)



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



-- Environment - impermanent, per-run environment variables
--  This is for values that may be set from cmd args to override values in the
--  config for the duration of the execution. The environment is never written
--  to disk, it is constructed from command-line args (or another external
--  argument source) - for example, any cmd flags before the first subcommand
--  Verbosity is a good example.

data Env = Env
  { repoDir     :: FilePath
  , repoType    :: RepoType
  , extDir      :: FilePath -- External working path - rename `ewp`?
  , intDir      :: Path     -- Internal working path - rename `iwp`?
  , selManifest :: Maybe String
  , dryRun      :: Bool  -- Dry run, don't write any changes, just log them
  -- , splitStrat :: SplitStrategy  -- Block splitting strat
  -- , hashStrat :: Maybe HashStrategy -- File checksums
  -- , cipherStrat :: Maybe CipherStrategy -- Symmetric cipher use
  -- , exchangeStrat :: Maybe ExchangeStrategy -- Key exchange (asymmetric / pubkey)
  -- , signingStrat :: Maybe SigningStrategy  --
  , magicSlash  :: Bool
  , verbosity   :: Verbosity
  } deriving (Show, Read, Generic)

-- instance Serialize Env
-- instance ToJSON Env
-- instance FromJSON Env
-- instance Encodable Env

data RepoType = HiddenRepo | BareRepo deriving (Show, Read, Eq)



data Repo = Repo
  { repoEnv      :: Env
  , repoConfig   :: Config
  , repoManifest :: Manifest
  , repoIgnore   :: Ignore
  -- TODO: Implement these later
  -- , distributedParams :: Maybe DistConfig ... -- Should include keyspace, etc
  -- TODO: Potential block buffer - for pure interface that defers writing
  --  blocks out until explicitly flushed - might need to be transactions
  --  instead - added [(BlockId,Block)] / [removed (BlockId)]

  -- , bufferedBlocks :: MVar (M.Map (BlockType, Key) Block) -- Needs an accompanying flushBlocks :: IO ()
  -- or
  -- , repoBlockStore :: a -- A (potentially flushable) blockstore
  } deriving (Show, Read)



-- Inherited values

data Inherited a
  = Explicit a
  | Inherit
  deriving (Show, Read, Eq)

inherit :: a -> Inherited a -> a
inherit _ (Explicit b) = b
inherit a Inherit = a

inheritFrom :: (a -> b) -> a -> Inherited b -> b
inheritFrom _ _ (Explicit b) = b
inheritFrom f a Inherit = f a
