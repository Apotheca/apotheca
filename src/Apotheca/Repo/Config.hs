module Apotheca.Repo.Config
( Config (..)
, configName
, defaultConfig
, readConfigFile, writeConfigFile
-- Re-exports
, WatchStrategy (..)
) where

import           GHC.Generics

import qualified Data.Aeson               as A
import qualified Data.ByteString.Char8    as BC
import qualified Data.Yaml                as Y

import           System.Directory         (makeAbsolute)

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Repo.Path       (Path)
import           Apotheca.Repo.Watcher
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash

import           Apotheca.Logs
import           Apotheca.Repo.Internal      (Config (..), SplitStrategy (..))



configName = "CONFIG"

defaultConfig :: Config
defaultConfig = Config
  { selectedManifest = Nothing
  , encryptManifest  = False
  , defaultSplit     = NoSplit
  , largeSplit       = Just . ExplicitSplit $ 2 ^ 22
  , largeSplitLimit  = 2 ^ 22 -- 4mb
  , blockHash        = newHashStrategy SHA2
  , defaultCipher    = Nothing
  , watchedDirs = []
  }

configEncoding = YAMLFormat @> NoCompression

readConfigFile :: FilePath -> IO Config
readConfigFile p = do
  mcfg <- decodeWithFile p configEncoding
  case mcfg of
    Just cfg -> return cfg
    Nothing -> error "Could not parse config."

writeConfigFile :: FilePath -> Config -> IO ()
writeConfigFile p = encodeWithFile p configEncoding
