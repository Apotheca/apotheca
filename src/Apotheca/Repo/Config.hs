module Apotheca.Repo.Config
( Config (..)
, configName
, defaultConfig
, readConfigFile, writeConfigFile
-- Re-exports
, WatchStrategy (..)
) where

import           GHC.Generics

import qualified Data.Aeson             as A
import qualified Data.ByteString.Char8  as BC
import qualified Data.Yaml              as Y

import           System.Directory       (makeAbsolute)

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Repo.Path       (Path)
import           Apotheca.Repo.Watcher
import           Apotheca.Security.Cipher (CipherStrategy)
import           Apotheca.Security.Hash   (HashStrategy)

import           Apotheca.Logs
import           Apotheca.Repo.Types      (Config (..))



configName = "CONFIG"

defaultConfig :: Config
defaultConfig = Config
  { selectedManifest = Nothing
  , encryptManifest = False
  -- , defaultSplit    = Nothing -- A system default will be provided
  -- , defaultHash     = Nothing
  -- , defaultCipher   = Nothing
  -- , defaultExchange = Nothing
  -- , defaultSigning  = Nothing
  , watchedDirs = []
  }

defaultConfigEncoding = YAMLFormat @> NoCompression

readConfigFile :: FilePath -> IO Config
readConfigFile p = do
  mcfg <- decodeWithFile p defaultConfigEncoding
  case mcfg of
    Just cfg -> return cfg
    Nothing -> error "Could not parse config."

writeConfigFile :: FilePath -> Config -> IO ()
writeConfigFile p = encodeWithFile p defaultConfigEncoding
