module Caligo.Repo.Config
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

import           Caligo.Bytes
import           Caligo.Encodable
import           Caligo.Repo.Path       (Path)
import           Caligo.Repo.Watcher
import           Caligo.Security.Cipher (CipherStrategy)
import           Caligo.Security.Hash   (HashStrategy)

import           Caligo.Logs
import           Caligo.Repo.Types      (Config (..))



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
