module Apotheca.Repo.Config
( Config (..)
, defaultConfig
, readConfig, writeConfig
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
import           Apotheca.Repo.Internal   (Config (..), SplitStrategy (..))



defaultConfig :: Config
defaultConfig = Config
  { selectedManifest = Nothing
  , encryptManifest  = False
  , blockHash        = newHashStrategy SHA2
  , defaultSplit     = NoSplit
  , largeSplitLimit  = Just $ 2 ^ 22 -- 4mb
  , defaultCompression = NoCompression
  , defaultHash      = Just $ newHashStrategy SHA2
  , defaultCipher    = Just $ defaultCipherStrategy
  , watchedDirs = []
  }

configEncoding = JSONFormat @> NoCompression

readConfig :: FilePath -> IO Config
readConfig p = do
  mcfg <- decodeWithFile p configEncoding
  case mcfg of
    Just cfg -> return cfg
    Nothing -> error "Could not parse config."

writeConfig :: FilePath -> Config -> IO ()
writeConfig p = encodeWithFile p configEncoding
