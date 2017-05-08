module Apotheca.Repo.Env where

import           Control.Monad

import qualified Data.List              as L (unfoldr)
import           Data.Maybe

import           Apotheca.Logs
import           Apotheca.Repo.Blocks
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Path

import           System.Directory
import           System.FilePath



-- Special names

specialName = ".apo" -- Change to .store (need to move vault first)
manifestName = "MANIFEST"
configName = "CONFIG"
ignoreName = "IGNORE"
distName = "DISTRIBUTED"



-- Create

defaultEnv = Env
  { repoDir = "."
  , repoType = HiddenRepo
  , extDir = "." -- Made absolute elsewhere?
  , intDir = []
  , selManifest = Nothing
  , dryRun = False
  , magicSlash = True
  , verbosity = Warn
  }



-- Helpers

dataDir :: Env -> FilePath
dataDir e = case repoType e of
  BareRepo -> repoDir e
  HiddenRepo -> repoDir e </> specialName


-- Repo env queries
-- NOTE: findRepo makes a path absolute first before using pathAncestry
findRepo :: FilePath -> IO (Maybe FilePath)
findRepo p = do
  ps <- pathAncestry <$> makeAbsolute p
  ps' <- filterM doesRepoExist ps
  if null ps'
    then return Nothing
    else return $ Just $ head ps'

doesRepoExist :: FilePath -> IO Bool
doesRepoExist p = isJust <$> getRepoType p

doesBareRepoExist :: FilePath -> IO Bool
doesBareRepoExist = doesFileExist . (</> specialName)

doesHiddenRepoExist :: FilePath -> IO Bool
doesHiddenRepoExist = doesDirectoryExist . (</> specialName)

getRepoType :: FilePath -> IO (Maybe RepoType)
getRepoType p = do
  bre <- doesBareRepoExist p
  hre <- doesHiddenRepoExist p
  return $ case (bre, hre) of
    (True, _) -> Just BareRepo
    (_, True) -> Just HiddenRepo
    _ -> Nothing -- Degenerate case, shouldn't happen

checkRepoData :: FilePath -> IO Bool
checkRepoData p = do
  manExists <- doesFileExist $ p </> manifestName
  cfgExists <- doesFileExist $ p </> configName
  igExists <- doesFileExist $ p </> ignoreName
  -- distributedFile is optional, non-required
  dirExists <- mapM (doesDirectoryExist . (p </>)) blockDirs
  return $ and $ [manExists,cfgExists,igExists] ++ dirExists



-- FilePath helpers

-- NOTE: Does not include initial path - use pathAncestry for that
pathAncestors :: FilePath -> [FilePath]
pathAncestors = L.unfoldr f
  where
    f p =
      let p' = takeDirectory p
      in if p == p' then Nothing else Just (p', p')

-- NOTE: Does include initial path
pathAncestry :: FilePath -> [FilePath]
pathAncestry p = p : pathAncestors p



-- IO Helpers

getAncestors :: FilePath -> IO [FilePath]
getAncestors p = pathAncestors <$> canonicalizePath p

getAncestry :: FilePath -> IO [FilePath]
getAncestry p = pathAncestry <$> canonicalizePath p

-- Should be called only on directories, errors otherwise
-- Returns a relative path, not just name
-- Does *NOT* return "." or ".." like getDirectoryContents
-- TODO: Rename getDirectoryPaths
-- NOTE: Naive use of specialName - won't work for bare repos
getDirectory :: FilePath -> IO [FilePath]
getDirectory p = map (p </>) . filter stripSpecial <$> getDirectoryContents p
  where stripSpecial a = a /= "." && a /= ".." && a /= specialName

-- Ditto
getDirectoryRecursive :: FilePath -> IO [FilePath]
getDirectoryRecursive p = do
  dc <- getDirectory p
  dc's <- filterM doesDirectoryExist dc >>= mapM getDirectoryRecursive
  return $ dc ++ concat dc's
