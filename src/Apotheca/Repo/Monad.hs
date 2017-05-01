{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Maybe               

import           System.Directory
import           System.FilePath

import           Apotheca.Logs            (Verbosity (..), logM)
import           Apotheca.Repo.Blocks
import           Apotheca.Repo.Config
import           Apotheca.Repo.Env
import           Apotheca.Repo.Ignore
import           Apotheca.Repo.Internal
import qualified Apotheca.Repo.Manifest   as Mf



-- Monad

type RM m a = StateT Repo m a
type RIO a = RM IO a

runRM :: (Monad m) => RM m a -> Repo -> m (a, Repo)
runRM = runStateT
evalRM :: (Monad m) => RM m a -> Repo -> m a
evalRM = evalStateT
execRM :: (Monad m) => RM m a -> Repo -> m Repo
execRM = execStateT
withRM :: (Monad m) => (Repo -> Repo) -> RM m a -> RM m a
withRM = withStateT
mapRM :: (Monad m, Monad n) =>
  (m (a, Repo) -> n (b, Repo)) -> StateT Repo m a -> StateT Repo n b
mapRM = mapStateT

getRM :: (Monad m) => RM m Repo
getRM = get
putRM :: (Monad m) => Repo -> RM m ()
putRM = put
modifyRM :: (Monad m) => (Repo -> Repo) -> RM m ()
modifyRM = modify
selectRM :: (Monad m) => (Repo -> a) -> RM m a
selectRM = gets



-- io lifter

io :: IO a -> RIO a
io = liftIO



-- Logging

getVerbosity :: (Monad m) => RM m Verbosity
getVerbosity = verbosity <$> getEnv

logRIO :: Verbosity -> String -> RIO ()
logRIO v s = do
  v' <- getVerbosity
  logM v v' s

debug = logRIO Debug
verbose = logRIO Verbose
terse = logRIO Terse
warn = logRIO Warn
fatal = logRIO Fatal



-- Errors

errorIf :: (Monad m) => Bool -> String -> m ()
errorIf b = when b . error

errorWhen :: (Monad m) => m Bool -> String -> m ()
errorWhen f e = f >>= flip errorIf e
errorUnless f = errorWhen (not <$> f)



-- Repo convenience

getEnv :: (Monad m) => RM m Env
getEnv = selectRM repoEnv

getConfig :: (Monad m) => RM m Config
getConfig = selectRM repoConfig

getManifest :: (Monad m) => RM m Manifest
getManifest = selectRM repoManifest

getIgnore :: (Monad m) => RM m Ignore
getIgnore = selectRM repoIgnore



-- Env lifters

modifyEnv :: (Monad m) => (Env -> Env) -> RM m ()
modifyEnv f = do
  r <- getRM
  putRM $ r { repoEnv = f $ repoEnv r }

queryEnv :: (Monad m) => (Env -> a) -> RM m a
queryEnv f = f <$> getEnv

-- Config lifters

modifyConfig :: (Monad m) => (Config -> Config) -> RM m ()
modifyConfig f = do
  r <- getRM
  putRM $ r { repoConfig = f $ repoConfig r }

queryConfig :: (Monad m) => (Config -> a) -> RM m a
queryConfig f = f <$> getConfig

-- Manifest lifters

modifyManifest :: (Monad m) => (Manifest -> Manifest) -> RM m ()
modifyManifest f = do
  r <- getRM
  putRM $ r { repoManifest = f $ repoManifest r }

queryManifest :: (Monad m) => (Manifest -> a) -> RM m a
queryManifest f = f <$> getManifest



-- Environment convenience

isBare :: (Monad m) => RM m Bool
isBare = (== BareRepo) . repoType <$> getEnv

dataPath :: (Monad m) => RM m FilePath
dataPath = queryEnv dataDir



-- Repo create

defaultRepo = Repo
  { repoEnv = defaultEnv
  , repoConfig = defaultConfig
  , repoManifest = Mf.emptyManifest
  , repoIgnore = ignore []
  }

newRepo e = defaultRepo { repoEnv = e }



-- Repo management

openOrCreateRepo :: Env -> IO Repo
openOrCreateRepo e = do
    exists <- doesRepoExist rp
    if exists
      then do
        openRepo e
      else createRepo e
  where
    rp = repoDir e

-- This just sets up the repo directories - repo must still be persisted after.
createRepo :: Env -> IO Repo
createRepo e = do
    -- TODO: Fix / warn on creating bare repo in a directory w/ existing contents
    errorWhen (doesRepoExist rp) "Cannot create repo: Repo already exists."
    --
    createDirectoryIfMissing True dp
    mapM_ (createDirectoryIfMissing True . (dp </>)) blockDirs
    when bare $ B.writeFile (dp </> specialName) "BARE"
    --
    execRM persistRepo $ newRepo e
  where
    bare = repoType e == BareRepo
    rp = repoDir e
    dp = dataDir e

-- NOTE: repoType will be overridden when opening a repo
openRepo :: Env -> IO Repo
openRepo e = do
    -- Ensure exists
    errorUnless (doesRepoExist rp) "Cannot open repo: Repo does not exist."
    -- Fix repotype
    (Just rt) <- getRepoType $ repoDir e
    let e' = e { repoType = rt }
        dp = dataDir e'
    --
    errorUnless (checkRepoData dp) "Cannot load repo: Missing files!"
    c <- readConfig $ dp </> configName
    m <- Mf.readManifest $ dp </> manifestName
    i <- readIgnore $ dp </> ignoreName
    --
    return Repo
      { repoEnv = e'
      , repoConfig = c
      , repoManifest = m
      , repoIgnore = i
      }
  where
    rp = repoDir e

persistRepo :: RIO ()
persistRepo = do
    dp <- dataPath
    getConfig >>= io . writeConfig (dp </> configName)
    getManifest >>= io . Mf.writeManifest (dp </> manifestName)
    selectRM repoIgnore >>= io . writeIgnore (dp </> ignoreName)
    io $ B.writeFile (dp </> distName) "DISTRIBUTED_PLACEHOLDER"

destroyRepo :: RIO ()
destroyRepo = do
  rp <- queryEnv repoDir
  dp <- dataPath
  exists <- io $ doesRepoExist rp
  errorIf (not exists) "Cannot destroy repo: Repo does not exist."
  io $ removeDirectoryRecursive dp



-- Blocks

-- TODO: (isLarge :: Bool, isRandomAccess :: Bool)

getDefaultSplitStrategy :: (Monad m) => RM m SplitStrategy
getDefaultSplitStrategy = queryConfig defaultSplit

getLargeSplitStrategy :: (Monad m) => RM m (Maybe SplitStrategy)
getLargeSplitStrategy = queryConfig largeSplit

getLargeSplitLimit :: (Monad m) => RM m Int
getLargeSplitLimit = queryConfig largeSplitLimit

isLarge :: (Monad m) => ByteString -> RM m Bool
isLarge bs = do
    lsplit <- getLargeSplitStrategy
    limit <- getLargeSplitLimit
    return $ maybe False (const $ len > limit) lsplit
  where len = B.length bs

getSplitStrategy :: (Monad m) => ByteString -> RM m SplitStrategy
getSplitStrategy bs = do
  large <- isLarge bs
  if large
    then fromJust <$> getLargeSplitStrategy
    else getDefaultSplitStrategy

splitBlocks :: (Monad m) => ByteString -> RM m [Block]
splitBlocks bs = do
  s <- getSplitStrategy bs
  return $ splitWith s bs

assignBlockHeaders :: (Monad m) => BlockType -> [Block] -> RM m [(BlockHeader, Block)]
assignBlockHeaders bt bs = do
  bh <- queryConfig blockHash
  return $ map (assignBlockHeader bh bt) bs



