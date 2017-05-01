{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Repo
( Env (..)
, defaultEnv
, Repo (..)
, RepoType (..)
, newRepo
, createRepo
, openRepo
-- , openOrCreateRepo
, persistRepo
, destroyRepo
, findRepo
, isBare
, getRepoType
, doesRepoExist
, doesBareRepoExist
, doesHiddenRepoExist
-- Lifters
, modifyManifest
, queryManifest
, mqueryManifest
-- Map-like
, WriteMode (..)
-- Data
, getDatum
, putDatum
, delDatum
-- Handle
, getHandle
, putHandle
-- File
-- , getFile
-- , putFile
-- Path
, listPath
, getPath
, putPath
, delPath
-- Push / pull
, pushPath
, pullPath
-- , transferPath
-- Helpers
-- NOTE: Bad nomenclature, should probably be in Path
, Glob (..)
, filterExt
, filterInt
, getDirectory
, getDirectoryRecursive
, globDirectoryIO
, globDirectoryIO'
, globDirectoryIO''

-- Exposed for multiplexing, should be refactored
, multiplex, multiplex'
) where

import           Prelude                       hiding (readFile, writeFile)

import           GHC.Generics

import           Control.Monad                 (filterM, foldM, mapM, mapM_,
                                                unless, when)

import           Data.Bits
import           Data.Bits.ByteString
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromJust, isJust, maybe)
import           Data.Time.Clock               (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX         (utcTimeToPOSIXSeconds)

import           System.Directory              (createDirectoryIfMissing,
                                                doesDirectoryExist,
                                                doesFileExist,
                                                getDirectoryContents,
                                                getModificationTime,
                                                makeAbsolute,
                                                removeDirectoryRecursive)
import           System.FilePath               (dropTrailingPathSeparator,
                                                hasTrailingPathSeparator,
                                                joinPath, normalise, splitPath,
                                                takeDirectory, takeExtension,
                                                takeFileName, (</>))
import qualified System.FilePath.Glob          as G
import           System.IO                     (Handle, IOMode (..), withFile)

import           Apotheca.Bytes
import           Apotheca.Distributed.Keyspace
import           Apotheca.Encodable
import           Apotheca.Logs
import           Apotheca.Misc
import           Apotheca.Repo.Blocks
import           Apotheca.Repo.Config
import           Apotheca.Repo.Ignore
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Manifest        (Manifest, emptyManifest,
                                                readManifestFile,
                                                writeManifestFile)
import qualified Apotheca.Repo.Manifest        as Mf
import           Apotheca.Repo.Path
import           Apotheca.Security.Hash



-- Git implementation reference:
--  https://hackage.haskell.org/package/gitlib-0.6.5

-- File structure of repo data dir $ (is either ./ (bare) or ./.apo/ (hidden))
--  $/local/ -- Local block storage; for non-distributed repo or owned blocks
--  $/cache/  -- Blocks cached to speed up network
--  $/incoming/ -- Blocks downloaded and awaiting use
--  $/outgoing/ -- Blocks awaiting upload
--  $/CONFIG  -- Local repo configuration
--  $/MANIFEST -- Repo data contents - single manifest for now
--    Maybe rename 'CONTENTS' if going with multiple-manifests-per-repo style
--  $/DISTRIBUTED -- Distributed repo / node config, optional)
--  ./.apo -- A hidden FILE signifying a bare repo (otherwise is dir)

-- NOTE: This is all IO and context passing right now
--  Will promote it to a monad later, maybe (MonadIo m) => m a, or STM/TVar
--  STM/TVar is probably better because we can do the MonadIO on top
-- TODO: Manifest.hs is written nicely, this badly needs to be refactored to
--  match that quality and expressive power.

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

dataDir :: Env -> FilePath
dataDir e = case repoType e of
  BareRepo -> repoDir e
  HiddenRepo -> repoDir e </> specialName

dataPath = dataDir . repoEnv



defaultRepo = Repo
  { repoEnv = defaultEnv
  , repoConfig = defaultConfig
  , repoManifest = emptyManifest
  , repoIgnore = ignore []
  }

newRepo e = defaultRepo { repoEnv = e }

isBare :: Repo -> Bool
isBare = (== BareRepo) . repoType . repoEnv

-- Data paths

specialName = ".apo" -- Change to .store (need to move vault first)
manifestName = "MANIFEST"
distName = "DISTRIBUTED"

internalPath :: FilePath -> Repo -> FilePath
internalPath p = (</> p) . dataPath

-- Creating

errorWhen :: String -> Bool -> IO ()
errorWhen = flip when . error

-- Returns the repo for chaining
createRepo :: Env -> IO Repo
createRepo e = do
    -- TODO: Fix / warn on creating bare repo in a directory w/ existing contents
    errorWhen "Cannot create repo: Repo already exists." =<< doesRepoExist (repoDir e)
    -- Create stuff
    createDirectoryIfMissing True dp
    mapM_ (createDirectoryIfMissing True . (dp </>)) blockDirs
    when bare $ B.writeFile (dp </> specialName) $ BC.pack "BARE"
    persistRepo $ newRepo e
  where
    bare = repoType e == BareRepo
    dp = dataDir e

-- NOTE: repoType will be overridden when opening a repo
openRepo :: Env -> IO Repo
openRepo e = do
  errorWhen "Cannot open repo: Repo does not exist." . not =<< doesRepoExist (repoDir e)
  (Just rt) <- getRepoType $ repoDir e
  let e' = e { repoType = rt }
      dp = dataDir e'
  c <- readConfigFile $ dp </> configName
  m <- readManifestFile $ dp </> manifestName
  i <- readIgnoreFile $ dp </> ignoreName
  return Repo
    { repoEnv = e'
    , repoConfig = c
    , repoManifest = m
    , repoIgnore = i
    }

persistRepo :: Repo -> IO Repo
persistRepo r = do
    writeConfigFile (dp </> configName) c
    writeManifestFile (dp </> manifestName) m
    writeIgnoreFile (dp </> ignoreName) i
    B.writeFile (dp </> distName) "DISTRIBUTED_PLACEHOLDER"
    return r
  where
    dp = dataPath r
    c = repoConfig r
    m = repoManifest r
    i = repoIgnore r

destroyRepo :: Repo -> IO ()
destroyRepo r = do
    exists <- doesRepoExist . repoDir . repoEnv $ r
    errorWhen "Cannot destroy repo: Repo does not exist." $ not exists
    removeDirectoryRecursive $ dataPath r

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



-- Store object

-- blockType :: Repo -> Key -> BlockType
-- blockType _ _ = LocalBlock



-- Helpers

isLarge :: Repo -> ByteString -> Bool
isLarge r bs = case largeSplit c of
    Just ss -> B.length bs >= largeSplitLimit c
    Nothing -> False
  where
    c = repoConfig r

-- splitStrat :: Repo -> SplitStrategy
-- splitStrat r = AdaptiveSplit (4096, 1048576) -- From 4kb to 1mb

splitBlocks :: Repo -> ByteString -> [Block]
splitBlocks r bs = splitWith strat bs
  where
    c = repoConfig r
    strat = if isLarge r bs
      then fromJust $ largeSplit c
      else defaultSplit c

assignBlockHeaders :: Repo -> BlockType -> [Block] -> [(BlockHeader, Block)]
assignBlockHeaders r bt bs = map (assignBlockHeader h bt) bs
  where h = blockHash $ repoConfig r



-- Main exported functions + helpers

-- Lifters

modifyManifest :: (Manifest -> Manifest) -> Repo -> Repo
modifyManifest f r = r { repoManifest = f $ repoManifest r }

queryManifest :: (Manifest -> a) -> Repo -> a
queryManifest f = f . repoManifest

mqueryManifest :: (Manifest -> (a, Manifest)) -> Repo -> (a, Repo)
mqueryManifest f r = (a, r { repoManifest = m })
  where (a, m) = f $ repoManifest r



-- Manifest convenience manipulations - takes the 'create/
-- TODO: rename to fooManPath/Directory/etc, too confusing otherwise

-- NOTE: Combines removePath and removePathForcibly
removePath :: Bool -> Path -> Repo -> Repo
removePath force = modifyManifest . f
  where f = if force then Mf.removePathForcibly else Mf.removePath

-- NOTE: Creates entire dir path
createDirectory :: Path -> Repo -> Repo
createDirectory p = modifyManifest (Mf.createDirectoryIfMissing True p)

readDirectory :: Path -> Repo -> [Path]
readDirectory = queryManifest . Mf.readDirectory
readDirectoryRecursive :: Path -> Repo -> [Path]
readDirectoryRecursive = queryManifest . Mf.readDirectoryRecursive

removeDirectory :: Path -> Repo -> Repo
removeDirectory = modifyManifest . Mf.removeDirectoryRecursive

-- NOTE: Always creates parents
createFile :: Path -> FileHeader -> Repo -> Repo
createFile p fh = modifyManifest (Mf.createFile p fh) . createDirectory (parent p)

readFile :: Path -> Repo -> FileHeader
readFile = queryManifest . Mf.readFile

writeFile :: Path -> FileHeader -> Repo -> Repo
writeFile p fh = modifyManifest (Mf.writeFile p fh)

removeFile :: Path -> Repo -> Repo
removeFile = modifyManifest . Mf.removeFile



-- Map-like
-- NOTE: Paths must already be validated or an error may occur
-- NOTE: getFile/putFile/delFile as in file-in-the-manifest, not file-on-disk
-- These exist because file actions modify blocks, directory actions do not
-- NOTE: only get/putPath fully obey -opr/cmdline flags properly/completely

-- TODO: Need to split these into get-/put- ext/int

getDatum :: GetFlags -> Path -> Repo -> IO ByteString
getDatum gf p r = B.concat <$> mapM fetchLocalBlock bhs
  where
    bhs = dataBlockHeaders $ readFile p r
    fetchLocalBlock :: BlockHeader -> IO Block
    fetchLocalBlock = (fromJust <$>) . fetchBlock (dataPath r)

putDatum :: PutFlags -> Path -> ByteString -> Repo -> IO Repo
putDatum pf p b r = do
    -- Remove previous version if exists
    r' <- if queryManifest (Mf.pathIsFile p) r
      then delDatum p r
      else return r
    -- Write blocks to disk
    mapM_ storeLocalBlock pairs
    -- Update manifest, return
    return $ createFile p fh r'
  where
    fh = FileHeader
      { dataSize = B.length b
      , dataCompression = NoCompression
      , dataHashHeader = Nothing
      , dataCipherHeader = Nothing
      , dataBlockHeaders = map fst pairs
      }
    blocks = splitBlocks r b
    pairs = assignBlockHeaders r LocalBlock $ blocks
    storeLocalBlock :: (BlockHeader, Block) -> IO ()
    storeLocalBlock (bh, b) = storeBlock (dataPath r) bh b
    -- TODO: pre- vs post-encryption splitting
    --  Encryption /before/ splitting == whole-file encryption
    --  Encryption /after/ splitting == per-block encryption
    --  This means that if per-block encryption is specified, the block split
    --  strategy should be ignored in favor of the encryption block split.
    --  This would require that CipherHeader use same-length lists instead of singles
    --  Eg: nonces :: [Nonce], digests :: Maybe [Digest]
    --  Unique nonces are essential when using per-block encryption

delDatum :: Path -> Repo -> IO Repo
delDatum p r = do
    -- Remove blocks
    mapM_ deleteLocalBlock bhs
    -- Update manifest, return
    return $ removeFile p r
  where
    bhs = dataBlockHeaders $ readFile p r
    deleteLocalBlock = deleteBlock (dataPath r)

-- NOTE: This writes the datum to a handle, does not 'get a handle'
--  Returns the bytestring in case needed elsewhere
getHandle :: GetFlags -> Handle -> Path -> Repo -> IO ()
getHandle gf h p r = getDatum gf p r >>= B.hPut h

-- NOTE:This reads the datum from a handle, does not 'put into handle'
putHandle :: PutFlags -> Handle -> Path -> Repo -> IO Repo
putHandle pf h p r = B.hGetContents h >>= flip (putDatum pf p) r


getFile :: GetFlags -> FilePath -> Path -> Repo -> IO ()
getFile gf src dst r = withFile src ReadMode $ \h -> getHandle gf h dst r

putFile :: PutFlags -> FilePath -> Path -> Repo -> IO Repo
putFile pf src dst r = withFile src WriteMode $ \h -> putHandle pf h dst r



-- Path

-- These are all repo actions, and don't return anything
--  unlike the get/put/del file functions (get just returns the bytestring)

listPath :: Bool -> Path -> Repo -> IO [Path]
listPath rc dst r = if queryManifest (Mf.pathExists dst) r
    then return paths
    else Mf.pathNotExistErr
  where
    readDir = if rc then readDirectoryRecursive else readDirectory
    paths = if queryManifest (Mf.pathIsFile dst) r
      then [dst]
      else readDir dst r

-- getPath - overwrite, recurse, src, dst, repo
getPath :: GetFlags -> Bool -> Bool -> Path -> FilePath -> Repo -> IO Repo
getPath gf rp rc src dst r = do
    efexists <- doesFileExist dst'
    edexists <- doesDirectoryExist dst'
    case (isf, isd) of
      (True,_) -> do -- File
        when edexists $
          error $ "Directory already exists at file target: " ++ toFilePath src
        if wm == Overwrite || not efexists
          then do
            verbose v $ "Getting: " ++ toFilePath src
            bs <- getDatum gf src r
            debug v $ "Writing file " ++ dst'
            B.writeFile dst' bs
            return r
          else do
            error $ "File already exists; use -o to overwrite: " ++ dst'
            return r
      (_,True) -> do -- Directory
          when efexists $
            error $ "File already exists at directory target: " ++ dst'
          if rp && edexists
            then do
              verbose v $ "Replacing:" ++ dst'
              removeDirectoryRecursive dst'
              -- createDirectoryIfMissing True dst'
            else do
              verbose v $ "Getting: " ++ dst'
              -- createDirectoryIfMissing True dst'
          createDirectoryIfMissing True dst'
          foldM getChild r $ filter filterChild $ readDirectory src r
      _ -> error "Get error: Source path does not exist."
    -- doesFileExist dst >>= (flip when) $ error "File exists at target."
  where
    v = verbosity $ repoEnv r
    wm = gfWriteMode gf
    isf = queryManifest (Mf.pathIsFile src) r
    isd = queryManifest (Mf.pathIsDirectory src) r
    dst' = normalise $ dst </> takeFileName (toFilePath src)
    getChild r src' = getPath gf rp rc src' dst' r
    filterChild p = rc || Mf.pathIsFile p (repoManifest r)

-- putPath - overwrite files, replace dirs, recurse children, src, dst, repo
putPath :: PutFlags -> Bool -> Bool -> FilePath -> Path -> Repo -> IO Repo
putPath pf rp rc src dst r = do
    efexists <- doesFileExist src
    edexists <- doesDirectoryExist src
    case (efexists, edexists) of
      (True,_) -> do -- File
        debug v $ "Reading file " ++ src
        bs <- B.readFile src
        when (idexists dst') $
          error $ "Directory already exists at file target: " ++ toFilePath dst'
        if wm == Overwrite || not (ifexists dst')
          then do
            verbose v $ "Putting file: " ++ toFilePath dst'
            putDatum pf dst' bs r
          else do
            error $ "File already exists; use -o to overwrite: " ++ toFilePath dst'
            return r
      (_,True) -> do
        when (ifexists dst') $
          error $ "File already exists at directory target: " ++ toFilePath dst'
        r' <- if rp && idexists dst'
          then do
            verbose v $ "Replacing:" ++ toFilePath dst'
            delPath True dst' r
          else do
            verbose v $ "Putting dir: " ++ toFilePath dst'
            return $ createDirectory dst' r
        -- The filterM strips child directories if non-recursive
        getDirectory src >>= filterM filterChild >>= foldM putChild r
      _ -> error "Put error: Source path does not exist."
  where
    v = verbosity $ repoEnv r
    wm = pfWriteMode pf
    -- Directory dst
    dst' = fromFilePath . normalise $ (toFilePath dst) </> takeFileName src
    ifexists p = queryManifest (Mf.pathIsFile p) r
    idexists p = queryManifest (Mf.pathIsDirectory p) r
    putChild r src' = putPath pf rp rc src' dst' r
    -- Filter files for recursive
    filterChild p = if rc
      then return True
      else doesFileExist p

-- delPath - force, dst, repo
delPath :: Bool -> Path -> Repo -> IO Repo
delPath _ [] r = error "Cannot delete root!"
delPath force dst r = case (isf, isd) of
    (True, _) -> do -- File
      verbose v $ "Deleting: " ++ toFilePath dst
      delDatum dst r
    (_, True) -> do -- Dir
      when (haschild && not force) $ error "Cannot delete non-empty directory. Use -f to force deletion"
      -- Delete existing children if needed
      r' <- if haschild && force
        then foldM (flip $ delPath force) r children
        else return r
      -- Delete self
      verbose v $ "Deleting: " ++ toFilePath dst
      return $ modifyManifest (Mf.removeDirectory dst) r'
    _ -> error "Del error: Target path does not exist."
  where
    v = verbosity $ repoEnv r
    isf = queryManifest (Mf.pathIsFile dst) r
    isd = queryManifest (Mf.pathIsDirectory dst) r
    children = readDirectory dst r
    haschild = not . null $ children

-- pushPath - mode, src, dst, repo
pushPath :: SyncMode -> FilePath -> Path -> Repo -> IO Repo
pushPath sm = case sm of
  SynchronizeMode -> pushSync
  AdditiveMode -> pushAdditive
  DeadDropMode -> pushDeadDrop

pushSync src dst r = undefined
pushAdditive src dst r = undefined
pushDeadDrop src dst r = undefined

pullPath :: SyncMode -> Path -> FilePath -> Repo -> IO Repo
pullPath sm = case sm of
  SynchronizeMode -> pullSync
  AdditiveMode -> pullAdditive
  DeadDropMode -> pullDeadDrop

pullSync src dst r = undefined
pullAdditive src dst r = undefined
pullDeadDrop src dst r = undefined



-- IO helpers

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

-- Should be called only on directories, errors otherwise
-- Returns a relative path, not just name
-- Does *NOT* return "." or ".." like getDirectoryContents
-- TODO: Rename getDirectoryPaths
getDirectory :: FilePath -> IO [FilePath]
getDirectory p = map (p </>) . filter stripSpecial <$> getDirectoryContents p
  where stripSpecial a = a /= "." && a /= ".."

-- Ditto
getDirectoryRecursive :: FilePath -> IO [FilePath]
getDirectoryRecursive p = do
  dc <- getDirectory p
  dc's <- filterM doesDirectoryExist dc >>= mapM getDirectoryRecursive
  return $ dc ++ concat dc's

getPathModifyTime p = floor . utcTimeToPOSIXSeconds <$> getModificationTime p


-- Globbing
-- NOTE: Globbing is not recursive unless specified with **/

-- TODO: Convenience functions transform from src+options (maybe glob, recurse,
--  ignore) to a list of matching paths
-- Once we've translated into a list, we can just map and handle them one-by-one

filterExt :: Maybe Glob -> Maybe Ignore -> [FilePath] -> [FilePath]
filterExt mg mi = fg . fi
  where
    fi = case mi of
      Just i -> filter (not . doesIgnore i)
      Nothing -> id
    fg = case mg of
      Just _ -> fst . L.partition (G.match c)
      Nothing -> id
    c = G.simplify . G.compile . fromJust $ mg

filterInt :: Maybe Glob -> Maybe Ignore -> [Path] -> [Path]
filterInt g i = map fromFilePath . filterExt g i . map toFilePath

globDirectoryIO :: FilePath -> Glob -> IO ([FilePath],[FilePath])
globDirectoryIO p pat = L.partition (G.match c) <$> getDirectoryRecursive p
  where c = G.simplify $ G.compile pat

globDirectoryIO' p pat = fst <$> globDirectoryIO p pat
globDirectoryIO'' p pat = snd <$> globDirectoryIO p pat



-- Multiplexing
isMagicSlash r = magicSlash (repoEnv r)

-- Multiplexes on a trailing slash, external
multiplex :: (FilePath -> Repo -> IO Repo) -> FilePath -> Repo -> IO Repo
multiplex f p r = if isMagicSlash r && hasTrailingPathSeparator p
  then  getDirectory p >>= foldM (flip f) r
  else f p r

-- Multiplex on internal paths
multiplex' :: (Path -> Repo -> IO Repo) -> FilePath -> Repo -> IO Repo
multiplex' f p r = if isMagicSlash r && hasTrailingPathSeparator p
    then foldM (flip f) r $ readDirectory p' r
    else f p' r
  where p' = fromFilePath p
