{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Either
import           Data.Maybe

import           System.Directory
import           System.FilePath
import           System.IO                (Handle, IOMode (..), withFile)

import           Apotheca.Encodable       (GzipCompression (..), compress,
                                           decompress)
import           Apotheca.Logs            (Verbosity (..), logM)
import           Apotheca.Repo.Blocks
import           Apotheca.Repo.Config
import           Apotheca.Repo.Env
import           Apotheca.Repo.Ignore
import           Apotheca.Repo.Internal
import qualified Apotheca.Repo.Manifest   as Mf
import           Apotheca.Repo.Path
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash



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
queryRM :: (Monad m) => (Repo -> a) -> RM m a
queryRM = gets



-- io lifter

io :: IO a -> RIO a
io = liftIO

unio :: RIO a -> RIO (IO a)
unio rio = evalRM rio <$> getRM



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
getEnv = queryRM repoEnv

getConfig :: (Monad m) => RM m Config
getConfig = queryRM repoConfig

getManifest :: (Monad m) => RM m Manifest
getManifest = queryRM repoManifest

getIgnore :: (Monad m) => RM m Ignore
getIgnore = queryRM repoIgnore



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

isMagicSlash :: (Monad m) => RM m Bool
isMagicSlash = queryEnv magicSlash

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
--  $/IGNORE -- Ignore file
--  ./.apo -- A hidden FILE signifying a bare repo (otherwise is dir)

openOrCreateRepo :: Env -> IO Repo
openOrCreateRepo e = do
    exists <- doesRepoExist rp
    if exists
      then openRepo e
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
    queryRM repoIgnore >>= io . writeIgnore (dp </> ignoreName)
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



-- Manifest convenience
-- NOTE: create / write functions cause errors if a file / dir exists inappropriately

-- Directory management

-- NOTE: Creates entire dir path
createManifestDirectory :: (Monad m) => Path -> RM m ()
createManifestDirectory p = modifyManifest (Mf.createDirectoryIfMissing True p)

readManifestDirectory :: (Monad m) => Path -> RM m [Path]
readManifestDirectory = queryManifest . Mf.readDirectory
readManifestDirectoryRecursive :: (Monad m) => Path -> RM m [Path]
readManifestDirectoryRecursive = queryManifest . Mf.readDirectoryRecursive

removeManifestDirectory :: (Monad m) => Path -> RM m ()
removeManifestDirectory = modifyManifest . Mf.removeDirectoryRecursive

-- File management

-- NOTE: Always creates ancestor directories
createManifestFile :: (Monad m) => Path -> FileHeader -> RM m ()
createManifestFile p fh = do
  createManifestDirectory (parent p)
  modifyManifest (Mf.createFile p fh)

readManifestFile :: (Monad m) => Path -> RM m FileHeader
readManifestFile = queryManifest . Mf.readFile

writeManifestFile :: (Monad m) => Path -> FileHeader -> RM m ()
writeManifestFile p fh = modifyManifest (Mf.writeFile p fh)

removeManifestFile :: (Monad m) => Path -> RM m ()
removeManifestFile = modifyManifest . Mf.removeFile

-- Generic management

removeManifestPath :: (Monad m) => Bool -> Path -> RM m ()
removeManifestPath force = modifyManifest . f
  where f = if force then Mf.removePathForcibly else Mf.removePath

readManifestAccess :: (Monad m) => Path -> RM m AccessHeader
readManifestAccess = queryManifest . Mf.readAccess



-- Compare headers

type CHCompare = CompareHeader -> CompareHeader -> Bool

data CompareHeader = CompareHeader
  { chTime       :: Int
  , chHashHeader :: Maybe HashHeader
  } deriving (Show, Read, Eq)

defaultCompareHeader = CompareHeader
  { chTime = 0
  , chHashHeader = Nothing
  }

compareTimeHeader :: Int -> CompareHeader
compareTimeHeader t = defaultCompareHeader { chTime = t }

compareHashHeader :: Maybe HashHeader -> CompareHeader
compareHashHeader mh = defaultCompareHeader { chHashHeader = mh }

compareHeader :: Int -> Maybe HashHeader -> CompareHeader
compareHeader t mh = defaultCompareHeader
  { chTime = t
  , chHashHeader = mh
  }

-- True if sameness can be proven, false otherwise
compareHeaders :: CompareHeader -> CompareHeader -> Bool
compareHeaders a b = compareTimes a b && compareHashes a b

compareHeadersWith :: (CompareHeader -> a) -> (a -> a -> Bool)
  -> CompareHeader -> CompareHeader -> Bool
compareHeadersWith s f a b = f (s a) (s b)

compareTimes :: CompareHeader -> CompareHeader -> Bool
compareTimes = compareHeadersWith chTime compareTime
compareHashes :: CompareHeader -> CompareHeader -> Bool
compareHashes = compareHeadersWith chHashHeader compareHash

-- Returns true if timestamp is the same
-- NOTE: This is NOT a > b - ie, 'recent'. If we did do it that way, rolled-back
--  files with properly-dated timestamps would fail to update in the repo with via
--  Update or Freshes.
compareTime :: Int -> Int -> Bool
compareTime a b = a /= b
-- True if sameness can be proven
compareHash :: Maybe HashHeader -> Maybe HashHeader -> Bool
compareHash ma mb = isJust ma && maybe False (fromJust ma ==) mb



-- Should-write convenience

shouldWrite :: WriteMode -> CompareHeader -> Maybe CompareHeader -> Bool
shouldWrite = shouldWriteWith compareTimes compareHashes

shouldWriteWith :: CHCompare -> CHCompare -> WriteMode -> CompareHeader -> Maybe CompareHeader -> Bool
shouldWriteWith light heavy wm a mb = case wm of
    Add -> isNothing mb
    Overwrite -> maybe True (not . heavy a) mb
    Update -> maybe (fwd Overwrite) (not . light a) mb
    Freshen -> isJust mb && fwd Update
  where
    b = fromJust mb
    fwd wm' = shouldWriteWith light heavy wm' a mb

-- TODO: shouldWriteWithRIO :: (RIO CHCompare) -> (RIO CHCompare) -> ... -> RIO Bool

-- Convenience function for just timestamps, to use before hashing
shouldWriteTime :: WriteMode -> Int -> Maybe Int -> Bool
shouldWriteTime wm ta mtb = shouldWrite wm a mb
  where
    a = compareTimeHeader ta
    mb = mtb >>= Just . compareTimeHeader



-- Transform datum

-- TODO: pre- vs post-encryption splitting
--  Encryption /before/ splitting == whole-file encryption
--  Encryption /after/ splitting == per-block encryption
--  This means that if per-block encryption is specified, the block split
--  strategy should be ignored in favor of the encryption block split.
--  This would require that CipherHeader use same-length lists instead of singles
--  Eg: nonces :: [Nonce], digests :: Maybe [Digest]
--  Unique nonces are essential when using per-block encryption
transformDatum :: TransformFlags -> ByteString -> RIO (FileHeader, [(BlockHeader,Block)])
transformDatum tf b = do
    -- compress
    let b' = compress cprs b
    -- encrypt
    (b'', ch)  <- if isJust $ tfCipherStrat tf
      then do
        let cs = fromJust $ tfCipherStrat tf
        nonce <- io $ getStratNonceIO cs
        secret <- getSecret -- Tsk tsk
        let Right (ch, ct) = encipherHeaderWith cs nonce secret b'
        return (ct, Just ch)
      else return (b', Nothing)
    pairs <- splitBlocks b'' >>= assignBlockHeaders LocalBlock
    return ( FileHeader
        { fhSize = B.length b
        , fhCompression = cprs
        , fhHashHeader = Nothing
        , fhCipherHeader = ch
        , fhBlockHeaders = map fst pairs
        }
      , pairs
      )
  where
    cprs = fromMaybe NoCompression $ tfCompression tf

untransformDatum :: FileHeader -> ByteString -> RIO ByteString
untransformDatum fh b = do
    secret <- getSecret
    let pt = case fhCipherHeader fh of
          Just ch -> fromRight $ decipherHeaderWith ch secret b
          Nothing -> b
    return $ decompress (fhCompression fh) pt
  where fromRight (Right r) = r



-- Map-like
-- NOTE: Paths must already be validated or an error may occur

-- Read/write
-- NOTE: These are all RIO instead of RM m because the blockstore isn't ready

-- Datum (in-repo)

accessDatum :: Path -> RIO AccessHeader
accessDatum = readManifestAccess

readDatum :: Path -> RIO ByteString
readDatum p = do
    fh <- readManifestFile p
    bks <- mapM fetchLocalBlock $ fhBlockHeaders fh
    untransformDatum fh $ B.concat bks
  where
    fetchLocalBlock :: BlockHeader -> RIO Block
    fetchLocalBlock bh = do
      dp <- dataPath
      io $ fromJust <$> fetchBlock dp bh

writeDatum :: WriteFlags -> Path -> ByteString -> RIO ()
writeDatum wf p b = do
    -- Remove previous version if exists
    exists <- queryManifest $ Mf.pathIsFile p
    when exists $ removeDatum p
    -- Transform datum
    (fh, pairs) <- transformDatum (wfTransformFlags wf) b
    -- Write blocks to disk
    mapM_ storeLocalBlock pairs
    -- Update manifest, return
    createManifestFile p fh
  where
    storeLocalBlock :: (BlockHeader, Block) -> RIO ()
    storeLocalBlock (bh, b) = do
      dp <- dataPath
      io $ storeBlock dp bh b

removeDatum :: Path -> RIO ()
removeDatum p = do
    -- Remove blocks
    bhs <- fhBlockHeaders <$> readManifestFile p
    mapM_ deleteLocalBlock bhs
    -- Update manifest, return
    removeManifestFile p
  where
    deleteLocalBlock ::BlockHeader -> RIO ()
    deleteLocalBlock bh = do
      dp <- dataPath
      io $ deleteBlock dp bh

-- Handle - IO

accessHandle :: Handle -> RIO AccessHeader
accessHandle _ = io $ Mf.accessTime <$> Mf.getTime

readHandle :: Handle -> RIO ByteString
readHandle = io . B.hGetContents

writeHandle :: Handle -> ByteString -> RIO ()
writeHandle h bs = io $ B.hPut h bs

removeHandle :: Handle -> RIO ()
removeHandle _ = return ()

-- File - IO

accessExtFile :: FilePath -> RIO AccessHeader
accessExtFile p = io $ Mf.accessTime . Mf.convertUTC <$> getModificationTime p

readExtFile :: FilePath -> RIO ByteString
readExtFile src = do
  r <- getRM
  io . withFile src ReadMode $ \h -> evalRM (readHandle h) r

writeExtFile :: FilePath -> ByteString -> RIO ()
writeExtFile dst bs = do
  r <- getRM
  io . withFile dst WriteMode $ \h -> evalRM (writeHandle h bs) r

removeExtFile :: FilePath -> RIO ()
removeExtFile p = io $ removeFile p


-- Multiplexing

-- Multiplexes on a trailing slash, external
multiplexExt :: (FilePath -> RIO a) -> FilePath -> RIO [a]
multiplexExt f p = do
  isMagic <- isMagicSlash
  if isMagic && hasTrailingPathSeparator p
    then io (getDirectory p) >>= mapM f
    else sequence [f p]

-- Multiplexes on a trailing slash, internal
multiplexInt :: (Monad m) => (Path -> RM m a) -> FilePath -> RM m [a]
multiplexInt f p = do
    isMagic <- isMagicSlash
    if isMagic && hasTrailingPathSeparator p
      then readManifestDirectory p' >>= mapM f
      else sequence [f p']
  where p' = fromFilePath p


-- Temporary

getSecret :: (Monad m) => RM m ByteString
getSecret = return "cheese"
