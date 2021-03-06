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
import           Apotheca.Repo.Glob
import           Apotheca.Repo.Ignore
import           Apotheca.Repo.Internal
import qualified Apotheca.Repo.Manifest   as Mf
import           Apotheca.Repo.Path
import           Apotheca.Security.Auth
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

newRepo :: ConfigFlags -> Env -> Repo
newRepo cf e = defaultRepo
    { repoEnv = e
    , repoConfig = defaultConfig
      { defaultSplit = fromDefault defaultSplit $ cfSplit cf
      , largeSplitLimit = cfLarge cf
      , defaultHash = cfHashStrat cf
      , defaultCompression = fromDefault defaultCompression $ cfCompression cf
      , defaultCipher = cfCipher cf
      }-- TODO: Overwrite defaultConfig values with ConfigFlag values
    }
  where
    fromDefault :: (Config -> a) -> Maybe a -> a
    fromDefault f ma = fromMaybe (f defaultConfig) ma



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

openOrCreateRepo :: ConfigFlags -> Env -> IO Repo
openOrCreateRepo cf e = do
    exists <- doesRepoExist rp
    if exists
      then openRepo e
      else createRepo cf e
  where
    rp = repoDir e

-- This just sets up the repo directories - repo must still be persisted after.
createRepo :: ConfigFlags -> Env -> IO Repo
createRepo cf e = do
    -- TODO: Fix / warn on creating bare repo in a directory w/ existing contents
    errorWhen (doesRepoExist rp) "Cannot create repo: Repo already exists."
    --
    putStrLn $ "Creating repo directory."
    createDirectoryIfMissing True dp
    putStrLn $ "Creating block directories."
    mapM_ (createDirectoryIfMissing True . (dp </>)) blockDirs
    when bare $ B.writeFile (dp </> specialName) "BARE"
    when hasSecret $ writeAuth (dp </> "AUTH") $ generatePassthru (newHashStrategy SHA3) secret
    --
    execRM persistRepo $ newRepo cf e
  where
    bare = repoType e == BareRepo
    hasSecret = isJust $ masterSecret e
    Just secret = masterSecret e
    rp = repoDir e
    dp = dataDir e

authRepo :: Env -> IO Env
authRepo e = do
    auexists <- doesFileExist aup
    case (auexists, msecret) of
      (True, Just secret) -> do
        au <- readAuth aup
        unless (checkAuth au secret) $ error "Incorrect auth password!"
        return $ e { masterSecret = decodeAuth au secret }
      (True, Nothing) -> error "Auth password required!"
      (False, Just secret) -> do
        when (v < Warn) $ putStrLn "[!] Auth password not validated - successful encryption / decryption not guaranteed."
        return e
        -- when (v < Warn) $ putStrLn "[!] Auth password not required - ignoring supplied password."
        -- return $ e { masterSecret = Nothing }
      (False, Nothing) -> return e
  where
    v = verbosity e
    aup = dataDir e </> "AUTH"
    msecret = masterSecret e


-- NOTE: repoType will be overridden when opening a repo
openRepo :: Env -> IO Repo
openRepo e = do
    -- Ensure exists
    errorUnless (doesRepoExist rp) "Cannot open repo: Repo does not exist."
    -- Check / transform secret if needed
    e' <- authRepo e
    --
    errorUnless (checkRepoData dp) "Cannot load repo: Missing files!"
    --
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
    dp = dataDir e

persistRepo :: RIO ()
persistRepo = do
    dp <- dataPath
    verbose "Persisting repo..."
    getConfig >>= io . writeConfig (dp </> configName)
    getManifest >>= io . Mf.writeManifest (dp </> manifestName)
    queryRM repoIgnore >>= io . writeIgnore (dp </> ignoreName)
    io $ B.writeFile (dp </> distName) "DISTRIBUTED_PLACEHOLDER"

destroyRepo :: RIO ()
destroyRepo = do
  rp <- queryEnv repoDir
  dp <- dataPath
  exists <- io $ doesRepoExist rp
  verbose "Destroying repo..."
  errorIf (not exists) "Cannot destroy repo: Repo does not exist."
  io $ removeDirectoryRecursive dp



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

writeManifestAccess :: (Monad m) => Path -> AccessHeader -> RM m ()
writeManifestAccess p ac = modifyManifest (Mf.writeAccess p ac)

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

timeCompareHeader :: Int -> CompareHeader
timeCompareHeader t = defaultCompareHeader { chTime = t }

hashCompareHeader :: Maybe HashHeader -> CompareHeader
hashCompareHeader mh = defaultCompareHeader { chHashHeader = mh }

deepenCompareHeader :: Maybe HashStrategy -> ByteString -> CompareHeader -> CompareHeader
deepenCompareHeader mh bs ch = case mh of
  Just h -> ch { chHashHeader = Just $ hashHeaderWith h bs }
  Nothing -> ch

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

-- NOTE: Externally rolled-back files with properly-dated timestamps will fail to
--  update the repo since their timestamps will be lower
--  Update or Freshes.
compareTime :: Int -> Int -> Bool
compareTime a b = a <= b
-- True if sameness can be proven
compareHash :: Maybe HashHeader -> Maybe HashHeader -> Bool
compareHash ma mb = isJust ma && maybe False (fromJust ma ==) mb



-- Should-write convenience

shouldWrite :: WriteMode -> CompareHeader -> Maybe CompareHeader -> (Bool, Maybe String)
shouldWrite = shouldWriteWith compareTimes compareHashes

shouldWrite' :: WriteMode -> CompareHeader -> Maybe CompareHeader -> Bool
shouldWrite' wm ch mch = fst $ shouldWrite wm ch mch

-- shouldWriteWith :: CHCompare -> CHCompare -> WriteMode -> CompareHeader -> Maybe CompareHeader -> Bool
-- shouldWriteWith light deep wm a mb = case wm of
--     Add -> isNothing mb
--     Overwrite -> maybe True (not . deep a) mb
--     Update -> maybe (fwd Overwrite) (not . light a) mb
--     Freshen -> isJust mb && fwd Update
--   where
--     b = fromJust mb
--     fwd wm' = shouldWriteWith light deep wm' a mb

shouldWriteWith :: CHCompare -> CHCompare -> WriteMode -> CompareHeader -> Maybe CompareHeader -> (Bool, Maybe String)
shouldWriteWith light deep wm a mb = case wm of
      Add -> wrap $ isNothing mb
      Overwrite -> wrap $ maybe True (not . deep a) mb
      Update -> maybe (wrap True) (\b -> if light a b then wrap False else fwd Overwrite) mb
      Freshen -> maybe (wrap False) (const $ fwd Update) mb
    where
      b = fromJust mb
      fwd wm' = shouldWriteWith light deep wm' a mb
      wrap b = (b, if b then Nothing else Just $ shouldWriteIgnoreReason wm)

-- NOTE: The correct reason is not returned properly if the query forwards to another WriteMode
shouldWriteIgnoreReason :: WriteMode -> String
shouldWriteIgnoreReason wm = case wm of
  Add -> "File already exists; use -o to overwrite: "
  Overwrite -> "Matching hash found: "
  Update -> "Insufficient timestamp found; use -o to overwrite: "
  Freshen -> "File does not exist; use -u to update: "



-- TODO: shouldWriteWithRIO :: (RIO CHCompare) -> (RIO CHCompare) -> ... -> RIO Bool

-- Convenience function for just timestamps, to use before hashing
-- shouldWriteTime :: WriteMode -> Int -> Maybe Int -> Bool
-- shouldWriteTime wm ta mtb = shouldWrite' wm a mb
--   where
--     a = timeCompareHeader ta
--     mb = mtb >>= Just . timeCompareHeader

whenWritable :: (Bool, Maybe String) -> WriteMode -> String -> RIO () -> RIO ()
whenWritable (writable, mreason) wm dst act = if writable
  then act
  else verbose $ concat [ "Ignoring ", show wm, ": ", fromJust mreason, dst ]



-- Config convenience
-- TODO: (isLarge :: Bool, isRandomAccess :: Bool)

getBlockHash :: (Monad m) => RM m HashStrategy
getBlockHash = queryConfig blockHash

getDefaultSplitStrategy :: (Monad m) => RM m SplitStrategy
getDefaultSplitStrategy = queryConfig defaultSplit

getLargeSplitLimit :: (Monad m) => RM m (Maybe Int)
getLargeSplitLimit = queryConfig largeSplitLimit

getLargeSplitStrategy :: (Monad m) => RM m (Maybe SplitStrategy)
getLargeSplitStrategy = do
  mlimit <- queryConfig largeSplitLimit
  return (ConstSplit <$> mlimit)

isLarge :: (Monad m) => ByteString -> RM m Bool
isLarge bs = do
    mlimit <- getLargeSplitLimit
    return $ maybe False (\limit -> len > limit) mlimit
  where len = B.length bs

getSplitStrategy :: (Monad m) => ByteString -> RM m SplitStrategy
getSplitStrategy bs = do
  large <- isLarge bs
  if large
    then fromJust <$> getLargeSplitStrategy
    else getDefaultSplitStrategy

getCompression :: (Monad m) => RM m GzipCompression
getCompression = queryConfig defaultCompression

getDefaultHash :: (Monad m) => RM m (Maybe HashStrategy)
getDefaultHash = queryConfig defaultHash

getDefaultCipher :: (Monad m) => RM m (Maybe CipherStrategy)
getDefaultCipher = queryConfig defaultCipher



-- Perform splitting

splitBlocks :: (Monad m) => TransformFlags -> ByteString -> RM m [Block]
splitBlocks tf bs = do
  s <- inheritRM (getSplitStrategy bs) $ tfSplitStrat tf
  return $ splitWith s bs

assignBlockHeaders :: (Monad m) => BlockType -> [Block] -> RM m [(BlockHeader, Block)]
assignBlockHeaders bt bs = do
  bh <- queryConfig blockHash
  return $ map (assignBlockHeader bh bt) bs



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
    b' <- case cprs of
      NoCompression -> return b
      _ -> do
        verbose $ "Compressing with: " ++ show cprs
        return $ compress cprs b
    -- encrypt
    (b'', ch)  <- if isJust $ tfCipherStrat tf
      then do
        let cs = fromJust $ tfCipherStrat tf
        nonce <- io $ getStratNonceIO cs -- NOTE: No need for makeNonce here
        secret <- getSecret -- Tsk tsk
        verbose $ "Encrypting with: " ++ show (calgorithm cs)
        let Right (ch, ct) = encipherHeaderWith cs nonce secret b'
        return (ct, Just ch)
      else return (b', Nothing)
    pairs <- splitBlocks tf b'' >>= assignBlockHeaders LocalBlock
    return ( FileHeader
        { fhSize = B.length b
        , fhCompression = cprs
        , fhHashHeader = Nothing  -- NOTE: assigned in writeDatum after transforming
        , fhCipherHeader = ch
        , fhBlockHeaders = map fst pairs
        }
      , pairs
      )
  where
    cprs = tfCompression tf

untransformDatum :: FileHeader -> ByteString -> RIO ByteString
untransformDatum fh b = do
    secret <- getSecret
    verbose "Decrypting..."
    let pt = case fhCipherHeader fh of
          Just ch -> fromRight $ decipherHeaderWith ch secret b
          Nothing -> b
    case fhCompression fh of
      NoCompression -> return pt
      cprs -> do
        verbose "Decompressing..."
        return $ decompress cprs pt
  where fromRight (Right r) = r



-- Read/write
-- NOTE: These are all RIO instead of RM m because the blockstore isn't ready

-- Datum (in-repo)

datumCompareHeader :: Path -> RIO CompareHeader
datumCompareHeader p = do
  ac <- readManifestAccess p
  fh <- readManifestFile p
  return $ compareHeader (modifyTime ac) (fhHashHeader fh)

readDatum :: Path -> RIO ByteString
readDatum p = do
    verbose $ "Reading datum: " ++ toFilePath p
    fh <- readManifestFile p
    verbose $ "Fetching blocks..."
    bks <- mapM fetchLocalBlock $ fhBlockHeaders fh
    untransformDatum fh $ B.concat bks
  where
    fetchLocalBlock :: BlockHeader -> RIO Block
    fetchLocalBlock bh = do
      dp <- dataPath
      debug $ "Fetching block: " ++ blockPath bh
      io $ fromJust <$> fetchBlock dp bh

writeDatum :: WriteFlags -> Path -> ByteString -> RIO ()
writeDatum wf p b = do
    verbose $ "Writing datum: " ++ toFilePath p
    -- Remove previous version if exists
    exists <- queryManifest $ Mf.pathIsFile p
    when exists $ removeDatum p
    -- Transform datum
    (fh, pairs) <- transformDatum tf b
    -- Write blocks to disk
    verbose $ "Storing blocks..."
    mapM_ storeLocalBlock pairs
    -- Apply hash, update manifest, return
    createManifestFile p $ fh { fhHashHeader = mh }
    modifyManifest . Mf.writeAccess p . Mf.timeAccess $ afModifyTime af
  where
    af = wfAccessFlags wf
    tf = wfTransformFlags wf
    mh = flip hashHeaderWith b <$> afHashStrat af
    storeLocalBlock :: (BlockHeader, Block) -> RIO ()
    storeLocalBlock (bh, b) = do
      dp <- dataPath
      debug $ "Storing block: " ++ blockPath bh
      io $ storeBlock dp bh b

removeDatum :: Path -> RIO ()
removeDatum p = do
    verbose $ "Deleting datum: " ++ toFilePath p
    -- Remove blocks
    bhs <- fhBlockHeaders <$> readManifestFile p
    mapM_ deleteLocalBlock bhs
    -- Update manifest, return
    removeManifestFile p
  where
    deleteLocalBlock ::BlockHeader -> RIO ()
    deleteLocalBlock bh = do
      dp <- dataPath
      debug $ "Deleting block: " ++ blockPath bh
      io $ deleteBlock dp bh

-- Handle - IO

handleCompareHeader :: Handle -> RIO CompareHeader
handleCompareHeader _ = do
  t <- io Mf.getTime
  return $ compareHeader t Nothing

readHandle :: Handle -> RIO ByteString
readHandle = io . B.hGetContents

writeHandle :: Handle -> ByteString -> RIO ()
writeHandle h bs = io $ B.hPut h bs

removeHandle :: Handle -> RIO ()
removeHandle _ = return ()

-- File - IO

extFileCompareHeader :: FilePath -> RIO CompareHeader
extFileCompareHeader p = do
  t <- Mf.convertUTC <$> io (getModificationTime p)
  return $ compareHeader t Nothing

-- NOTE: We use evalRM here because we know this doesn't change the repo's state
readExtFile :: FilePath -> RIO ByteString
readExtFile src = do
  r <- getRM
  verbose $ "Reading extfile: " ++ src
  io . withFile src ReadMode $ \h -> evalRM (readHandle h) r

-- NOTE: We use evalRM here because we know this doesn't change the repo's state
writeExtFile :: WriteFlags -> FilePath -> ByteString -> RIO ()
writeExtFile _ dst bs = do
  r <- getRM
  verbose $ "Writing extfile: " ++ dst
  io . withFile dst WriteMode $ \h -> evalRM (writeHandle h bs) r
  -- NOTE: Cannot set file time

removeExtFile :: FilePath -> RIO ()
removeExtFile p = do
  verbose $ "Deleting extfile: " ++ p
  io $ removeFile p



-- Multiplexing

-- Multiplexes on a trailing slash, external
multiplexExt :: (FilePath -> RIO a) -> FilePath -> RIO [a]
multiplexExt f p = do
  isMagic <- isMagicSlash
  if isMagic && hasTrailingPathSeparator p
    then do
      verbose $ "Multiplexing on magic slash: " ++ p
      io (getDirectory p) >>= mapM f
    else sequence [f p]

-- Multiplexes on a trailing slash, internal
multiplexInt :: (Path -> RIO a) -> FilePath -> RIO [a]
multiplexInt f p = do
    isMagic <- isMagicSlash
    if isMagic && hasTrailingPathSeparator p
      then do
        verbose $ "Multiplexing on magic slash: " ++ p
        readManifestDirectory p' >>= mapM f
      else sequence [f p']
  where p' = fromFilePath p



-- Single master secret

getSecret :: (Monad m) => RM m ByteString
getSecret = fromMaybe B.empty <$> queryEnv masterSecret



-- Map-like
-- NOTE: Paths must already be validated or an error may occur
--  They should be identical in function, and prune is clearer (and matches 'p')

findPath :: FilterFlags -> Path -> RIO [Path]
findPath ff dst = do
  exists <- queryManifest (Mf.pathExists dst)
  unless exists Mf.pathNotExistErr
  t <- io Mf.getTime
  listPath True dst >>= filterM (filterOnFlags t ff)

filterOnFlags :: Int -> FilterFlags -> Path -> RIO Bool
filterOnFlags t ff p = fld filters p
  where
    filters :: [Path -> RIO Bool]
    -- NOTE: Order for efficiency
    filters = catMaybes $
      [ filterType <$> ffType ff
      , filterGlob <$> ffGlob ff
      , filterAge t <$> ffAge ff
      , filterSize <$> ffSize ff
      ]
    fld :: [Path -> RIO Bool] -> Path -> RIO Bool
    fld [] _ = return True
    fld (x:xs) p = do
      is <- x p
      if is then fld xs p else return False

-- ordFilterRM :: (Ordering, a) -> Path -> RIO Bool
ordFilterRM :: (Monad m, Ord a) => (t -> RM m a) -> (Ordering, a) -> t -> RM m Bool
ordFilterRM f (o, a) p = (o ==) . flip compare a <$> f p

filterGlob :: Glob -> Path -> RIO Bool
filterGlob g p = return $ gmatch (gcompile g) $ toGlobPath p

-- NOTE: T is time-as-in-now, a is relative age
-- NOTE: Directories do not get their time updated yet when their contents are modified
filterAge :: (Monad m) => Int -> (Ordering, Int) -> Path -> RM m Bool
filterAge t (o, a) = ordFilterRM ((modifyTime <$>) . readManifestAccess) (o, t - a)

-- NOTE: Filtering on size will not return directories
filterSize :: (Monad m) => (Ordering, Int) -> Path -> RM m Bool
filterSize (o, a) p = do
  isFile <- queryManifest $ Mf.pathIsFile p
  if isFile
    then ordFilterRM ((fhSize <$>) . readManifestFile) (o, a) p
    else return False

filterType :: (Monad m) => EntryType -> Path -> RM m Bool
filterType t p = case t of
  DirType -> queryManifest $ Mf.pathIsDirectory p
  FileType -> queryManifest $ Mf.pathIsFile p


listPath :: Bool -> Path -> RIO [Path]
listPath rc dst = do
    exists <- queryManifest (Mf.pathExists dst)
    unless exists Mf.pathNotExistErr
    isFile <- queryManifest (Mf.pathIsFile dst)
    if isFile
      then return [dst]
      else readDir dst
  where
    readDir = if rc
      then readManifestDirectoryRecursive
      else readManifestDirectory

-- Compare light headers, maybe cancel >  ingest stream > compare deep headers, maybe cancel > transform > do the things
-- putDatum :: (Monad m) => (a -> RM m ByteString) -> (b -> ByteString -> RM m ()) -> PutFlags -> a -> b -> RM m ()

-- NOTE: In writeDatum, when pathIsFile we use existing hash to check equality
--  Remember, only the existing hash is used for comparison headers. The passed-in
--  hash strat is for the plaintext checksum, not for comparison - not until next write.
--  This means that each writeFoo should take care of WriteMode/Flags + pre-existing entries.

-- getPath - overwrite, prune recurse, src, dst, repo
getPath :: GetFlags -> Bool -> Bool -> Path -> FilePath -> RIO ()
getPath gf pr rc src dst = do
    efexists <- io $ doesFileExist dst'
    edexists <- io $ doesDirectoryExist dst'
    isf <- queryManifest (Mf.pathIsFile src)
    isd <- queryManifest (Mf.pathIsDirectory src)
    case (isf, isd) of
      (True,_) -> do -- File
        when edexists $
          error $ "Directory already exists at file target: " ++ dst'
        -- Source compare header
        chsrc <- datumCompareHeader src
        -- Maybe destination compare header
        mchdst <- if efexists
          then Just <$> extFileCompareHeader dst'
          else return Nothing
        -- Light check, pre-read termination if possible
        whenWritable (shouldWrite wm chsrc mchdst) wm dst' $ do
          terse $ "Getting: " ++ toFilePath src
          -- Read
          bs <- readDatum src
          -- NOTE: No deep check on extfiles for now
          -- write
          wf <- inheritGet (chTime chsrc) gf
          writeExtFile wf dst' bs
      (_,True) -> do -- Directory
          when efexists $
            error $ "File already exists at directory target: " ++ dst'
          terse $ "Getting: " ++ toFilePath src
          -- NOTE: Magic slash takes precedence over pruning - when multiplexed
          --  immediate children of the destination will not be pruned. This is
          --  the logically correct behavior.
          --  This is most visible with `apo get -orp / .` vs `apo --no-magic-slash get -orp / .`
          children <- readManifestDirectory src >>= filterM filterChild
          if edexists
            then when pr $ do
              verbose $ "Pruning: " ++ dst'
              children' <- io $ getDirectory dst'
              -- NOTE: Dropping init from children - See Manifest.readDirectory
              let cnames = map last children -- works because Path is a [String]
              flip mapM_ children' $ \c -> when (notElem c cnames) $ do
                debug $ "Pruning child: " ++ dst' </> c
                io $ removePathRecursively $ dst' </> c
            else io $ createDirectoryIfMissing True dst'
          mapM_ getChild children
      _ -> error "Get error: Source path does not exist."
  where
    wm = gfWriteMode gf
    dst' = normalise $ dst </> takeFileName (toFilePath src)
    getChild src' = getPath gf pr rc src' dst'
    filterChild p = (rc ||) <$> queryManifest (Mf.pathIsFile p) -- if rc then true else doesFileExist

-- putPath - overwrite files, prune dirs, recurse children, src, dst, repo
putPath :: PutFlags -> Bool -> Bool -> FilePath -> Path -> RIO ()
putPath pf pr rc src dst = do
    efexists <- io $ doesFileExist src
    edexists <- io $ doesDirectoryExist src
    ifexists <- queryManifest (Mf.pathIsFile dst')
    idexists <- queryManifest (Mf.pathIsDirectory dst')
    case (efexists, edexists) of
      (True,_) -> do -- File
        when idexists $
          error $ "Directory already exists at file target: " ++ toFilePath dst'
        -- Source compare header
        chsrc <- extFileCompareHeader src
        -- Maybe destination compare header
        mchdst <- if ifexists
          then Just <$> datumCompareHeader dst'
          else return Nothing
        -- Light check, pre-read termination if possible
        whenWritable (shouldWrite wm chsrc mchdst) wm (toFilePath dst') $ do
          verbose $ "Checking: " ++ src
          -- Read
          bs <- readExtFile src
          -- Compare source with dest hash if it exists
          let
            mhdst :: Maybe HashStrategy
            mhdst = do
              chdst <- mchdst
              hh <- chHashHeader chdst
              return $ hashStrategy hh
            chdeep = deepenCompareHeader mhdst bs chsrc
          -- Deep check, pre-write termination if possible
          -- NOTE: Redundant when chdeep == chsrc
          whenWritable (shouldWrite wm chdeep mchdst) wm (toFilePath dst') $ do
              -- Write
              terse $ "Putting: " ++ src
              wf <- inheritPut (chTime chsrc) pf
              writeDatum wf dst' bs
      (_,True) -> do
        when ifexists $
          error $ "File already exists at directory target: " ++ toFilePath dst'
        verbose $ "Checking: " ++ src
        children <- io (getDirectory src) >>= filterM filterChild
        if idexists
          then when pr $ do
            -- NOTE: See getPath pruning notes
            verbose $ "Pruning: " ++ toFilePath dst'
            -- NOTE: Dropping init from children - See Manifest.readDirectory
            children' <- map last <$> readManifestDirectory dst'
            flip mapM_ children' $ \c -> when (notElem c children) $ do
              debug $ "Pruning child: " ++ toFilePath (dst' ++ [c])
              removeManifestPath True (dst' ++ [c])
          else do
            debug $ "Creating dir: " ++ toFilePath dst'
            createManifestDirectory dst'
        terse $ "Putting: " ++ src
        mapM_ putChild children
      _ -> error "Put error: Source path does not exist."
  where
    wm = pfWriteMode pf
    hs = pfHashStrat pf
    -- Directory dst
    dst' = fromFilePath . normalise $ (toFilePath dst) </> takeFileName src
    putChild src' = putPath pf pr rc src' dst'
    -- Filter files for recursive
    filterChild p = (rc ||) <$> io (doesFileExist p) -- if rc then true else doesFileExist

delPath :: Bool -> Path -> RIO ()
delPath _ [] = error "Cannot delete root!"
delPath force dst = do
  isf <- queryManifest (Mf.pathIsFile dst)
  isd <- queryManifest (Mf.pathIsDirectory dst)
  terse $ "Deleting: " ++ toFilePath dst
  case (isf, isd) of
    (True, _) -> do -- File
      removeDatum dst
    (_, True) -> do -- Dir
      children <- readManifestDirectory dst
      let haschild = not $ null children
      when haschild $ if force
        then mapM_ (delPath force) children
        else error "Cannot delete non-empty directory. Use -f to force deletion"
      -- Delete self
      removeManifestDirectory dst
    _ -> error "Del error: Target path does not exist."

-- Handle convenience

getHandle :: GetFlags -> Handle -> Path -> RIO ()
getHandle _ h p = readDatum p >>= writeHandle h

-- NOTE:This reads the datum from a handle, does not 'put into handle'
putHandle :: PutFlags -> Handle -> Path -> RIO ()
putHandle pf h p = do
  t <- io $ Mf.getTime
  wf <- inheritPut t pf
  readHandle h >>= writeDatum wf p



-- Inherit args

inheritRM :: (Monad m) => (RM m a) -> Inherited a -> RM m a
inheritRM _ (Explicit b) = return b
inheritRM f Inherit = f

inheritPut :: (Monad m) => Int -> PutFlags -> RM m WriteFlags
inheritPut t pf = do
  cmprs <- inheritRM getCompression $ pfCompression pf
  hs <- inheritRM getDefaultHash $ pfHashStrat pf
  cs <- inheritRM getDefaultCipher $ pfCipherStrat pf
  return $ WriteFlags
    { wfWriteMode = pfWriteMode pf
    , wfAccessFlags = AccessFlags
      { afModifyTime = t
      , afHashStrat = hs
      }
    , wfTransformFlags = TransformFlags
      { tfSplitStrat = pfSplitStrat pf
      , tfCompression = cmprs
      , tfCipherStrat = cs
      }
    }

inheritGet :: (Monad m) => Int ->  GetFlags -> RM m WriteFlags
inheritGet t gf = return $ WriteFlags
  { wfWriteMode = gfWriteMode gf
  , wfAccessFlags = AccessFlags
    { afModifyTime = t
    , afHashStrat = undefined
    }
  , wfTransformFlags = undefined
  }
