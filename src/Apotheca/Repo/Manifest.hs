{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Manifest
( Manifest
, emptyManifest
, newManifest
, newManifestIO
-- Time
, getManifestTime
, setManifestTime
, updateManifestTime
-- File IO
, readManifestFile
, writeManifestFile
-- Path-based
, pathExists
, pathIsDirectory
, pathIsFile
-- , createPath
, movePath
, removePath
, removePathForcibly
, createDirectory
, createDirectoryIfMissing
, readDirectory
, readDirectoryRecursive
, removeDirectory
, removeDirectoryRecursive
, createFile
, readFile
, writeFile
, removeFile
-- Experimental
, globDir
-- TODO:
-- , reindex :: Path -> HashStrategy -> Manifest -> Manifest
--  reindexes  file or dir children recursively using the new hashstrat
-- Convenience export - move elsewhere
, pathNotExistErr
, pathAlreadyExistsErr
, dirNotExistErr
, fileNotExistErr
) where



import           GHC.Generics

import           Prelude                hiding (delete, insert, lookup,
                                         readFile, writeFile)
import qualified Prelude                as P

import qualified Data.Aeson             as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import qualified Data.List              as L
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Time.Clock        (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX  (utcTimeToPOSIXSeconds)

import qualified System.FilePath.Glob   as G

import qualified Crypto.Random          as R

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Misc
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Path
import           Apotheca.Security.Hash (sha2, unsalted)
import           Data.Aeson             (Value (..), object, withArray, (.:),
                                         (.=))



-- So this class has been through a bunch of different concepts, and has some
--  funky baggage belied by its exported simplicity.
--  The 'entryId' stuff is somewhat overkill, given that it doens't even expose
--  Entry or ContentHeader either. I mean, why not just do a tree (or treemap)
--  and be done with it?
--  The answer is (nitpicking about how-to-choose-entryid aside) future
--  flexibility, because of:
--  1) merging manifests
--  2) potential data deduplication
--  3) symlinking (sorta needed for mounting proper drives)
--  4) `data Manifest` will probably become `class Manifest p d f` with
--    some default implementations
--  5) Sub-manifests or obfuscated directories trees
--  Basically things people might want to configure

-- TODO: [Optional] Manifest and file versioning
--  Files could be a list of blockid's for initial (or current) commit + deltas
--  You should be able to walk down a file's history: although two forked
--  manifests may forked files, somewhere down the history they have a common origin.
--  Keeping the history alive for one keeps those historical blocks alive for
--  others in decay-mode. Pruning should be possible and historical blocks may
--  disappear, but the manifests still have a record of ids and checksums.
--  Versioning means that data loss may still be partially recoverable.
--  Versioning but no pruning (or deleting at all) makes this an additive repo,
--  (much more git-like) and allowing us to use special distributed tactics.
--  See CAP theorem and ACID.


-- Directory Tree lib - https://hackage.haskell.org/package/directory-tree-0.12.1/docs/System-Directory-Tree.html

emptyManifest = newManifest 0

newManifest :: Int -> Manifest
newManifest t =  Manifest
    { topId = tid
    , entries = M.singleton tid top
    -- , blocks = [] -- No owned-blocks cache for now, can iter over entries
    , manifestTime = t
    , ctr = tid + 1
    }
  where
    tid = 0
    eh = accessTime t
    top = udirectory eh [] tid tid

newManifestIO :: IO Manifest
newManifestIO = newManifest <$> getManifestTime

getManifestTime :: IO Int
getManifestTime = floor . utcTimeToPOSIXSeconds <$> getCurrentTime

setManifestTime :: Int -> Manifest -> Manifest
setManifestTime t m = m { manifestTime = t }

updateManifestTime :: Manifest -> IO Manifest
updateManifestTime m = do
  t <- getManifestTime
  return $ setManifestTime t m

accessNow :: Manifest -> AccessHeader
accessNow m = accessTime (manifestTime m)

accessTime :: Int -> AccessHeader
accessTime t = AccessHeader { modifyTime = t }


-- Encoding

defaultManifestEncoding = YAMLFormat @> NoCompression

readManifestFile :: FilePath -> IO Manifest
readManifestFile p = do
  mcfg <- decodeWithFile p defaultManifestEncoding
  case mcfg of
    Just cfg -> return cfg
    Nothing -> error $ "Could not parse config."

writeManifestFile :: FilePath -> Manifest -> IO ()
writeManifestFile p = encodeWithFile p defaultManifestEncoding


-- Lifting functions

-- Lift query
liftQ :: (EntryMap -> a) -> Manifest -> a
liftQ f = f . entries

-- Lift update
liftU :: (EntryMap -> EntryMap) -> Manifest -> Manifest
liftU f m = m { entries = f $ entries m }

-- Lift mutating query (query-update pair)
liftMQ :: (EntryMap -> (a, EntryMap)) -> Manifest -> (a, Manifest)
liftMQ f m = (a, m { entries = em })
  where (a, em) = f $ entries m



-- Lifted raw map funcs

--  These functions may corrupt the manifest if used incorrectly. They only
--  handle the map-like aspect, and do not assign parents, etc.
--  They are internal functions, and are not to be exported.
--  They don't handle errors yet so don't misuse them.

-- NOTE: It is an error to call with non-existent key
(!) :: Manifest -> EntryId -> Entry
t ! k = liftQ (M.!) t k
-- t ! k = entries t M.! k

size :: Manifest -> Int
size = liftQ M.size

member :: EntryId ->  Manifest -> Bool
member = liftQ . M.member
notMember k t = not $ member k t

lookup :: EntryId -> Manifest -> Maybe Entry
lookup = liftQ . M.lookup
-- lookup' k = fromJust . lookup k
-- lookupWithDefault d k t = fromMaybe d (lookup k t)

insert :: Entry -> Manifest -> Manifest
insert e = liftU $ M.insert (entryId e) e

replace :: Entry -> Manifest -> (Maybe Entry, Manifest)
replace e m = (lookup eid m, insert e m)
  where eid = entryId e

remove :: Entry -> Manifest -> Manifest
remove = delete . entryId

delete :: EntryId -> Manifest -> Manifest
delete k = liftU $ M.delete k



-- Entry functions

increment :: Manifest -> (EntryId, Manifest)
increment m = (ctr m, m')
  where m' = m { ctr = ctr m + 1 }

top :: Manifest -> Entry
top m = fromJust $ lookup (topId m) m

isTop :: Entry -> Manifest -> Bool
isTop e m = entryId e == topId m

lookupParent :: Entry -> Manifest -> Maybe Entry
lookupParent = lookup . parentId
lookupParent' e m = fromMaybe orphanErr $ lookupParent e m

lookupChild :: String -> Entry -> Manifest -> Maybe Entry
lookupChild n e m = if isDir e
  then (P.lookup n . dirList $ e) >>= (`lookup` m)
  else Nothing

lookupName :: Entry -> Manifest -> Maybe String
lookupName e m = if isTop e m
  then Just ""
  else lookupParent e m >>= childName (entryId e)

childName :: EntryId -> Entry -> Maybe String
childName eid p = fst <$> L.find ((eid ==) . snd) (dirList p)

-- lookupChild :: (String, EntryId) -> Manifest -> Maybe (String, Entry)
-- lookupChild (n, eid) m = do
--   e' <- lookup eid m
--   return (n, e')

lookupChildren :: Entry -> Manifest -> Maybe [(String, Entry)]
lookupChildren e m = if isDir e
    then Just . mapMaybe f . dirList $ e
    else Nothing
  where
    f (n, eid) = do
      e' <- lookup eid m
      return (n, e')



-- Entry creation funcs

assignPid pid e = e { parentId = pid }

type UEntry = (EntryId -> EntryId -> Entry)

uentry :: AccessHeader -> ContentHeader -> UEntry
uentry ah ch eid pid = Entry eid pid ah ch
udirectory :: AccessHeader -> [(String,EntryId)] -> UEntry
udirectory ah = uentry ah . DirContents . M.fromList
ufile :: AccessHeader -> FileHeader -> UEntry
ufile ah = uentry ah . FileContents



-- Entry modification funcs

modifyDir :: (DirMap -> DirMap) -> Entry -> Entry
modifyDir f e =  e { contentHeader = DirContents . f . dirMap $ e }

linkParent :: Entry -> String -> Manifest -> Manifest
linkParent e n m = insert p' m
  where p' = modifyDir (M.insert n $ entryId e) $ lookupParent' e m

unlinkParent :: Entry -> Manifest -> Manifest
unlinkParent e m = insert p' m
  where
    p = lookupParent' e m
    n = fromJust $ lookupName e m
    p' = modifyDir (M.delete n) p



-- Contents

-- Extrapolated type, not serialized
data EntryType
  = DirType
  | FileType
  deriving (Show, Read, Eq)

entryType :: Entry -> EntryType
entryType e = case contentHeader e of
  DirContents _ -> DirType
  FileContents _ -> FileType

isEntryType :: EntryType -> Entry -> Bool
isEntryType t e = entryType e == t

isNotFileErr = error "Error: Is not file."
isNotDirErr = error "Error: Is not directory."

assumeDir p a = if isDir p then a else isNotDirErr
assumeFile p a = if isFile p then a else isNotFileErr

isFile :: Entry -> Bool
isFile = isEntryType FileType

isDir :: Entry -> Bool
isDir = isEntryType DirType

fileHeader :: Entry -> FileHeader
fileHeader e = case contentHeader e of
  FileContents fh -> fh
  _ -> isNotFileErr

fileBlockHeaders :: Entry -> [BlockHeader]
fileBlockHeaders e = case contentHeader e of
  FileContents fh -> dataBlockHeaders fh
  _ -> isNotFileErr

dirMap :: Entry -> DirMap
dirMap e = case contentHeader e of
  DirContents nids -> nids
  _ -> isNotDirErr

dirList :: Entry -> [(String,EntryId)]
dirList = M.toList . dirMap

dirNames :: Entry -> [String]
dirNames = map fst . dirList




-- Path / tree funcs
--  Semantics are modeled after System.Directory

parentNotExistErr = error "Parent directory does not exist."
pathNotExistErr = error "Path does not exist."
pathAlreadyExistsErr = error "Path already exists."
dirNotExistErr = error "Directory does not exist."
fileNotExistErr = error "File does not exist."

findFrom :: Entry -> Path -> Manifest -> Maybe Entry
findFrom e (n:ns) m = do
  next <- lookupChild n e m
  findFrom next ns m
findFrom e [] _ = Just e

find :: Path -> Manifest -> Maybe Entry
find [] m = Just $ top m
find ns m = findFrom (top m) ns m

pathType :: Path -> Manifest -> Maybe EntryType
pathType p m = entryType <$> find p m

pathExists :: Path -> Manifest -> Bool
pathExists p m = isJust $ find p m

pathIsDirectory :: Path -> Manifest -> Bool
pathIsDirectory p m = maybe False isDir $ find p m

pathIsFile :: Path -> Manifest -> Bool
pathIsFile p m = maybe False isFile $ find p m

-- NOTE: Parent path must exist or it will error
createPath :: UEntry -> Path -> Manifest -> Manifest
createPath _ [] _ = error "Error: Cannot create root."
createPath ue p m
    | pathExists p m = pathAlreadyExistsErr
    | pathIsDirectory pp m = linkParent e n . insert e $ m'
    | otherwise = parentNotExistErr
  where
    (pp,n) = (init p, last p) -- Parent path, name
    pe = fromJust $ find pp m -- Parent entry
    e = ue eid (entryId pe)
    (eid, m') = increment m

movePath :: Path -> Path -> Manifest -> Manifest
movePath [] _ _ = error "Cannot move root."
movePath _ [] _ = error "Cannot move onto root."
movePath p p' m
    | pathExists p' m = pathAlreadyExistsErr
    | pathIsDirectory pp' m = linkParent e' n . insert e' . unlinkParent e $ m
    | otherwise = parentNotExistErr
  where
    (pp', n) = (init p', last p')  -- Parent path, name
    e = fromJust $ find p m   -- Src entry
    pe = fromJust $ find pp' m -- Dest parent entry
    e' = e { parentId = entryId pe }

removePath :: Path -> Manifest -> Manifest
removePath p m = case entryType . fromJust $ find p m of
  DirType -> removeDirectory p m
  FileType -> removeFile p m

removePathForcibly :: Path -> Manifest -> Manifest
removePathForcibly p m = if pathExists p m
    then case entryType e of
      DirType -> removeEntry . removeChildren $ m
      FileType -> removeEntry m
    else pathNotExistErr
  where
    e = fromJust $ find p m
    children = readDirectory p m
    removeChildren m = foldr removePathForcibly m children
    removeEntry = delete (entryId e) . unlinkParent e

createDirectory :: Path -> Manifest -> Manifest
createDirectory p m = createPath (udirectory (accessNow m) []) p m

createDirectoryIfMissing :: Bool -> Path -> Manifest -> Manifest
createDirectoryIfMissing _ [] m = m -- Reached root
createDirectoryIfMissing recurse p m = case pathType p m of
    Just DirType -> m -- Already exists, return
    Just FileType -> error "File exists in path."
    Nothing -> if recurse
      then createDirectory p . createDirectoryIfMissing True p' $ m
      else createDirectory p m
  where
    p' = init p

readDirectory :: Path -> Manifest -> [Path]
readDirectory p m = if pathIsDirectory p m
  then map (\n -> p ++ [n]) . dirNames . fromJust $ find p m
  else isNotDirErr

-- TODO: Should be Bool -> Path -> Manifest -> [Path]
readDirectoryRecursive :: Path -> Manifest -> [Path]
readDirectoryRecursive p m = if pathIsDirectory p m
    then p's ++ (concatMap readChild . filter childIsDir $ p's)
    else isNotDirErr
  where
    p's = readDirectory p m
    readChild n = readDirectoryRecursive n m
    childIsDir n = pathIsDirectory n m


removeDirectory :: Path -> Manifest -> Manifest
removeDirectory p m = if pathIsDirectory p m
  then case readDirectory p m of
    [] -> removePathForcibly p m
    _ -> error "Error: Directory is not empty."
  else isNotDirErr

removeDirectoryRecursive :: Path -> Manifest -> Manifest
removeDirectoryRecursive p m = if pathIsDirectory p m
  then removePathForcibly p m
  else isNotDirErr

createFile :: Path -> FileHeader -> Manifest -> Manifest
createFile p fh m = createPath (ufile (accessNow m) fh) p m

readFile :: Path -> Manifest -> FileHeader
readFile p m = if pathIsFile p m
    then fileHeader . fromJust $ find p m
    else isNotFileErr

writeFile :: Path -> FileHeader -> Manifest -> Manifest
writeFile p fh m = if pathIsFile p m
    then insert (e { contentHeader = FileContents fh }) m
    else isNotFileErr
  where
    e = fromJust $ find p m

removeFile :: Path -> Manifest -> Manifest
removeFile p m = if pathIsFile p m
  then removePathForcibly p m
  else isNotFileErr



-- Experimental

globDir :: Path -> String -> Manifest -> ([Path],[Path])
globDir p pat m = if pathIsDirectory p m
    then L.partition (G.match c . toFilePath) $ readDirectoryRecursive p m
    else isNotDirErr
  where
    c = G.simplify $ G.compile pat

globDir' p s m = fst $ globDir p s m
globDir'' p s m = snd $ globDir p s m



-- Repair

-- NOTE: With exposed functions, orphans should never happen

orphanErr = error "Error: Orphaned entry."

isOrphan :: Entry -> Manifest -> Bool
isOrphan e m = isNothing $ lookupParent e m

isIndirectOrphan :: Entry -> Manifest -> Bool
isIndirectOrphan e m
    | isTop e m = False -- Top is never an orphan
    | isJust e' = isIndirectOrphan (fromJust e') m
    | otherwise = True
  where e' = lookupParent e m

-- findOrphans :: Manifest -> [Entry]

-- lostChildren :: Entry -> [EntryId] -- A directory may have missing children

-- State
