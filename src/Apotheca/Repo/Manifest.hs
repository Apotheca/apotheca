{-# LANGUAGE OverloadedStrings #-}

module Apotheca.Repo.Manifest
( Manifest
, emptyManifest
, readManifestFile
, writeManifestFile
-- Path-based
, pathExists
, pathIsDirectory
, pathIsFile
-- , createPath
, movePath
, overwritePath
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

import qualified System.FilePath.Glob   as G

import qualified Crypto.Random          as R

import           Apotheca.Bytes
import           Apotheca.Encodable
import           Apotheca.Misc
import           Apotheca.Repo.Path
import           Apotheca.Repo.Types
import           Apotheca.Security.Hash (sha2, unsalted)
import           Data.Aeson             (Value (..), object, withArray, (.:),
                                         (.=))



-- So this class has been through a bunch of different concepts, and has some
--  funky baggage belied by its exported simplicity.
--  The 'entryId' stuff is somewhat overkill, given that it doens't even expose
--  Entry or EntryContents either. I mean, why not just do a tree (or treemap)
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
--  Files should be a list of blockid's consisting of initial commit + deltas
--  You should be able to walk down a file's history: although two forked
--  manifests may forked files, somewhere down the history they have a common root.
--  Keeping the history alive for one keeps those historical blocks alive for
--  others in decay-mode. Pruning should be possible and historical blocks may
--  disappear, but the manifests still have a record of ids and checksums.
--  Versioning means that data loss may still be partially recoverable.
--  Versioning but no pruning (or deleting at all) makes this an additive repo,
--  (much more git-like) and allowing us to use special distributed tactics.
--  See CAP theorem and ACID.


-- Directory Tree lib - https://hackage.haskell.org/package/directory-tree-0.12.1/docs/System-Directory-Tree.html

emptyManifest = Manifest
    { topId = tid
    , entries = M.singleton tid top
    -- , blocks = [] -- No owned-blocks cache for now, can iter over entries
    }
  where
    top = udirectory [] tid tid
    tid = B.empty



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

top :: Manifest -> Entry
top m = fromJust $ lookup (topId m) m

isTop :: Entry -> Manifest -> Bool
isTop e m = entryId e == topId m

lookupParent :: Entry -> Manifest -> Maybe Entry
lookupParent = lookup . parentId
lookupParent' e m = fromMaybe orphanErr $ lookupParent e m

lookupChild :: String -> Entry -> Manifest -> Maybe Entry
lookupChild n e m = if isDir e
  then (P.lookup n . dirContents $ e) >>= (`lookup` m)
  else Nothing

lookupName :: Entry -> Manifest -> Maybe String
lookupName e m = if isTop e m
  then Just ""
  else lookupParent e m >>= childName (entryId e)

childName :: EntryId -> Entry -> Maybe String
childName eid p = fst <$> L.find ((eid ==) . snd) (dirContents p)

-- lookupChild :: (String, EntryId) -> Manifest -> Maybe (String, Entry)
-- lookupChild (n, eid) m = do
--   e' <- lookup eid m
--   return (n, e')

lookupChildren :: Entry -> Manifest -> Maybe [(String, Entry)]
lookupChildren e m = if isDir e
    then Just . mapMaybe f . dirContents $ e
    else Nothing
  where
    f (n, eid) = do
      e' <- lookup eid m
      return (n, e')



-- Entry creation funcs

assignPid pid e = e { parentId = pid }

type UEntry = (EntryId -> EntryId -> Entry)

uentry :: EntryContents -> UEntry
uentry c eid pid = Entry eid pid c
udirectory :: [(String,EntryId)] -> UEntry
udirectory = uentry . DirContents . M.fromList
ufile :: [BlockId] -> UEntry
ufile = uentry . FileContents



-- Entry modification funcs

-- renamed to updateDir
modifyDir :: (DirMap -> DirMap) -> Entry -> Entry
modifyDir f e =  e { entryContents = DirContents . f . dirContentsMap $ e }

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
entryType e = case entryContents e of
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

fileContents :: Entry -> [BlockId]
fileContents e = case entryContents e of
  FileContents bids -> bids
  _ -> isNotFileErr

dirContentsMap :: Entry -> DirMap
dirContentsMap e = case entryContents e of
  DirContents nids -> nids
  _ -> isNotDirErr

dirContents :: Entry -> [(String,EntryId)]
dirContents = M.toList . dirContentsMap

dirNames :: Entry -> [String]
dirNames = map fst . dirContents




-- Path / tree funcs
--  Semantics are modeled after System.Directory

-- NOTE: EntryId doesn't need to exposed or provided externally - we just need
--  it to be internally unique. Having the id based on a hash of the path
--  fulfills this function so long as we remember to change the id when we move
--  the file. Later, we may need ids to be consistent even after moving, at
--  which case we'll likely need to move to incremental or random IDs (or
--  hash-path + non-repeating salts).
path2id :: Path -> EntryId
path2id = unsalted sha2 . BC.pack . L.intercalate "/"

pathNotExistErr = error "Error: Path does not exist."
pathAlreadyExistsErr = error "Error: Path already exists."
dirNotExistErr = error "Error: Directory does not exist."
fileNotExistErr = error "Error: File does not exist."

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

-- Yes this is confusing, because p = path but n = parentId and p' = parentEntry
createPath :: UEntry -> Path -> Manifest -> Manifest
createPath _ [] _ = error "Error: Cannot create root."
createPath ue p m = if pathIsDirectory pp m
    then linkParent e n . insert e $ m
    else isNotDirErr
  where
    (pp,n) = (init p, last p) -- Parent path, name
    pe = fromJust $ find pp m -- Parent entry
    e = ue (path2id p) (entryId pe)

-- NOTE: Because ids are based on paths, we must change them when we move them
--  to reflect the new path, else we will get the same id if we create a new
--  entry at the old path
overwritePath :: Path -> Path -> Manifest -> Manifest
overwritePath [] _ _ = error "Cannot move root."
overwritePath _ [] _ = error "Cannot move onto root."
overwritePath p p' m = if pathIsDirectory pp' m
    then linkParent e' n . insert e' . remove e . unlinkParent e $ m
    else error "Parent directory does not exist."
  where
    (pp', n) = (init p', last p')  -- Parent path, name
    e = fromJust $ find p m   -- Entry
    pe = fromJust $ find pp' m -- Parent entry
    e' = uentry (entryContents e) (path2id p') (entryId pe) -- Updated e

movePath :: Path -> Path -> Manifest -> Manifest
movePath p p' m = if pathExists p' m
  then pathAlreadyExistsErr
  else overwritePath p p' m

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
createDirectory = createPath $ udirectory []

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

createFile :: Path -> [BlockId] -> Manifest -> Manifest
createFile = flip $ createPath . ufile

readFile :: Path -> Manifest -> [BlockId]
readFile p m = if pathIsFile p m
    then fileContents . fromJust $ find p m
    else isNotFileErr

writeFile :: Path -> [BlockId] -> Manifest -> Manifest
writeFile p bids m = if pathIsFile p m
    then insert (e { entryContents = FileContents bids }) m
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
isIndirectOrphan e m = case (isTop e m, lookupParent e m) of
  (True, _) -> False -- Top is never an orphan
  (False, Just e') -> isIndirectOrphan e' m
  (_,_) -> True

-- findOrphans :: Manifest -> [Entry]

-- lostChildren :: Entry -> [EntryId] -- A directory may have missing children

-- State
