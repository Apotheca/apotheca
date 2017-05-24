module Apotheca.Runtime.Commands
( RuntimeCommand (..)
, runCommand
) where



import           Control.Monad            (foldM, void, when)

import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.List                as L
import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

import           Apotheca.Encodable
import qualified Apotheca.Logs            as Lg
import           Apotheca.Repo.Config
import           Apotheca.Repo.Env
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Monad
import           Apotheca.Repo.MVar
import           Apotheca.Repo.Path
import           Apotheca.Repo.Watcher
import           Apotheca.Security.Cipher
import           Apotheca.Security.Hash



-- TODO: Command-line list
-- Repo management
--  new
--  nuke
-- Query
--  find -- prints path of repo if it can find one
--  info -- prints repo info
-- Map-like
--  get
--  put
--  del
--  list -- prints paths
-- Sync strategies using sync|additive|deaddrop - like smart get/put/del
--  push - local -> repo
--  pull - repo -> local
--  transfer - repo -> repo
-- Watch
--  watch -- adds a new watch to the config
--  unwatch -- removes a watch from the config
-- Node
--  run -- run with various options - httpserve, mount, watch

data RuntimeCommand
  = NoCommand
  -- Repo management
  | New ConfigFlags -- TODO: --template <TEMPLATE>
  | Nuke Bool
  -- Query
  | Where
  -- TODO: | Open -- basically apo where | xargs open in shell
  | Info
  | Ciphers -- TODO: --list <|> encipher cs n src dst
  | Hashes -- TODO: --list <|> hash hs src
  -- | Find -- Something smarter than 'list'
  -- Auth
  | Auth
  | Unauth
  -- Map-like
  | Find FilterFlags FilePath
  -- | Filter
  | List Bool Bool FilePath -- Recurse, tree, dst
  -- TODO: Add --large :: (Maybe Bool) flag to the opr flagset
  -- Overwrite files, prune [instead of merge] directories, recursive
  | Get GetFlags Bool Bool FilePath FilePath
  | Put PutFlags Bool Bool FilePath FilePath
  -- Force, dst
  | Del Bool FilePath
  -- Sync
  -- | SyncPush SyncMode (Maybe Glob) FilePath FilePath
  -- | SyncPull SyncMode (Maybe Glob) FilePath FilePath
  -- | Transfer SyncMode
  -- Watch
  | Watch (Maybe Glob) FilePath FilePath
  | Unwatch (Maybe Glob) FilePath
  -- Node
  | RunNode Bool
  -- Version
  | Version
  deriving (Show, Read, Eq)





-- NOTE: repoType e may != (repoTye . repoEnv) r, since opening a repo sets type
--  This is because repotype isn't really an environment variable - it should
--  probably be a repo variable, but is needed prior to loading the repo, so it
--  lives in the env
runCommand :: RuntimeCommand -> Env -> IO ()
runCommand cmd e = do
  case cmd of
    -- Repo-less (env-only) commands
    New cf -> runNew cf e
    Where -> runWhere e
    Info -> runInfo e
    Hashes -> runHashes e
    Ciphers -> runCiphers e
    _ -> do
      e' <- fixMasterSecret e
      r <- openRepo e'
      flip evalRM r $ case cmd of
        Nuke force -> runNuke force
        -- Auth
        Auth -> runAuth $ masterSecret e'
        Unauth -> runUnauth
        -- Map-like
        Find ff src -> runFind ff (convertInt src)
        List rc t dst -> runList rc t (convertInt dst)
        Get gf pr rc src dst -> runGet gf pr rc (convertInt src) (convertExt dst)
        Put pf pr rc src dst -> runPut pf pr rc (convertExt src) (convertInt dst)
        Del force dst -> runDel force (convertInt dst)
        -- Watcher
        Watch mg src dst -> runWatch mg (convertExt src) (convertInt dst)
        Unwatch mg src -> runUnwatch mg (convertExt src)
        -- Node
        RunNode rw -> runRunNode rw
        _ -> io $ putStrLn "Unimplemented command!"
  where
    v = verbosity e
    -- Simplistic convert
    --  if relative, makes relative to int-dir / ext-dir
    --  does not multiplex
    --  loses trailing slashes for intpath because Path is just a stub type
    -- TODO: This should be part of Apotheca.Repo.IO or something, and used in Apotheca.Repo.Repo
    convertInt p = toFilePath (intDir e) </> p
    -- convertExt p = extDir e </> p
    convertExt p = if p == "-" then p else extDir e </> p


-- Commands

-- Repo management commands

runNew cf e = do
    Lg.terse v $ "Attempting to create repo at: " ++ repoDir e
    p <- case masterSecret e of
      Just p -> return $ Just p
      Nothing -> do
        getpass <- promptYn "Do you wish to create a master password?"
        if getpass
          then Just <$> promptPass
          else return Nothing
    void . createRepo cf $ e
      { repoType = if bare then BareRepo else HiddenRepo
      , masterSecret = p
      }
  where
    v = verbosity e
    bare = cfBare cf

runNuke :: Bool -> RIO ()
runNuke force = do
  dp <- dataPath
  terse $ "Destroying repo at: " ++ dp
  confirmed <- if force
    then return True
    else io $ promptYn "Confirm?"
  if confirmed
    then do
      verbose "Destroying repo..."
      destroyRepo
      terse "Repo destroyed."
    else terse "Aborted."

-- Query commands

runWhere e = do
    Lg.verbose v $ "Looking for repo from: " ++ repoDir e
    mrd <- findRepo $ repoDir e
    case mrd of
      Just p -> putStrLn p
      Nothing -> putStrLn "Not found."
  where v = verbosity e

runInfo e = do
    Lg.debug v "Printing environment and repo info..."
    exists <- doesRepoExist $ repoDir e
    if exists
      then do
        r <- openRepo e
        putStrLn "Environment:"
        printPairs . envPairs $ repoEnv r
        putStrLn "Repository:"
        printPairs $
          [ ("BARE", show $ isBare r)
          ]
        putStrLn "Watches:"
        printWatches . watchedDirs $ repoConfig r
      else do
        putStrLn "Environment:"
        printPairs $ envPairs e
  where
    v = verbosity e
    isBare = (== BareRepo) . repoType . repoEnv

runHashes e = do
  putStrLn "Available hashes:"
  mapM_ (\h -> putStrLn ("  " ++ show h)) availableHashes

runCiphers e = do
  putStrLn "Available ciphers:"
  mapM_ (\h -> putStrLn ("  " ++ show h)) availableCiphers

-- Auth

runAuth Nothing = error "Cannot auth without password."
runAuth (Just authkey) = do
  fp <- (</> "AUTHKEY") <$> dataPath
  exists <- io $ doesFileExist fp
  when exists $ warn "Overwriting existing auth!"
  io $ B.writeFile fp authkey

runUnauth = do
  fp <- (</> "AUTHKEY") <$> dataPath
  exists <- io $ doesFileExist fp
  if exists
    then io $ removeFile fp
    else error "Cannot unauth: Auth does not exist."

-- Map-like

runFind ff src = do
  paths <- findPath ff (fromFilePath src)
  io $ mapM_ (putStrLn . toFilePath) paths

-- NOTE: No multiplexing on the list
-- TODO: Multiplex on magic slash /only/ if explicitly set?
runList rc tree dst = do
    paths <- listPath rc (fromFilePath dst)
    io $ mapM_ (putStrLn . tf) paths
  where
    tf = if tree
      then error "--tree flag is not yet implemented"
      else toFilePath

runPut pf pr rc src dst = if src == "-"
    then putHandle pf stdin (fromFilePath dst) >> persistRepo
    else multiplexExt f src >> persistRepo
  where f src' = putPath pf pr rc src' (fromFilePath dst)

runGet gf pr rc src dst = if dst == "-"
    then getHandle gf stdout (fromFilePath src)
    else void $ multiplexInt f src
  where f src' = getPath gf pr rc src' dst

-- runDel force dst r = delPath force dst r >>= void . persistRepo
runDel force dst = multiplexInt (delPath force) dst >> persistRepo



-- Sync

runPush sm r = undefined
runPull sm r = undefined
runTransfer sm r = undefined

-- Watches

runWatch mg src dst = do
    wds <- queryConfig watchedDirs
    case L.find match wds of
      -- Update existing
      Just ws -> do
        let ws' = addGlob ws
        setWatchedDirs $ map (\w -> if match w then ws' else w) wds
        syncWatcher ws'
      -- Add new
      Nothing -> do
        setWatchedDirs $ wds ++ [newWatch]
        syncWatcher newWatch
    persistRepo
  where
    g' = case mg of
      Just g -> [g]
      Nothing -> []
    match ws = sourcePath ws == src && destPath ws == dst
    newWatch = simpleWatcher g' src dst
    addGlob ws = ws { globs = L.nub (globs ws ++ g') }
    setWatchedDirs wds = modifyConfig (\c -> c { watchedDirs = wds })

-- TODO: syncWatcher on unwatch
runUnwatch mg src = do
    wd <- queryConfig watchedDirs
    let wd' = mapMaybe mutate wd
    modifyConfig (\c -> c { watchedDirs = wd' }) >> persistRepo
  where
    match ws = sourcePath ws == src
    mutate ws = if match ws
      then case mg of
        Just g -> Just $ ws { globs = filter (/= g) $ globs ws }
        Nothing -> Nothing
      else Just ws

-- Run

runRunNode rw = do
  mr <- getRM >>= io . newMRepo
  when rw $ io $ runWatcher mr






-- Helpers

printPairs :: [(String,String)] -> IO ()
printPairs = mapM_ (\(s,v) -> putStrLn $ concat ["\t",s,": ",v])

envPairs e =
  [ ("STOREDIR", repoDir e)
  -- , ("STORETYPE", show $ repoType e)
  , ("EXTDIR", extDir e)
  , ("INTDIR", toFilePath $ intDir e)
  , ("MANIFEST", fromMaybe "DEFAULT" $ selManifest e)
  , ("DRYRUN", show $ dryRun e)
  , ("MAGIC-SLASH", show $ magicSlash e)
  , ("VERBOSITY", show $ verbosity e)
  ]

printWatches :: [WatchStrategy] -> IO ()
printWatches ws = putStrLn . unlines $ map ("\t" ++) ls
  where
    ls = filter (not . null) . lines . BC.unpack $ encodeYAML ws
