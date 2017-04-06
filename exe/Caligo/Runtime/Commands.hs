module Caligo.Runtime.Commands
( RuntimeCommand (..)
, runCommand
, multiplex
) where



import           Control.Monad         (foldM, void, when)

import qualified Data.ByteString.Char8 as BC
import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

import           Caligo.Encodable
import           Caligo.Logs
import           Caligo.Repo.Config
import           Caligo.Repo.Path
import           Caligo.Repo.Repo
import           Caligo.Repo.Types



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
  | New Bool
  | Nuke Bool
  -- Query
  | Where
  | Info
  -- | Find -- Something smarter than 'list'
  -- Map-like
  | List Bool FilePath -- Recurse, dst
  -- Overwrite files + replace [instead of merge] directories, recursive
  -- TODO: Add (rename :: String) option to get / put / params
  | Get Bool Bool Bool FilePath FilePath
  | Put Bool Bool Bool FilePath FilePath
  -- Force, dst
  | Del Bool FilePath
  -- Sync
  | Push WatchMode
  | Pull WatchMode
  | Transfer WatchMode
  -- Watch
  | Watch
  | Unwatch
  -- Node
  | RunNode
  deriving (Show, Read, Eq)





-- NOTE: repoType e may != (repoTye . repoEnv) r, since opening a repo sets type
--  This is because repotype isn't really an environment variable - it should
--  probably be a repo variable, but is needed prior to loading the repo, so it
--  lives in the env
runCommand :: RuntimeCommand -> Env -> IO ()
runCommand cmd e = do
  debug v $ "Command was:" ++ show cmd
  case cmd of
    -- Repo-less (env-only) commands
    New bare -> runNew bare e
    Nuke force -> runNuke force e
    Where -> runWhere e
    Info -> runInfo e
    _ -> do
      r <- openRepo e
      case cmd of
        List rc dst -> runList rc (convertInt dst) r
        Get ow rp rc src dst -> runGet ow rp rc (convertInt src) (convertExt dst) r
        Put ow rp rc src dst -> runPut ow rp rc (convertExt src) (convertInt dst) r
        Del force dst -> runDel force (convertInt dst) r
        _ -> putStrLn "Unimplemented command!"
  where
    v = verbosity e
    -- Simplistic convert
    --  if relative, makes relative to int-dir / ext-dir
    --  does not multiplex
    --  loses trailing slashes for intpath because Path is just a stub type
    -- TODO: This should be part of Caligo.Repo.IO or something, and used in Caligo.Repo.Repo
    convertInt p = toFilePath (intDir e) </> p
    convertExt p = extDir e </> p



-- Commands

-- Repo management commands

runNew bare e = do
    warn v $ "Attempting to create repo at: " ++ repoDir e
    void . createRepo $ e { repoType = if bare then BareRepo else HiddenRepo }
  where v = verbosity e

runNuke force e = do
    r <- openRepo e
    warn v $ "Attempting to destroy repo at: " ++ repoDir (repoEnv r)
    confirmed <- if force
      then return True
      else promptYn "Confirm nuke?"
    if confirmed
      then do
        warn v "Destroying repo..."
        destroyRepo r
      else warn v "Aborting nuke..."
  where v = verbosity e

-- Query commands

runWhere e = do
    debug v $ "Looking for repo from: " ++ repoDir e
    mrd <- findRepo $ repoDir e
    case mrd of
      Just p -> putStrLn p
      Nothing -> putStrLn "Not found."
  where v = verbosity e

runInfo e = do
    debug v "Printing environment and repo info..."
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
  where v = verbosity e

-- Map-like

-- NOTE: No multiplexing on the list
-- TODO: Multiplex on magic slash /only/ if explicitly set?
runList rc dst r = listPath rc (fromFilePath dst) r

-- TODO: Multiplexing / trailing-sep handled HERE instead of in Repo
-- runPut ow rp rc src dst r = putPath ow rp rc src dst r >>= void . persistRepo
runPut ow rp rc src dst r = multiplex f src r >>= void . persistRepo
  where f src' = putPath ow rp rc src' (fromFilePath dst)

-- runGet ow rp rc src dst r = getPath ow rp rc src dst r >>= void . persistRepo
runGet ow rp rc src dst r = multiplex' f src r >>= void . persistRepo
  where f src' = getPath ow rp rc src' dst

-- runDel force dst r = delPath force dst r >>= void . persistRepo
runDel force dst r = multiplex' (delPath force) dst r >>= void . persistRepo



-- Sync

runPush wm r = undefined
runPull wm r = undefined
runTransfer wm r = undefined

-- Watches

runWatch r = undefined
runUnwatch r = undefined

-- Run

runNode r = undefined





-- Helpers

prompt :: String -> IO String
prompt s = putStr (s ++ " ") >> hFlush stdout >> getLine

promptChar :: String -> IO Char
promptChar s = putStr (s ++ " ") >> hFlush stdout >> getCharImmediately

promptYn :: String -> IO Bool
promptYn s = (== 'Y') <$> promptChar (s ++ " Y/n")

getCharImmediately :: IO Char
getCharImmediately = withBufferMode stdin NoBuffering $ do
  c <- getChar
  putStrLn ""
  hFlush stdout
  return c

printPairs :: [(String,String)] -> IO ()
printPairs = mapM_ (\(s,v) -> putStrLn $ concat ["\t",s,": ",v])

withBufferMode :: Handle ->  BufferMode -> IO a -> IO a
withBufferMode h b f = do
  b' <- hGetBuffering h
  hSetBuffering h b
  a <- f
  hSetBuffering h b'
  return a

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
