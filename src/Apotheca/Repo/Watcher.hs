module Apotheca.Repo.Watcher
( WatchStrategy (..)
, defaultWatcher
, simpleWatcher
, runWatcher
-- Exposed for now
, syncWatcher
) where

-- TODO: Move this module to Apotheca.Integrations



import           GHC.Generics

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)

import           System.FilePath        (makeRelative, takeDirectory, (</>))

import           System.FSNotify

import           Apotheca.Encodable
import           Apotheca.Repo.Env
import           Apotheca.Repo.Glob
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Monad
import           Apotheca.Repo.MVar
import           Apotheca.Repo.Path



defaultWatcher = WatchStrategy
  { globs = []
  , sourcePath = "."
  , destPath = "/"
  -- , pollInterval = 5000000 -- 5 second poll interval?
  -- , forcePolling = False
  }

simpleWatcher :: [String] -> FilePath -> FilePath -> WatchStrategy
simpleWatcher gs src dst = defaultWatcher
  { globs = gs
  , sourcePath = src
  , destPath = dst
  }

-- TODO: Periodic (configurable, default 15 min) indexing to detect 'missed' files.
-- TODO: On watcher start, synchronize to update from changes before program run
runWatcher :: MRepo -> IO ()
runWatcher mr = withManagerConf conf $ \mgr -> do
    -- Get list of watch strategies
    watches <- withMRepo mr $ queryConfig watchedDirs
    -- start each watching job (in the background)
    mapM_ (watchWithStrategy mr mgr) watches
    -- Perform initial syncs for changes that happened while not running
    modifyMRepo_ mr $ mapM_ syncWatcher watches
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where
    conf = WatchConfig
      { confDebounce = DebounceDefault
      , confPollInterval = 1000000
      , confUsePolling = False
      }

syncWatcher :: WatchStrategy -> RIO ()
syncWatcher ws = do
    verbose $ "Watcher sync: " ++ src ++ " -> " ++ dst
    watcherPut ws src
    persistRepo
  where
    src = sourcePath ws
    dst = destPath ws


-- starts a watching job in the background, returning a stop IO ()
watchWithStrategy :: MRepo -> WatchManager -> WatchStrategy -> IO StopListening
watchWithStrategy mr mgr ws = do
  watchTree
    mgr          -- manager
    (sourcePath ws) -- directory to watch
    (shouldUpdate ws)
    (handleEvent mr ws)

-- NOTE: For now, we're letting the repo decide if it should-write since that logic
--  is sort of stuck to the do-write
shouldUpdate :: WatchStrategy -> Event -> Bool
-- shouldUpdate _ _ _ = True
shouldUpdate ws (Added p utc) = matchStrategy ws p
shouldUpdate ws (Modified p utc) = matchStrategy ws p
shouldUpdate ws (Removed p utc) = matchStrategy ws p

relsrc ws p = makeRelative (sourcePath ws) p

matchStrategy ws p = null gs || any (flip gmatch p' . gcompile) gs
  where
    gs = globs ws
    p' = relsrc ws p

-- NOTE: OSX fsnotify events are screwed up
--  See: https://github.com/haskell-fswatch/hfsnotify/issues/36
-- "Solution" - figure event type from path existence

handleEvent :: MRepo -> WatchStrategy -> Event -> IO ()
handleEvent mr ws (Added p utc) = handleEvent' mr ws p
handleEvent mr ws (Modified p utc) = handleEvent' mr ws p
handleEvent mr ws (Removed p utc) = handleEvent' mr ws p

handleEvent' mr ws p = do
  exists <- doesPathExist p
  if exists
    then handleWriteEvent mr ws p
    else handleDeleteEvent mr ws p

handleWriteEvent :: MRepo -> WatchStrategy -> FilePath -> IO ()
handleWriteEvent mr ws p = modifyMRepo_ mr $ do
  watcherPut ws p
  persistRepo

handleDeleteEvent :: MRepo -> WatchStrategy -> FilePath -> IO ()
handleDeleteEvent mr ws p = modifyMRepo_ mr $ do
    verbose $ "Watcher remove: " ++ toFilePath dst
    delPath True dst
    persistRepo
  where
    p' = relsrc ws p
    dst = fromFilePath $ (destPath ws) </> p'

watcherPut :: WatchStrategy -> FilePath -> RIO ()
watcherPut ws p = do
    verbose $ "Watcher write: " ++ toFilePath dst
    putPath pf True True p dst
  where
    pf = PutFlags
      { pfWriteMode = Overwrite
      , pfSplitStrat = Inherit
      , pfHashStrat = Inherit
      , pfCompression = Inherit
      , pfCipherStrat = Inherit
      }
    p' = relsrc ws p
    dst = fromFilePath . takeDirectory $ (destPath ws) </> p'
