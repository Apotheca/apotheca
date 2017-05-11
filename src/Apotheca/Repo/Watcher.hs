module Apotheca.Repo.Watcher
( WatchStrategy (..)
, defaultWatcher
, simpleWatcher
, runWatcher
) where



import           GHC.Generics

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad           (forever)

import           System.FSNotify

import           Apotheca.Encodable
import           Apotheca.Repo.Glob
import           Apotheca.Repo.Internal  (Config (..), Env (..), Repo (..),
                                          WatchStrategy (..))
import           Apotheca.Repo.Monad



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

-- MVar helper
withMRepo :: MVar Repo -> RIO b -> IO b
withMRepo mr = withMVar mr . evalRM

-- TODO: Periodic (configurable, default 15 min) indexing to detect 'missed' files.
-- TODO: On watcher start, synchronize to update from changes before program run
runWatcher :: MVar Repo -> IO ()
runWatcher mr = withManagerConf conf $ \mgr -> do
    -- Get list of watch strategies
    watches <- withMRepo mr $ (queryConfig watchedDirs)
    -- start each watching job (in the background)
    mapM_ (watchWithStrategy mr mgr) watches
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where
    conf = WatchConfig
      { confDebounce = DebounceDefault
      , confPollInterval = 1000000
      , confUsePolling = False
      }

-- starts a watching job in the background, returning a stop IO ()
watchWithStrategy :: MVar Repo -> WatchManager -> WatchStrategy -> IO StopListening
watchWithStrategy mr mgr ws = do
  watchTree
    mgr          -- manager
    (sourcePath ws) -- directory to watch
    (shouldUpdate mr ws)
    print  -- (handleEvent w)

-- NOTE: For now, we're letting the repo decide if it should-write since that logic
--  is sort of stuck to the do-write
shouldUpdate :: MVar Repo -> WatchStrategy -> Event -> Bool
shouldUpdate _ _ _ = True
-- shouldUpdate mr ws e@(Added p utc) = undefined
-- shouldUpdate mr ws e@(Modified p utc) = undefined
-- shouldUpdate mr ws e@(Removed p utc) = undefined

handleEvent :: MVar Repo -> WatchStrategy -> Event -> IO ()
handleEvent mr ws e@(Added p utc) = undefined
handleEvent mr ws e@(Modified p utc) = undefined
handleEvent mr ws e@(Removed p utc) = undefined
