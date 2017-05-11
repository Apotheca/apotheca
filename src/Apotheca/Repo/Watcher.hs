module Apotheca.Repo.Watcher
( WatchStrategy (..)
, defaultWatcher
, simpleWatcher
, runWatcher
) where



import           GHC.Generics

import           Control.Concurrent     (threadDelay)
import           Control.Concurrent.STM
import           Control.Monad          (forever)

import           System.FSNotify

import           Apotheca.Encodable
import           Apotheca.Repo.Glob
import           Apotheca.Repo.Internal (Config (..), Env (..), Repo (..),
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


-- TODO: Periodic (configurable, default 15 min) indexing to detect 'missed' files.
-- TODO: On watcher start, synchronize to update from changes before program run
-- NOTE:
runWatcher :: RIO ()
runWatcher = do
    watches <- queryConfig watchedDirs
    tr <- getRM >>= io . newTVarIO -- TVar Repo aka tr
    io $ withManagerConf conf $ \mgr -> do
      -- start each watching job (in the background)
      mapM_ (watchWithStrategy tr mgr) watches
      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000
    putRM =<< io (readTVarIO tr)
  where
    conf = WatchConfig
      { confDebounce = DebounceDefault
      , confPollInterval = 1000000
      , confUsePolling = False
      }

-- starts a watching job in the background, returning a stop IO ()
watchWithStrategy :: TVar Repo -> WatchManager -> WatchStrategy -> IO StopListening
watchWithStrategy tr mgr ws = do
  watchTree
    mgr          -- manager
    (sourcePath ws) -- directory to watch
    (const True) -- (shouldUpdate ws)
    print  -- (handleEvent w)

-- NOTE: For now, we're letting the repo decide if it should-write since that logic
--  is sort of stuck to the do-write
-- shouldUpdate :: TVar Repo -> WatchStrategy -> Event -> Bool
-- shouldUpdate tr ws e@(Added p utc) = undefined
-- shouldUpdate tr ws e@(Modified p utc) = undefined
-- shouldUpdate tr ws e@(Removed p utc) = undefined

handleEvent :: TVar Repo -> WatchStrategy -> Event -> IO ()
handleEvent tr ws e@(Added p utc) = undefined
handleEvent tr ws e@(Modified p utc) = undefined
handleEvent tr ws e@(Removed p utc) = undefined
