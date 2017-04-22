module Apotheca.Repo.Watcher
( WatchStrategy (..)
, SyncMode (..)
, defaultWatcher
, watcher
, runWatcher
) where



import           GHC.Generics

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever)

import           System.FSNotify

import           Apotheca.Encodable
import           Apotheca.Repo.Path
import           Apotheca.Repo.Types (SyncMode (..), Transaction (..),
                                      WatchStrategy (..))



defaultWatcher = WatchStrategy
  { syncMode = SynchronizeMode
  , syncDirection = Push
  , globFilter = Nothing
  , sourcePath = "/"
  , destPath = "."
  , pollInterval = 5000000
  , forcePolling = False
  }

type UntargetedWatcher = (FilePath -> FilePath -> WatchStrategy)

-- watcher AdditiveMode +>

watcher :: SyncMode -> Transaction -> Maybe [String] -> UntargetedWatcher
watcher m d mg src dst = defaultWatcher
  { syncMode = m
  , syncDirection = d
  , globFilter = mg
  , sourcePath = src
  , destPath = dst
  }



-- NOTE: For now, if a path is reused in additive / dead-drop mode, it is just
--  overwritten. In the future we may want to do stuff such as optionally rename
--  the old manifest entry, or something.
-- TODO: Rename SyncMode = | Synchronize | Additive | DeadDrop
--  This would be useful in a `caligo synchronize` command as well

-- TODO: Periodic (configurable, default 15 min) indexing to detect 'missed' files.
-- TODO: On watcher start, synchronize to update from changes before program run
runWatcher :: WatchStrategy -> IO ()
runWatcher w = withManagerConf conf $ \mgr -> do
    -- start a watching job (in the background)
    watchTree
      mgr          -- manager
      (sourcePath w) -- directory to watch
      (const True) --(shouldUpdate w)-- predicate
      print  -- (handleEvent w)
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where
    conf = WatchConfig
      { confDebounce = DebounceDefault
      , confPollInterval = pollInterval w
      , confUsePolling = forcePolling w
      }

-- shouldUpdate w e@(Added p utc) = case SyncMode w of
--   SynchronizeMode -> True
--   AdditiveMode _ -> True
--   DeadDropMode -> True
-- shouldUpdate w e@(Modified p utc) = case SyncMode w of
--   SynchronizeMode -> True
--   AdditiveMode _ -> True
--   DeadDropMode -> True -- Conceivably false, but could be written and immediately updated
-- shouldUpdate w e@(Removed p utc) = case SyncMode w of
--   SynchronizeMode -> True
--   AdditiveMode _ -> False
--   DeadDropMode -> False

-- handleEvent :: WatchStrategy -> Event -> IO ()
-- handleEvent w e@(Added p utc) = case SyncMode w of
--   SynchronizeMode -> undefined -- Add file to repo
--   AdditiveMode -> undefined -- Add file to repo
--   DeadDropMode -> undefined -- Add file to repo, delete original
-- handleEvent w e@(Modified p utc) = case SyncMode w of
--   SynchronizeMode -> undefined -- Overwrite file in repo
--   AdditiveMode -> undefined -- Overwrite file in repo)
--   DeadDropMode -> undefined -- Add file to repo, delete original
-- handleEvent w e@(Removed p utc) = case SyncMode w of
--   SynchronizeMode -> undefined -- Remove file from reop
--   AdditiveMode -> return () -- error ""
--   DeadDropMode -> return () -- Add file to repo, delete original
