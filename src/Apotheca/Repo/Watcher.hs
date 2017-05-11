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

import           System.FilePath         (makeRelative, takeDirectory, (</>))

import           System.FSNotify

import           Apotheca.Encodable
import           Apotheca.Repo.Glob
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Monad
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

-- MVar helper
withMRepo :: MVar Repo -> RIO a -> IO a
withMRepo mr = withMVar mr . evalRM

modifyMRepo :: MVar Repo -> RM IO a -> IO ()
modifyMRepo mr = modifyMVar_  mr . execRM

-- TODO: Periodic (configurable, default 15 min) indexing to detect 'missed' files.
-- TODO: On watcher start, synchronize to update from changes before program run
runWatcher :: MVar Repo -> IO ()
runWatcher mr = withManagerConf conf $ \mgr -> do
    -- Get list of watch strategies
    watches <- withMRepo mr $ queryConfig watchedDirs
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
    (shouldUpdate ws)
    (\e -> do -- print -- (handleEvent mr ws)
      putStrLn $ "WEvent: " ++ show e
      handleEvent mr ws e
      )

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

handleEvent :: MVar Repo -> WatchStrategy -> Event -> IO ()
handleEvent mr ws (Added p utc) = handleEvent mr ws (Modified p utc)
handleEvent mr ws (Modified p utc) = modifyMRepo mr $ do
    verbose $ "Watcher modified: " ++ toFilePath dst
    verbose $ "Source: " ++ p'
    putPath pf True True p dst
    verbose $ "Success!" ++ toFilePath dst
    persistRepo
  where
    pf = PutFlags
      { pfWriteMode = Overwrite
      , pfSplitStrat = Inherit
      , pfHashStrat = Inherit
      , pfCompression = Inherit
      , pfCipherStrat = Inherit
      }
    p' = relsrc ws p
    dst = fromFilePath $ (destPath ws) </> p'
handleEvent mr ws (Removed p utc) = modifyMRepo mr $ do
    verbose $ "Watcher removed: " ++ toFilePath dst
    verbose $ "Source: " ++ p'
    delPath True dst
    verbose $ "Success!" ++ toFilePath dst
    persistRepo
  where
    p' = relsrc ws p
    dst = fromFilePath $ (destPath ws) </> p'
