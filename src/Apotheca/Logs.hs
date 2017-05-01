module Apotheca.Logs
( msg
, logv
, logM
, debug
, verbose
, terse
, warn
, fatal
, Verbosity (..)
) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class

import           Debug.Trace            (traceIO)

data Verbosity
  = Debug
  | Verbose
  | Terse
  | Warn
  | Fatal
  | Silent
  deriving (Show, Read, Eq, Ord)

msg = traceIO

logv :: Verbosity -> Verbosity -> String -> IO ()
logv r v s = when (r >= v) $ msg s

logM :: (MonadIO m) => Verbosity -> Verbosity -> String -> m ()
logM r v s = liftIO $ logv r v s

debug = logv Debug
verbose = logv Verbose
terse = logv Terse
warn = logv Warn
fatal = logv Fatal
