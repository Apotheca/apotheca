module Caligo.Logs
( msg
, logv
, debug
, verbose
, warn
, fatal
, Verbosity (..)
) where

import           Control.Monad (when)
import           Debug.Trace   (traceIO)

data Verbosity
  = Debug
  | Verbose
  | Warn
  | Fatal
  | Silent
  deriving (Show, Read, Eq, Ord)

msg = traceIO

logv :: Verbosity -> Verbosity -> String -> IO ()
logv r v s = when (r >= v) $ msg s

debug = logv Debug
verbose = logv Verbose
warn = logv Warn
fatal = logv Fatal
