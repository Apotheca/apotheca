module Apotheca.Repo.MVar where

import           Control.Concurrent.MVar

import           Data.Tuple

import           Apotheca.Repo.Internal
import           Apotheca.Repo.Monad



-- NOTE: The conventions for MVar and StateT are somewhat inverted with respect to each other
-- StateT is ordered state-last and returns (value, state) a la:
--  (s -> (a, s)) -> s -> (a, s)
-- MVar is ordered state-first and returns (state, value) a la:
--  s -> (s -> (s, a)) -> a
-- See: modifyMRepo



type MRepo = MVar Repo



-- RIO to MVar+IO - perform a RIO action from an MVar+IO environment

newMRepo :: Repo -> IO MRepo
newMRepo = newMVar

-- withMVar :: MVar a -> (a -> IO b) -> IO b
withMRepo :: MRepo -> RIO a -> IO a
withMRepo mr = withMVar mr . evalRM

-- modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMRepo :: MRepo -> RIO a -> IO a
modifyMRepo mr = modifyMVar mr . swapped
  where swapped f r = swap <$> runRM f r

-- modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMRepo_ :: MRepo -> RIO () -> IO ()
modifyMRepo_ mr = modifyMVar_  mr . execRM

-- MVar+IO to RIO - perform an MVar+IO action from a RIO environment?
