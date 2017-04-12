module Apotheca.Chord where

import qualified Data.Map.Strict             as M

import           Apotheca.Distributed.Keyspace

data ChordState a = CState
  { self        :: Key
  , predecessor :: Key
  , successors  :: [Key]
  , keyMap      :: M.Map Key a
  }

-- data ChordConfig = CConfig
--   { keyspace :: Keyspace
--   , blockDir :: FilePath -- Block storage directory
--   , blockSz  :: Int -- Size of each block in bytes
--   , succCt   :: Int -- Successor count
--   -- , replCt     :: Int -- Replication count, <= successor count
--   , timeout  :: Int-- Global request timeout in milliseconds
--   }

successor :: ChordState a -> Maybe a
successor st = successorN 1 st

successorN :: Int -> ChordState a -> Maybe a
successorN n st = undefined
