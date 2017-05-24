module Apotheca.Repo.Glob where

import qualified Data.List            as L

import qualified System.FilePath.Glob as G

type Pattern = G.Pattern

gcompile :: String -> Pattern
gcompile = G.simplify . G.compile

gmatch :: Pattern -> FilePath -> Bool
gmatch = G.match

glob :: Pattern -> [FilePath] -> [FilePath]
glob = globT id

globPart :: Pattern -> [FilePath] -> ([FilePath],[FilePath])
globPart = globPartT id

globNot :: Pattern -> [FilePath] -> [FilePath]
globNot = globNotT id

globT :: (a -> FilePath) -> Pattern ->  [a] -> [a]
globT t pat = fst . globPartT t pat

globPartT :: (a -> FilePath) -> Pattern -> [a] -> ([a],[a])
globPartT t pat = L.partition (gmatch pat . t)

globNotT :: (a -> FilePath) -> Pattern -> [a] -> [a]
globNotT t pat = snd . globPartT t pat

globM :: (Monad m) => (Pattern -> [FilePath] -> a) -> Pattern -> m [FilePath] -> m a
globM g pat paths  = g pat <$> paths
