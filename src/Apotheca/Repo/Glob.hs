module Apotheca.Repo.Glob where

import qualified Data.List            as L

import qualified System.FilePath.Glob as G

type Pattern = G.Pattern

gcompile :: String -> Pattern
gcompile = G.simplify . G.compile

gmatch :: Pattern -> FilePath -> Bool
gmatch = G.match

glob :: Pattern -> [FilePath] -> [FilePath]
glob pat = fst . globPart pat

globPart :: Pattern -> [FilePath] -> ([FilePath],[FilePath])
globPart pat fs = L.partition (gmatch pat) fs

globNot :: Pattern -> [FilePath] -> [FilePath]
globNot pat = snd . globPart pat

globM :: (Monad m) => (Pattern -> [FilePath] -> a) -> Pattern -> m [FilePath] -> m a
globM g pat paths  = g pat <$> paths
