module Apotheca.Repo.Ignore
( Ignore (..)
, ignore
, readIgnore, writeIgnore
, parseIgnore
, doesIgnore
) where

import qualified Data.ByteString.Char8  as BC
import           Data.Maybe

import qualified System.FilePath.Glob   as G

import           Apotheca.Misc
import           Apotheca.Repo.Internal (Ignore (..))

-- TODO: More complex globbing

ignore :: [String] -> Ignore
ignore = map (\a -> (a, G.simplify $ G.compile a))

-- Ignore file must contain no extra space, be only patterns and newlines
readIgnore :: FilePath -> IO Ignore
readIgnore p = parseIgnore . BC.unpack <$> BC.readFile p

writeIgnore :: FilePath -> Ignore -> IO ()
writeIgnore p = writeFile p . unlines . map fst

parseIgnore :: String -> Ignore
parseIgnore = ignore . lines

doesIgnore :: Ignore -> FilePath -> Bool
doesIgnore i p = any ((G.match $- p) . snd) i
