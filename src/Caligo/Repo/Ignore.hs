module Caligo.Repo.Ignore
( Ignore (..)
, ignore
, ignoreName
, readIgnoreFile, writeIgnoreFile
, parseIgnore
, doesIgnore
) where

import qualified Data.ByteString.Char8 as BC
import           Data.Maybe

import qualified System.FilePath.Glob  as G

import           Caligo.Misc
import           Caligo.Repo.Types     (Ignore (..))

-- TODO: More complex globbing

ignoreName = ".calignore"

ignore :: [String] -> Ignore
ignore = map (\a -> (a, G.simplify $ G.compile a))

-- Ignore file must contain no extra space, be only patterns and newlines
readIgnoreFile :: FilePath -> IO Ignore
readIgnoreFile p = parseIgnore . BC.unpack <$> BC.readFile p

writeIgnoreFile :: FilePath -> Ignore -> IO ()
writeIgnoreFile p = writeFile p . unlines . map fst

parseIgnore :: String -> Ignore
parseIgnore = ignore . lines

doesIgnore :: Ignore -> FilePath -> Bool
doesIgnore i p = any ((G.match $- p) . snd) i