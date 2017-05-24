module Apotheca.Repo.Path
( Path (..)
, PathElement (..)
, toFilePath
, toGlobPath
, fromFilePath
, root
, parent
) where

import           Apotheca.Repo.Internal
import           System.FilePath


-- NOTE: Path is:
--  1) always absolute
--  2) stringly-typed for now
-- TODO: Strongly-typed path elements
--  newtype Path = Path { unpath :: [PathElement] }
--  data PathElement = RootElem | DirElem String | FileElem String | TrailingSep
-- TODO: Path<->String parsing
--  show (RootElem "drive" <::/> DirElem "dir" </> DirElem "other" </:> FileElem "file")
--    == "drive::/dir/dir2/:file"

toFilePath :: Path -> FilePath
toFilePath [] = [pathSeparator]
toFilePath p = normalise $ [pathSeparator] ++ joinPath p -- L.intercalate "/"

-- A glob-friendly toFilePath
--  Path is currently absolute, so toFilePath always starts with slash, which
--  interferes with globbing.
toGlobPath :: Path -> FilePath
toGlobPath = dropDrive . toFilePath

fromFilePath :: FilePath -> Path
fromFilePath [] = []
fromFilePath p = splitDirectories . dropDrive $ normalise p

root :: Path
root = []

parent :: Path -> Path
parent [] = error "Cannot get root of parent"
parent p = init p

-- -- ...dir/dir - Append a dir onto an existing path
(</>) :: Path -> PathElement -> Path
a </> b = a ++ [b]

-- -- root::/elem -- only makes sense for named roots
-- (<::/>) :: Path -> PathElement -> Path
-- a <::/> b = undefined

-- -- ...elem/file -- Append a file onto existing path
-- (</:>) :: Path -> PathElement -> Path
-- a </:> b = undefined
