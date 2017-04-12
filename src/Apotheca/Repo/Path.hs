module Apotheca.Repo.Path
( Path (..)
, PathElement (..)
, toFilePath
, fromFilePath
, root
, parent
) where

import           Apotheca.Repo.Types
import           System.FilePath


-- NOTE: Path is:
--  1) always absolute
--  2) stringly-typed for now
-- TODO: Strongly-typed path elements
--  newtype Path = Path { unpath :: [PathElement] }
--  data PathElement = Root | Dir String | File String | TrailingSep
-- TODO: Path<->String parsing
--  show (Root "drive" <::/> Dir "dir" </> Dir "other" </:> File "file")
--    == "drive::/dir/dir2/:file"

toFilePath :: Path -> FilePath
toFilePath [] = [pathSeparator]
toFilePath p = normalise $ [pathSeparator] ++ joinPath p -- L.intercalate "/"
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
