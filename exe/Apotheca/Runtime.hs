module Apotheca.Runtime
( execRuntime
) where

import           Options.Applicative

import qualified Data.ByteString.Char8     as BC
import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

-- import           Apotheca.Encodable
import           Apotheca.Logs
import           Apotheca.Repo.Env
import           Apotheca.Repo.Internal
import           Apotheca.Repo.Path

import           Apotheca.Runtime.Commands
import           Apotheca.Runtime.Options

-- For version
import           Data.Version              (showVersion)
import           Paths_apotheca            (version)


-- TODO: APO-DIR shell environment variable to point to default repo if one
--  isn't found up-hierarchy or specified in command line.


execRuntime :: IO ()
execRuntime = do
  hSetBuffering stdout LineBuffering
  opts <- getOptions
  case optCommand opts of
    -- No command, default to help
    NoCommand -> do
      -- Show help
      withArgs ["--help"] getOptions
      return ()
    -- Non-repo commands
    -- TODO: Include git commit hash in version
    --  https://haskell-lang.org/library/optparse-applicative
    --  Under "Package version number and Git commit ID"
    Version -> putStrLn $ showVersion version
    -- Env/repo commands
    cmd -> buildEnv opts >>= runCommand cmd

buildEnv :: RuntimeOptions -> IO Env
buildEnv opts = do
    verbose v "Building environment..."
    -- cwd <- getCurrentDirectory
    cwd <- getCurrentDirectory >>= fixPath
    --
    -- sd <- buildStoreDir opts cwd
    sd <- buildStoreDir opts cwd >>= fixPath
    --
    -- let ewd = fromMaybe cwd $ optExtDir opts
    ewd <- fixPath . fromMaybe cwd $ optExtDir opts
    --
    iwd <- case optIntDir opts of
      Just i -> return i
      Nothing -> buildIntDir opts sd ewd
    -- Done
    return defaultEnv
      { repoDir = sd
      , extDir = ewd
      , intDir = iwd
      , magicSlash = optMagicSlash opts
      , verbosity = v
      , masterSecret = BC.pack <$> optMasterSecret opts
      }
  where
    v = getVerb opts
    -- NOTE: Should we absolutize dirs?
    -- fixPath p = normalise <$> makeAbsolute p
    -- fixPath p = return $ normalise p
    fixPath p = do
      a <- doesFileExist p
      b <- doesDirectoryExist p
      if a || b
        then canonicalizePath p
        else error $ "Path does not exist: " ++ p

buildStoreDir :: RuntimeOptions -> FilePath -> IO FilePath
buildStoreDir opts cwd = do
    case optSearchDir opts of
      Just sd -> do
        exists <- doesRepoExist sd
        return sd
      Nothing -> do
        verbose v "Finding repo..."
        fr <- findRepo cwd
        case fr of
          Just fr' -> debug v $ "Found repo: " ++ show fr'
          Nothing -> debug v "Repo not found (defaulting to cwd)."
        return $ fromMaybe cwd fr
  where
    v = getVerb opts

-- NOTE: ".." in STOREDIR or EXTDIR breaks INTDIR magic
-- TODO: Warn users about needing explicit INTDIR if STOREDIR or EXTDIR contain ".."
buildIntDir :: RuntimeOptions -> FilePath -> FilePath -> IO Path
buildIntDir opts sd ewd = do
  needsMagic <- doesHiddenRepoExist sd
  case (needsMagic, i == ewd) of
    (True, False) -> do
      debug v $ "Applied INTDIR path magic: " ++ toFilePath iwd
      return iwd
    (True, True) -> do
      debug v "Path magic cancelled - not within repo."
      return []
    _ -> do
      debug v "Path magic cancelled - no repo found."
      return []
  where
    v = getVerb opts
    i = makeRelative sd ewd
    iwd = fromFilePath i

getVerb :: RuntimeOptions -> Verbosity
getVerb = fromMaybe Terse . optVerbosity
