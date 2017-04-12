module Apotheca.Runtime
( execRuntime
) where

import           Options.Applicative

import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO

-- import           Apotheca.Encodable
import           Apotheca.Logs
import           Apotheca.Repo.Path
import           Apotheca.Repo.Repo

import           Apotheca.Runtime.Commands
import           Apotheca.Runtime.Options


-- TODO: CAL-DIR shell environment variable to point to default repo if one
--  isn't found up-hierarchy or specified in command line.


execRuntime :: IO ()
execRuntime = do
  hSetBuffering stdout LineBuffering
  opts <- getOptions
  case optCommand opts of
    NoCommand -> do
      -- Show help
      withArgs ["--help"] getOptions
      return ()
    cmd -> buildEnv opts >>= runCommand cmd

buildEnv :: RuntimeOptions -> IO Env
buildEnv opts = do
    verbose v "Building environment..."
    -- cwd <- getCurrentDirectory
    cwd <- getCurrentDirectory >>= fixPath
    debug v $ "Cwd is: " ++ cwd
    --
    -- sd <- buildStoreDir opts cwd
    sd <- buildStoreDir opts cwd >>= fixPath
    debug v $ "STOREDIR set: " ++ sd
    --
    -- let ewd = fromMaybe cwd $ optExtDir opts
    ewd <- fixPath . fromMaybe cwd $ optExtDir opts
    debug v $ "EXTDIR set: " ++ ewd
    --
    iwd <- case optIntDir opts of
      Just i -> return i
      Nothing -> buildIntDir opts sd ewd
    debug v $ "INTDIR set: " ++ toFilePath iwd
    -- Done
    verbose v "Environment constructed!"
    return defaultEnv
      { repoDir = sd
      , extDir = ewd
      , intDir = iwd
      , magicSlash = optMagicSlash opts
      , verbosity = v
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
    verbose v "Finding store dir..."
    case optSearchDir opts of
      Just sd -> do
        exists <- doesRepoExist sd
        debug v $ "Specified STOREDIR: " ++ sd
        debug v $ "Exists? " ++ show exists
        return sd
      Nothing -> do
        debug v "Unspecified STOREDIR, searching upwards..."
        fr <- findRepo cwd
        case fr of
          Just fr' -> debug v $ "Found STOREDIR: " ++ show fr'
          Nothing -> debug v "STOREDIR not found; defaulting to cwd"
        return $ fromMaybe cwd fr
  where
    v = getVerb opts

-- NOTE: ".." in STOREDIR or EXTDIR breaks INTDIR magic
-- TODO: Warn users about needing explicit INTDIR if STOREDIR or EXTDIR contain ".."
buildIntDir :: RuntimeOptions -> FilePath -> FilePath -> IO Path
buildIntDir opts sd ewd = do
  verbose v "Applying magic..."
  needsMagic <- doesHiddenRepoExist sd
  case (needsMagic, i == ewd) of
    (True, False) -> do
      debug v $ "Applied INTDIR magic: " ++ toFilePath iwd
      return iwd
    (True, True) -> do
      debug v "Magic cancelled - not within repo."
      return []
    _ -> do
      debug v "Magic cancelled - no repo found."
      return []
  where
    v = getVerb opts
    i = makeRelative sd ewd
    iwd = fromFilePath i

getVerb :: RuntimeOptions -> Verbosity
getVerb = fromMaybe Warn . optVerbosity
