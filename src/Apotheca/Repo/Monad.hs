module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

import           Apotheca.Logs            (Verbosity (..), logM)
import           Apotheca.Repo.Config
import           Apotheca.Repo.Env
import           Apotheca.Repo.Ignore
import           Apotheca.Repo.Internal
import qualified Apotheca.Repo.Manifest   as Mf



-- Monad

type RM m a = StateT Repo m a
type RIO a = RM IO a

runRM :: (Monad m) => RM m a -> Repo -> m (a, Repo)
runRM = runStateT
evalRM :: (Monad m) => RM m a -> Repo -> m a
evalRM = evalStateT
execRM :: (Monad m) => RM m a -> Repo -> m Repo
execRM = execStateT
withRM :: (Monad m) => (Repo -> Repo) -> RM m a -> RM m a
withRM = withStateT
mapRM :: (Monad m, Monad n) =>
  (m (a, Repo) -> n (b, Repo)) -> StateT Repo m a -> StateT Repo n b
mapRM = mapStateT

getRM :: (Monad m) => RM m Repo
getRM = get
putRM :: (Monad m) => Repo -> RM m ()
putRM = put
modifyRM :: (Monad m) => (Repo -> Repo) -> RM m ()
modifyRM = modify
selectRM :: (Monad m) => (Repo -> a) -> RM m a
selectRM = gets



-- io lifter

io :: IO a -> RIO a
io = liftIO



-- Logging

getVerbosity :: (Monad m) => RM m Verbosity
getVerbosity = verbosity <$> getEnv

logRIO :: Verbosity -> String -> RIO ()
logRIO v s = do
  v' <- getVerbosity
  logM v v' s

debug = logRIO Debug
verbose = logRIO Verbose
terse = logRIO Terse
warn = logRIO Warn
fatal = logRIO Fatal



-- Errors

errorIf :: (Monad m) => Bool -> String -> m ()
errorIf b = when b . error

errorWhen :: (Monad m) => m Bool -> String -> m ()
errorWhen f e = f >>= flip errorIf e
errorUnless f = errorWhen (not <$> f)



-- Repo convenience

getEnv :: (Monad m) => RM m Env
getEnv = selectRM repoEnv

getConfig :: (Monad m) => RM m Config
getConfig = selectRM repoConfig

getManifest :: (Monad m) => RM m Manifest
getManifest = selectRM repoManifest

getIgnore :: (Monad m) => RM m Ignore
getIgnore = selectRM repoIgnore



-- Env lifters

modifyEnv :: (Monad m) => (Env -> Env) -> RM m ()
modifyEnv f = do
  r <- getRM
  putRM $ r { repoEnv = f $ repoEnv r }

queryEnv :: (Monad m) => (Env -> a) -> RM m a
queryEnv f = f <$> getEnv

-- Config lifters

modifyConfig :: (Monad m) => (Config -> Config) -> RM m ()
modifyConfig f = do
  r <- getRM
  putRM $ r { repoConfig = f $ repoConfig r }

queryConfig :: (Monad m) => (Config -> a) -> RM m a
queryConfig f = f <$> getConfig

-- Manifest lifters

modifyManifest :: (Monad m) => (Manifest -> Manifest) -> RM m ()
modifyManifest f = do
  r <- getRM
  putRM $ r { repoManifest = f $ repoManifest r }

queryManifest :: (Monad m) => (Manifest -> a) -> RM m a
queryManifest f = f <$> getManifest




