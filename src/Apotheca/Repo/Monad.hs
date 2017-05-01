module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

import           Apotheca.Logs            (Verbosity (..), logM)
import           Apotheca.Repo.Internal



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




