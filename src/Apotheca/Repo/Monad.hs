module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

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

printRIO :: (Show a) => a -> RIO ()
printRIO = io . print



