module Apotheca.Repo.Monad where

import           Control.Monad.State.Lazy

import           Apotheca.Repo.Internal



-- Monad

type RIO a = StateT Repo IO a

runRIO :: RIO a -> Repo -> IO (a, Repo)
runRIO = runStateT
evalRIO :: RIO a -> Repo -> IO a
evalRIO = evalStateT
execRIO :: RIO a -> Repo -> IO Repo
execRIO = execStateT
withRIO :: (Repo -> Repo) -> RIO a -> RIO a
withRIO = withStateT

-- Later
-- type RepoM m a = StateT Repo m a
-- type RIO a = RepoM IO a
-- mapRepoM :: (m (a, Repo) -> n (b, Repo)) -> StateT Repo m a -> StateT Repo n b
-- mapRepoM = mapStateT

io :: IO a -> RIO a
io = liftIO

printRIO :: (Show a) => a -> RIO ()
printRIO = io . print



-- get :: Monad m => StateT s m s
-- put :: Monad m => s -> StateT s m ()
-- modify :: Monad m => (s -> s) -> StateT s m ()
-- modify' :: Monad m => (s -> s) -> StateT s m ()
-- gets :: Monad m => (s -> a) -> StateT s m a
