module Caligo.Process
( delay
) where

import           Control.Distributed.Process

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (forever)

-- Easy lifted IO -> Process stuff
delay :: Int -> Process ()
delay nanos = liftIO $ threadDelay nanos
