module Apotheca.Backend
( BackendConfig (..)
, localhost
, bootProcess
, testMain
) where

import qualified Data.ByteString.Char8            as BC
import           Data.List                        (intercalate)

import           Control.Concurrent               (threadDelay)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Monad                    (forever)

import           Network.Socket                   (HostName, ServiceName,
                                                   withSocketsDo)
import           Network.Transport                (EndPointAddress (..),
                                                   Transport)
import           Network.Transport.TCP            (createTransport,
                                                   defaultTCPParameters,
                                                   encodeEndPointAddress)

import           System.Environment               (getArgs)


encodeNodeId :: HostName -> ServiceName -> NodeId
encodeNodeId host port = NodeId $ encodeEndPointAddress host port 0

data BackendConfig = BackendConfig
  { host  :: HostName
  , port  :: ServiceName
  , peers :: [NodeId] -- Bootstrap connection list
  }

localhost :: Int -> BackendConfig
localhost p = BackendConfig
  { host = "127.0.0.1"
  , port = show p
  , peers = []
  }

publichost :: HostName -> ServiceName -> BackendConfig
publichost h p = BackendConfig
  { host = h
  , port = p
  , peers = []
  }

bootProcess :: BackendConfig -> RemoteTable -> Process () -> IO ()
bootProcess np rt proc = withSocketsDo $ do
  node <- bootLocalNode np rt
  runProcess node proc

bootLocalNode :: BackendConfig -> RemoteTable -> IO LocalNode
bootLocalNode np rt = do
    transport <- either (error . show) id
      <$> createTransport (host np) (port np) defaultTCPParameters
    newLocalNode transport rt


-- Testing

testMain :: IO ()
testMain = do
  args <- getArgs
  (host, port) <- case args of
    [host, port] -> return $ (host, port)
    _ -> error "Arguments must be: hostname servicename key"
  bootProcess (publichost host port) initRemoteTable $ do
    selfNid <- getSelfNode
    say $ "Self: " ++ show selfNid
    _ <- expectTimeout 1000000 :: Process (Maybe String)
    _ <- liftIO $ getChar
    liftIO $ putStrLn "Done."


-- testMain :: IO ()
-- testMain = bootProcess (localhost 10501) initRemoteTable $ do
--   selfPid <- getSelfPid
--   selfNid <- getSelfNode
--   send selfPid "hello"
--   receivedStr <- expect :: Process String
--   liftIO . putStrLn $ "Received string: " ++ receivedStr
