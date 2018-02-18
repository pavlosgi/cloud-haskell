{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Nodes where

import Types

import qualified Control.Concurrent                        as C.C
import qualified Control.Distributed.Process               as D.P
import qualified Control.Distributed.Process.Node          as D.P.N
import qualified Control.Monad.IO.Class                    as IO
import qualified Control.Monad                             as M
import qualified Data.List                                 as L
import           Data.Monoid                               ((<>))
import qualified Data.Text                                 as Tx
import qualified Data.Text.Encoding                        as Tx.E
import qualified Data.Time.Clock.POSIX                     as T.C.POSIX
import qualified Network.Transport.TCP                     as N.T.TCP
import qualified Network.Transport                         as N.T
import qualified System.Random                             as Sys.R

import qualified Debug.Trace as D

endpointToNodeId :: Endpoint -> D.P.NodeId
endpointToNodeId e
  = D.P.NodeId (N.T.EndPointAddress (Tx.E.encodeUtf8 $ (endpointToText e <> ":0")))

startCoordinatorNode :: [D.P.ProcessId] -> Config -> Endpoint -> IO ()
startCoordinatorNode pIds Config {..} endpoint
  = do
      node <- createNode endpoint
      D.P.N.runProcess node $ do
        D.traceShowM ("Waiting for " <> show (_secondsInt cSendFor))
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cSendFor)
        D.traceShowM ("Sending StopWork" :: String)
        timestamp  <- IO.liftIO $ round . (* 1000) <$> T.C.POSIX.getPOSIXTime
        let msg = Message StopWork timestamp
        mapM_ (\e -> D.P.nsendRemote (endpointToNodeId e)
          (Tx.unpack (endpointToText e)) msg) cNodeEndpoints

        D.traceShowM ("Waiting for " <> show (_secondsInt cWaitFor))
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cWaitFor)
        D.traceShowM ("Kill all" :: String)
        mapM_ (flip D.P.kill "exit") pIds

startNode :: Config -> Endpoint -> IO D.P.ProcessId
startNode config endpoint
  = do
      node <- createNode endpoint
      D.P.N.forkProcess node $ do
        pid <- D.P.getSelfPid
        D.P.register (Tx.unpack $ endpointToText endpoint) pid
        M.void $ runSender config
        res <- runReceiver []
        IO.liftIO (print res)

runSender :: Config -> D.P.Process D.P.ProcessId
runSender Config {..}
  = D.P.spawnLocal $ do
      let rands = Sys.R.randomRs (0.0 :: Double, 1) (Sys.R.mkStdGen (_seedInt cSeed))
      M.void $ traverse (runSender' cNodeEndpoints) rands

  where
    runSender' endpoints rand
      = do
          timestamp  <- IO.liftIO $ round . (* 1000) <$> T.C.POSIX.getPOSIXTime
          let msg = Message (RandomNumber rand) timestamp
          mapM_ (\e -> D.P.nsendRemote (endpointToNodeId e)
            (Tx.unpack (endpointToText e)) msg) endpoints

          IO.liftIO $ C.C.threadDelay (secondsToMicroseconds (Seconds 1))

runReceiver :: [Message Double] -> D.P.Process ([Message Double], Double)
runReceiver msgs
  = do
      msg <- D.P.expect
      case msg of
        Message (RandomNumber r) tms -> runReceiver (Message r tms : msgs)
        Message StopWork _ ->
          let ordered = L.sortBy
                (\(Message _ tms1) (Message _ tms2) -> compare tms1 tms2)
                msgs

              total = foldl (\r (Message r' _) -> r + r') 0 msgs

          in pure (ordered, total)

createNode :: Endpoint -> IO D.P.N.LocalNode
createNode endpoint@(Endpoint h p)
  = do
      ei <- N.T.TCP.createTransport (Tx.unpack h) (show p)
        (Tx.unpack h,) N.T.TCP.defaultTCPParameters

      case ei of
        Left err -> error (Tx.unpack $ "Failed to create transport for " <>
          Tx.pack (show endpoint) <> "\n" <> Tx.pack (show err))

        Right tr -> D.P.N.newLocalNode tr D.P.N.initRemoteTable
