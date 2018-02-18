{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}

module Nodes where

import Helpers
import Types

import qualified Control.Concurrent                        as C.C
import qualified Control.Distributed.Process               as D.P
import qualified Control.Distributed.Process.Node          as D.P.N
import qualified Control.Monad.IO.Class                    as IO
import qualified Control.Monad                             as M
import qualified Data.List                                 as L
import           Data.Monoid                               ((<>))
import qualified Data.Text                                 as Tx
import qualified Network.Transport.TCP                     as N.T.TCP
import qualified System.Random                             as Sys.R

import qualified Debug.Trace as D

type MessageReceivePort
  = D.P.ReceivePort (Message Payload)

type MessageSendPort
  = D.P.SendPort (Message Payload)

data NodeWithChannels
  = NodeWithChannels
      { nwcEndpoint    :: !Endpoint
      , nwcSendPort    :: !(D.P.SendPort (Message Payload))
      , nwcReceivePort :: !(D.P.ReceivePort (Message Payload))
      }

instance Eq NodeWithChannels where
  c1 == c2 = nwcEndpoint c1== nwcEndpoint c2

instance Show NodeWithChannels where
  show (NodeWithChannels e _ _) = show e

instance Ord NodeWithChannels where
  compare c1 c2
    = compare (nwcEndpoint c1) (nwcEndpoint c2)

createNodeChannels
  :: NonEmptySet Endpoint
  -> D.P.Process (NonEmptySet NodeWithChannels)

createNodeChannels
  = traverseSet createChannel
  where
    createChannel e'
      = do
          (sp, rp) <- D.P.newChan
          pure (NodeWithChannels e' sp rp)

startProgram :: Config -> IO ()
startProgram config@Config {..}
  = do
      node <- createNode (Endpoint "127.0.0.1" 0)
      D.P.N.runProcess node $ do
        channels <- createNodeChannels cNodeEndpoints
        let sendPorts = nwcSendPort `mapSet` channels
        pIds <- IO.liftIO $ traverseSet (startNode config sendPorts) channels
        startTimestamp  <- IO.liftIO getCurrentMillis
        let startMsg = Message StartWork startTimestamp
        mapM_ (flip D.P.sendChan startMsg) sendPorts
        D.traceShowM ("Waiting for " <> show (_secondsInt cSendFor))
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cSendFor)
        D.traceShowM ("Sending StopWork" :: String)
        stopTimestamp  <- IO.liftIO getCurrentMillis
        let stopMsg = Message StopWork stopTimestamp
        mapM_ (flip D.P.sendChan stopMsg) sendPorts
        D.traceShowM ("Waiting for " <> show (_secondsInt cWaitFor))
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cWaitFor)
        D.traceShowM ("Kill all" :: String)
        mapM_ (flip D.P.kill "exit") pIds

startNode
  :: Config
  -> NonEmptySet MessageSendPort
  -> NodeWithChannels
  -> IO D.P.ProcessId

startNode config sendPorts (NodeWithChannels endpoint _ receivePort)
  = do
      node <- createNode endpoint
      D.P.N.forkProcess node $ do
        waitUntilStartMessage
        M.void $ runSender config sendPorts
        res <- runReceiver [] receivePort
        IO.liftIO (print res)

   where
     waitUntilStartMessage
       = do
           msg <- D.P.receiveChan receivePort
           case msg of
             Message StartWork _ -> pure ()
             Message _ _ -> waitUntilStartMessage

runSender :: Config -> NonEmptySet MessageSendPort -> D.P.Process D.P.ProcessId
runSender Config {..} sendPorts
  = D.P.spawnLocal $ do
      let rands = Sys.R.randomRs (0.0 :: Double, 1) (Sys.R.mkStdGen (_seedInt cSeed))
      M.void $ traverse runSender' rands

  where
    runSender' rand
      = do
          timestamp  <- IO.liftIO getCurrentMillis
          let msg = Message (RandomNumber rand) timestamp
          mapM_ (flip D.P.sendChan msg) sendPorts
          IO.liftIO $ C.C.threadDelay (secondsToMicroseconds (Seconds 1))

runReceiver
  :: [Message Double]
  -> MessageReceivePort
  -> D.P.Process ([Message Double], Double)

runReceiver msgs receivePort
  = do
      msg <- D.P.receiveChan receivePort
      case msg of
        Message (RandomNumber r) tms ->
          runReceiver (Message r tms : msgs) receivePort

        Message StopWork _ ->
          let ordered = L.sortBy
                (\(Message _ tms1) (Message _ tms2) -> compare tms1 tms2)
                msgs

              total = foldl (\r (Message r' _) -> r + r') 0 msgs

          in pure (ordered, total)

        Message _ _ -> runReceiver msgs receivePort

createNode :: Endpoint -> IO D.P.N.LocalNode
createNode endpoint@(Endpoint h p)
  = do
      ei <- N.T.TCP.createTransport (Tx.unpack h) (show p)
        (Tx.unpack h,) N.T.TCP.defaultTCPParameters

      case ei of
        Left err -> error (Tx.unpack $ "Failed to create transport for " <>
          Tx.pack (show endpoint) <> "\n" <> Tx.pack (show err))

        Right tr -> D.P.N.newLocalNode tr D.P.N.initRemoteTable
