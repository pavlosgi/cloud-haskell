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
import           Data.Maybe                                as Mb
import           Data.Monoid                               ((<>))
import qualified Data.Text                                 as Tx
import qualified Network.Transport.TCP                     as N.T.TCP
import qualified System.Random                             as Sys.R

type MessageSendPort
  = D.P.SendPort Message

type MessageReceivePort
  = D.P.ReceivePort Message

data NodeWithChannels
  = NodeWithChannels
      { nwcEndpoint    :: !Endpoint
      , nwcSendPort    :: !MessageSendPort
      , nwcReceivePort :: !MessageReceivePort
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
        startMsg <- IO.liftIO $ createMessage StartWork
        mapM_ (flip D.P.sendChan startMsg) sendPorts
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cSendFor)
        stopMsg <- IO.liftIO $ createMessage StopWork
        mapM_ (flip D.P.sendChan stopMsg) sendPorts
        IO.liftIO $ C.C.threadDelay (secondsToMicroseconds cWaitFor)
        mapM_ (flip D.P.kill "Grace period is over") pIds

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
        senderPId <- runSender config sendPorts
        res <- runReceiver [] receivePort senderPId
        IO.liftIO (print res)

   where
     waitUntilStartMessage
       = do
           msg <- D.P.receiveChan receivePort
           case msg of
             Message StartWork _ -> pure ()
             Message {} -> waitUntilStartMessage

runSender
  :: Config
  -> NonEmptySet MessageSendPort
  -> D.P.Process D.P.ProcessId

runSender Config {..} sendPorts
  = D.P.spawnLocal $ do
      let rands = Sys.R.randomRs (0.0 :: Double, 1)
                    (Sys.R.mkStdGen (_seedInt cSeed))

      M.void $ traverse runSender' rands

  where
    runSender' rand
      = do
          msg <- IO.liftIO $ createMessage (RandomNumber rand)
          mapM_ (\s -> D.P.sendChan s msg) sendPorts
          IO.liftIO (C.C.threadDelay 500)

runReceiver
  :: [Message]
  -> MessageReceivePort
  -> D.P.ProcessId
  -> D.P.Process ([Double], Double)

runReceiver msgs receivePort senderPId
  = do
      msg <- D.P.receiveChan receivePort
      case msg of
        Message (RandomNumber _) _ ->
          runReceiver (msg : msgs) receivePort senderPId

        Message StopWork _ -> do
          D.P.kill senderPId "Stop work received"
          msgs' <- receiveAnyMessagesLeft []
          let filtered = Mb.catMaybes (f <$> msgs <> msgs')
              f (Message (RandomNumber r) tms) = Just (tms, r)
              f _ = Nothing

              ordered = L.sortBy (\(tms1, _) (tms2, _) -> compare tms1 tms2) filtered
              total = foldl sumNums 0 filtered
              sumNums r (_, d) = r + d

          pure (fmap snd ordered, total)

        _ ->
          runReceiver msgs receivePort senderPId

  where
    receiveAnyMessagesLeft msgsReceived
      = do
          r <- D.P.receiveChanTimeout 0 receivePort
          case r of
            Just msg -> receiveAnyMessagesLeft (msg : msgsReceived)
            Nothing -> pure msgsReceived

createMessage :: Payload -> IO Message
createMessage pl
  = do
      tms <- getCurrentMillis
      pure (Message pl tms)

createNode :: Endpoint -> IO D.P.N.LocalNode
createNode endpoint@(Endpoint h p)
  = do
      ei <- N.T.TCP.createTransport (Tx.unpack h) (show p)
        (Tx.unpack h,) N.T.TCP.defaultTCPParameters

      case ei of
        Left err -> error (Tx.unpack $ "Failed to create transport for " <>
          Tx.pack (show endpoint) <> "\n" <> Tx.pack (show err))

        Right tr -> D.P.N.newLocalNode tr D.P.N.initRemoteTable
