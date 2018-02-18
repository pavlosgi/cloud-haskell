{-# LANGUAGE OverloadedStrings #-}

module Helpers
  ( endpointToText
  , secondsToMicroseconds
  , getCurrentMillis
  , endpointToNodeId
  , traverseSet
  , mapSet
  ) where

import Types

import qualified Control.Distributed.Process    as D.P
import           Data.Monoid                    ((<>))
import qualified Data.NonEmpty                  as NE
import qualified Data.Set                       as Set
import qualified Data.Text                      as Tx
import qualified Data.Text.Encoding             as Tx.E
import qualified Data.Time.Clock.POSIX          as T.C.POSIX
import qualified Network.Transport              as N.T

endpointToText :: Endpoint -> Tx.Text
endpointToText (Endpoint h p)
  = h <> ":" <> Tx.pack (show p)

secondsToMicroseconds :: Seconds -> Int
secondsToMicroseconds (Seconds s)
  = s * 1000000

getCurrentMillis :: IO Integer
getCurrentMillis
  = round . (* 1000) <$> T.C.POSIX.getPOSIXTime

endpointToNodeId :: Endpoint -> D.P.NodeId
endpointToNodeId e
  = D.P.NodeId (N.T.EndPointAddress (Tx.E.encodeUtf8 $ (endpointToText e <> ":0")))

traverseSet
  :: (Ord b, Applicative m)
  => (a -> m b)
  -> NonEmptySet a
  -> m (NonEmptySet b)

traverseSet f s
  = let traverseList = traverse f (NE.mapTail Set.toList s)
    in  fmap (NE.mapTail Set.fromList) traverseList

mapSet :: Ord b => (a -> b) -> NonEmptySet a -> NonEmptySet b
mapSet f ne
  = let (h, t) = NE.viewL ne
    in NE.cons (f h) (Set.map f t)
