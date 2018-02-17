module Node
  ( startNode
  ) where

import Types

{-import qualified Network.Transport.TCP as N.T.TCP-}
import qualified Data.NonEmpty as NE
import qualified Data.Set      as Set

startNode :: NonEmptySet Endpoint -> Endpoint -> IO ()
startNode allEndpoints nodeEndpoint
  = do
      print nodeEndpoint
      print otherEndpoints

  where
    toSet a
      = let (x, xs) = NE.viewL a
        in  Set.insert x xs

    otherEndpoints
      = Set.delete nodeEndpoint (toSet allEndpoints)
