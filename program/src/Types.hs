module Types
  ( Config (..)
  , Endpoint (..)
  , File (..)
  , Seconds (..)
  , Seed (..)
  , ConfigError (..)
  , NonEmptySet
  ) where

import qualified Data.NonEmpty    as NE
import qualified Data.Set         as Set
import qualified Data.Text        as Tx

data Config
  = Config
      { cNodeEndpoints :: !(NonEmptySet Endpoint)
      , cSendFor       :: !Seconds
      , cWaitFor       :: !Seconds
      , cWithSeed      :: !Seed
      }

  deriving (Eq, Show)

newtype Endpoint
  = Endpoint { _endpointText :: Tx.Text }
  deriving (Eq, Ord, Show)

newtype File
  = File { _fileText :: Tx.Text }
  deriving (Eq, Show)

newtype Seconds
  = Seconds { _secondsInt :: Int }
  deriving (Eq, Show)

newtype Seed
  = Seed { _seedInt :: Int }
  deriving (Eq, Show)

data ConfigError
  = ArgumentWithNameDoesNotExist !Tx.Text
  | NoValueForArgument !Tx.Text
  | FileDoesNotExist !File
  | InvalidSeconds !Tx.Text
  | EmptyArgumentList
  | EmptyNodesConfig !File
  deriving (Eq, Show)

type NonEmptySet a
  = NE.T Set.Set a
