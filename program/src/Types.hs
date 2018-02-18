{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Config (..)
  , Endpoint (..)
  , File (..)
  , Seconds (..)
  , Seed (..)
  , ConfigError (..)
  , NonEmptySet
  , Message (..)
  , Payload (..)
  ) where

import qualified Data.Binary                 as B
import qualified Data.NonEmpty               as NE
import qualified Data.Set                    as Set
import qualified Data.Text                   as Tx
import           GHC.Generics                (Generic)

data Config
  = Config
      { cNodeEndpoints :: !(NonEmptySet Endpoint)
      , cSendFor       :: !Seconds
      , cWaitFor       :: !Seconds
      , cSeed          :: !Seed
      }

  deriving (Eq, Show)

data Endpoint
  = Endpoint
      { host :: !Tx.Text
      , port :: !Int
      }

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
  | InvalidEndpoint !Tx.Text
  deriving (Eq, Show)

type NonEmptySet a
  = NE.T Set.Set a

data Message
  = Message
      { mPayload :: !Payload
      , mSentAt  :: !Integer
      }

  deriving (Eq, Generic, Show)

instance B.Binary Message

data Payload
  = StartWork
  | StopWork
  | RandomNumber !Double
  deriving (Eq, Generic, Show)

instance B.Binary Payload
