{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import CliParser
import Nodes
import Types

import qualified Control.Monad   as M
import qualified Data.NonEmpty   as NE
import qualified Data.Validation as V

main :: IO ()
main
  = do
      vc <- parseArgs
      case vc of
        V.AccFailure err -> print err
        V.AccSuccess config@Config {..} -> do
          pIds <- traverse (startNode config) (NE.toList cNodeEndpoints)
          M.void $ startCoordinatorNode pIds config (Endpoint "127.0.0.1" 0)
