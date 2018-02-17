{-# LANGUAGE RecordWildCards   #-}

module Main where

import CommandLineParser
import Node
import Types

import qualified Control.Concurrent as C.C
import qualified Data.Validation    as V

main :: IO ()
main
  = do
      vc <- parseArgs
      case vc of
        V.AccFailure err -> print err
        V.AccSuccess c@Config {..} -> do
          mapM_ (C.C.forkIO . startNode cNodeEndpoints) cNodeEndpoints
          print c
