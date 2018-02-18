{-# LANGUAGE RecordWildCards #-}

module Main where

import CliParser
import Nodes
import Types

import qualified Data.Validation as V

main :: IO ()
main
  = do
      vc <- parseArgs
      case vc of
        V.AccFailure err -> print err
        V.AccSuccess config@Config {..} ->
          startProgram config
