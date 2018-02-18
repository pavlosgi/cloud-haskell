module Main where

import CliParser
import Nodes

main :: IO ()
main
  = do
      config <- parseArgsOrFail
      startProgram config
