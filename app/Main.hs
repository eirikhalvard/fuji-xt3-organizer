module Main where

import Lib
import Types
import Cli

main :: IO ()
main = do
  command <- parseCommand
  env <- getEnv (getName command)
  runCommand command env
