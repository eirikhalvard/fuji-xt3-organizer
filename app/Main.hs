module Main where

import Cli
import Lib
import Types

main :: IO ()
main = do
  command <- parseCommand
  env <- getEnv (getName command)
  runCommand command env
