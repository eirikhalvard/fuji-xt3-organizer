module Main where

import qualified Brick.BChan as BC
import Cli
import Lib
import Types

main :: IO ()
main = do
  command <- parseCommand
  eventChan <- BC.newBChan 10
  env <- getEnv (getNameType command) eventChan
  runCommand command env
