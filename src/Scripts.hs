module Scripts where

import Paths_fuji (getDataFileName)
import Types
import Gui
import Data.List.Extra (chunksOf, dropPrefix, splitOn)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.Process.Extra (readProcess)
import Control.Exception (try, SomeException)
import System.Exit (exitWith, ExitCode(..))

-- functions --

runCreateFolder :: String -> Env -> IO [String]
runCreateFolder folderName env = runScript env "createFolder" [folderName] parseApplescriptLog


runLogPhotos :: Env -> IO (Map String (Set String))
runLogPhotos env = runScript env "logphotos" [] parseLoggedPhotos

runTransferPhotos :: String -> String -> Env -> IO [String]
runTransferPhotos folderName transferFolder env = runScript env "transferPhotos" [folderName, transferFolder] parseApplescriptLog

-- runners --

runScript :: Env -> String -> [String] -> (String -> parsed) -> IO parsed
runScript env scriptName inputParameters parser = do
    logEvent env "running script"
    scriptPath <- getScriptPath scriptName
    readProcess "/usr/bin/osascript" (scriptPath : inputParameters) ""
    response <- try (readProcess "/usr/bin/osascript" (scriptPath : inputParameters) "") :: IO (Either SomeException String)
    case response of
      Left err -> do
        logEvent env "some error occured"
        logEvent env (show err)
        event env Finished
        exitWith (ExitFailure 1)
      Right r -> do
        logEvent env "successfully executed script"
        return $ parser r

getScriptPath :: FilePath -> IO FilePath
getScriptPath script =
    getDataFileName ("scripts/" ++ script)

-- parsers --

parseApplescriptLog :: String -> [String]
parseApplescriptLog = splitOnWithPrefix "|||" . dropWhileEnd (== '\n')
  where
    splitOnWithPrefix sep = splitOn sep . dropPrefix sep

parseLoggedPhotos :: String -> Map String (Set String)
parseLoggedPhotos str = foldersToMap parsed
  where
    splitted = splitOn ";;;" <$> parseApplescriptLog str
    parsed = fmap parseOneAlbum splitted
    parseOneAlbum [] = error "PARSE error from logging photos: no album name or files"
    parseOneAlbum (album : files) = (album, files)

foldersToMap :: [(String, [String])] -> Map String (Set String)
foldersToMap = Map.fromList . fmap (fmap Set.fromList)
