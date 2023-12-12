module Scripts where

import Data.List.Extra (chunksOf, dropPrefix, splitOn)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.Process.Extra (readProcess)

-- functions --

runCreateFolder :: String -> IO [String]
runCreateFolder folderName = runScript "createFolder" [folderName] parseApplescriptLog

runLogPhotos :: IO (Map String (Set String))
runLogPhotos = runScript "logphotos" [] parseLoggedPhotos

runTransferPhotos :: String -> String -> IO [String]
runTransferPhotos folderName transferFolder = runScript "transferPhotos" [folderName, transferFolder] parseApplescriptLog

-- runners --

runScript :: String -> [String] -> (String -> parsed) -> IO parsed
runScript scriptName inputParameters parser = do
    folder <- getScriptFolder
    response <- readProcess (folder ++ scriptName) inputParameters ""
    return $ parser response

getScriptFolder :: IO String
getScriptFolder = do
    homeDirectory <- getHomeDirectory
    return $ homeDirectory ++ "/Drive/Skole/Prosjekter/Haskell/fuji/scripts/"

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
