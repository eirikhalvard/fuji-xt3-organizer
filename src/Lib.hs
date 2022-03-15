{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Cli
import Gui
import Types

import qualified Brick.BChan as BC
import qualified Brick.Main as M
import Control.Concurrent (threadDelay)
import Control.Monad.Extra (zipWithM_)
import Data.Char (isSpace, toLower, toUpper)
import Data.List.Extra (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Format
import Foreign.AppleScript
import System.Directory
import System.FilePath
import System.Hclip (setClipboard)
import System.PosixCompat.Files (getFileStatus)

runApplescriptTest :: IO ()
runApplescriptTest = do
  runScript
    [applescript| 
      open location $value{location}$ 
    |]

location = "https://github.com/"

create :: Env -> String -> IO ()
create env name = do
  setOrCreateDirectory env
  createStructure env
  mapM_ setClipboard (folderName env)

transfer :: Env -> String -> Transfer -> IO ()
transfer env name transf = do
  setOrCreateDirectory env
  transferPhotos env transf

createAndTransfer :: Env -> String -> Transfer -> IO ()
createAndTransfer env name transf = do
  setOrCreateDirectory env
  createStructure env
  transferPhotos env transf
  mapM_ setClipboard (folderName env)

showInfo :: Env -> IO ()
showInfo env = do
  checkExistence (sdLib env)
  checkExistence (ssdLib env)
  checkExistence (exportLib env)
  runApplescriptTest
 where
  checkExistence filepath = do
    fp <- canonicalizePath filepath
    exists <- doesDirectoryExist fp
    if exists
      then putStrLn $ "Folder " ++ filepath ++ " exists"
      else putStrLn $ "Folder " ++ filepath ++ " does not exist"

gui :: Env -> IO ()
gui env = return ()

showExport :: Env -> IO ()
showExport env = do
  exportInfo <- createExportMap env
  showExportMap exportInfo

showExportMap :: [(String, Set (String, Integer))] -> IO ()
showExportMap =
  mapM_
    ( \(name, info) ->
        putStrLn $
          name
            ++ ": "
            ++ show (length info)
            ++ " number of items, total bytes: "
            ++ showNumBytes (sum (S.map snd info))
    )

showNumBytes :: Integer -> String
showNumBytes n = prefix ++ getPostfix (length rest `div` 3)
 where
  numString = show n
  numLength = length numString
  numDigitsToDisplay = ((numLength -1) `mod` 3) + 1
  (prefix, rest) = splitAt numDigitsToDisplay numString
  getPostfix 0 = ""
  getPostfix 1 = "K"
  getPostfix 2 = "M"
  getPostfix 3 = "G"
  getPostfix 4 = "T"
  getPostfix _ = error "get fucked"

createExportMap :: Env -> IO [(String, Set (String, Integer))]
createExportMap env = do
  setCurrentDirectory (exportLib env)
  let excludeFolders = [".DS_Store", "iPod Photo Cache", "Gifs"]
  folders <- System.Directory.listDirectory "."
  let relevantFolders = filter (`notElem` excludeFolders) folders
  mapM getFolderEntry relevantFolders

getFolderEntry :: FilePath -> IO (String, Set (String, Integer))
getFolderEntry filepath = do
  let baseName = takeBaseName filepath
  infoSet <- withCurrentDirectory filepath $ getFolderInfo "."
  return (baseName, infoSet)

getFolderInfo :: FilePath -> IO (Set (String, Integer))
getFolderInfo filepath = do
  entries <- listDirectory "."
  information <- mapM getFileInfo entries
  return $ S.fromList information

getFileInfo :: FilePath -> IO (String, Integer)
getFileInfo filepath = do
  size <- getFileSize filepath
  let baseName = takeBaseName filepath
  return (baseName, size)

setOrCreateDirectory :: Env -> IO ()
setOrCreateDirectory env = do
  createDirectoryIfMissing False (ssdLib env)
  setCurrentDirectory (ssdLib env)

createStructure :: Env -> IO ()
createStructure env = do
  createOrCrash $ folderName env
  createOrCrash $ jpgPath env
  createOrCrash $ rawPath env
  createOrCrash $ exportPath env
  createOrCrash $ moviePath env
 where
  createOrCrash Nothing = error "Cant create directories since no folder is provided"
  createOrCrash (Just path) = createDirectory path

transferPhotos :: Env -> Transfer -> IO ()
transferPhotos env AllTransfer = do
  dirs <- getSDDirectories env
  mapM_ (transferBatch env) dirs
transferPhotos env (RangeTransfer from to) = do
  putStrLn "not yet implemented"

transferBatch :: Env -> FilePath -> IO ()
transferBatch env path = do
  filenames <- listDirectory path
  zipWithM_
    (transferSingle env path)
    [fromIntegral n / fromIntegral (length filenames) | n <- [1 ..]]
    filenames

transferSingle :: Env -> FilePath -> Float -> String -> IO ()
transferSingle env path progress filename = do
  let destinationM =
        case withExtension filename of
          Jpg _ -> jpgPath env
          Raw _ -> rawPath env
          Movie _ -> moviePath env
          Export _ -> exportPath env
      destination = fromMaybe (error "Folder is not present, can't transfer") destinationM
      canonicalFilename = canonicalFilenamePrefix env ++ "_" ++ filename
      destinationAbsolute = ssdLib env ++ destination ++ "/" ++ canonicalFilename
      originAbsolute = path ++ "/" ++ filename
  exists <- doesPathExist destinationAbsolute
  if exists
    then print ("SKIPPING EXISTING FILE (" ++ filename ++ ")")
    else
      print ("COPYING FILE " ++ filename)
        >> copyFileWithMetadata originAbsolute destinationAbsolute
        >> removeFile originAbsolute
        >> event env (TransferProgress progress)

getExtension :: String -> String
getExtension = tail . dropWhile (/= '.')

withExtension :: String -> Extension
withExtension filename = extended
 where
  ext = getExtension filename
  extended
    | isJpg ext = Jpg filename
    | isRaw ext = Raw filename
    | isMovie ext = Movie filename
    | otherwise = Export filename

isJpg, isRaw, isExport, isMovie :: String -> Bool
isJpg extension = extension `elem` ["jpg", "jpeg", "JPG", "JPEG"]
isRaw extension = extension `elem` ["raw", "raf", "RAW", "RAF"]
isExport extension = not (isJpg extension || isRaw extension || isMovie extension)
isMovie extension = extension `elem` ["mov", "MOV", "mp4", "MPEG4"]

---------------
--  Helpers  --
---------------

getYear :: IO String
getYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

getFolderName :: Maybe String -> IO (Maybe String)
getFolderName = traverse constructFolderName
 where
  constructFolderName name = do
    prefix <- formatTime defaultTimeLocale "%Y-%m-%d - " <$> getCurrentTime
    let cleanName = capitalize . trim $ name
    return $ prefix ++ cleanName

getSDDirectories :: Env -> IO [FilePath]
getSDDirectories env = fmap (sdLib env ++) <$> listDirectory (sdLib env)

trim :: String -> String
trim = f . f
 where
  f = reverse . dropWhile isSpace

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : fmap toLower xs

jpgPath, rawPath, exportPath, moviePath :: Env -> Maybe FilePath
jpgPath env = (\base -> base ++ "/" ++ jpgFolderName env) <$> folderName env
rawPath env = (\base -> base ++ "/" ++ rawFolderName env) <$> folderName env
exportPath env = (\base -> base ++ "/" ++ exportFolderName env) <$> folderName env
moviePath env = (\base -> base ++ "/" ++ movieFolderName env) <$> folderName env

getEnv :: Maybe String -> BC.BChan ApplicationEvent -> IO Env
getEnv mName chan = do
  year <- getYear
  canonicalFilenamePrefix <- formatTime defaultTimeLocale "%y%m%d" <$> getCurrentTime
  folderName <- getFolderName mName
  homeDirectory <- getHomeDirectory
  return $
    Env
      { sdLib = "/Volumes/Untitled/DCIM/"
      , ssdLib = "/Volumes/EirikT5/Pictures/Fuji/" ++ year ++ "/"
      , exportLib = homeDirectory ++ "/Pictures/Export/"
      , jpgFolderName = "01_JPG"
      , rawFolderName = "02_RAW"
      , exportFolderName = "03_EXPORT"
      , movieFolderName = "04_MOV"
      , year = year
      , folderName = folderName
      , canonicalFilenamePrefix = canonicalFilenamePrefix
      , eventChan = chan
      }

getName :: Command -> Maybe String
getName (Create name) = Just name
getName (Transfer _) = Nothing
getName (CreateAndTransfer name _) = Just name
getName Info = Nothing
getName GUI = Nothing
getName ShowExport = Nothing

event :: Env -> ApplicationEvent -> IO ()
event env = BC.writeBChan (eventChan env)

runCommand :: Command -> Env -> IO ()
runCommand command env =
  case command of
    Create name -> runWithGui (CreateState name) $ do
      create env name
    Transfer transf -> runWithGui (TransferState transf 0.0) $ do
      transfer env "" transf
    CreateAndTransfer name transf -> runWithGui (CreateAndTransferState name transf 0.0) $ do
      createAndTransfer env name transf
    Info -> runWithGui InfoState $ do
      showInfo env
    GUI -> runWithGui GUIState $ do
      gui env
    ShowExport -> runWithGui ShowExportState $ do
      showExport env
 where
  runWithGui cmdState program =
    runGui
      (eventChan env)
      (State env cmdState)
      (program <* threadDelay 2000000 <* event env Finished)
