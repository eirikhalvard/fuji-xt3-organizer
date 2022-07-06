module Lib where

import Cli
import Gui
import Types

import qualified Brick.BChan as BC
import qualified Brick.Main as M
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent as Thread
import Control.Monad.Extra (filterM, zipWithM_)
import Data.Char (isDigit, isSpace, toLower, toUpper)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sort)
import Data.List.Extra (chunksOf, dropPrefix, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Strict
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.FilePath
import System.Hclip (setClipboard)
import System.PosixCompat.Files (getFileStatus)
import System.Process.Extra (readProcess)

getLoggedPhotos :: IO (Map String (Set String))
getLoggedPhotos = parseLoggedPhotos <$> readProcess "/Users/n647546/Drive/Skole/Prosjekter/Haskell/fuji/scripts/logphotos" [] ""

parseLoggedPhotos :: String -> Map String (Set String)
parseLoggedPhotos str = foldersToMap parsed
 where
  splitted = splitOn ";;;" <$> splitOnWithPrefix "|||" str
  parsed = fmap parseOneAlbum splitted
  parseOneAlbum [] = error "PARSE error from logging photos: no album name or files"
  parseOneAlbum (album : files) = (album, files)
  splitOnWithPrefix sep = splitOn sep . dropPrefix sep

foldersToMap :: [(String, [String])] -> Map String (Set String)
foldersToMap = Map.fromList . fmap (fmap S.fromList)

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
 where
  checkExistence filepath = do
    fp <- canonicalizePath filepath
    exists <- doesDirectoryExist fp
    if exists
      then logEvent env $ "Folder " ++ filepath ++ " exists"
      else logEvent env $ "Folder " ++ filepath ++ " does not exist"

updateFolders :: Env -> IO ()
updateFolders env = do
  loggedPhotos <- getLoggedPhotos
  currentPhotos <- getPhotosFromFolder env
  logEvent env "diffing"
  let diffResult = diffPhotos currentPhotos loggedPhotos
  logEvent env "diffed"
  logEvent env "work to do:"
  updateFromDiff env diffResult

getPhotosFromFolder :: Env -> IO (Map String (Set String))
getPhotosFromFolder env = fmap (S.map fst) <$> createExportMap env

getPhotosFromDb :: Env -> IO (Map String (Set String))
getPhotosFromDb env = undefined

writeToDb :: Map String (Set String) -> IO ()
writeToDb = undefined

updateFromDiff :: Env -> Map String (Set String, Set String) -> IO ()
updateFromDiff env diffResult = do
  logEvent env "updating ........"
  mapM_
    ( \(name, (toRemove, toAdd)) -> do
        let toFolder = exportLib env ++ name
            removeFilePaths = (\filename -> toFolder ++ "/" ++ filename) <$> S.toList toRemove
        createDirectoryIfMissing False toFolder
        mapM_ (addToFolder env toFolder) $ S.toList toAdd
    )
    $ Map.toList diffResult

addToFolder :: Env -> FilePath -> String -> IO ()
addToFolder env toFolder filename = do
  matching <- matchingFiles
  case matching of
    [fromPath] -> do
      let toPath = toFolder ++ "/" ++ filename
      logEvent env $ "COPYING " ++ filename ++ " to " ++ toFolder
      copyFile fromPath toPath
    [] -> logEvent env ("No matching folder for " ++ filename)
    xs -> logEvent env ("Several matches for file " ++ unwords xs)
 where
  relevantFolders :: IO [FilePath]
  relevantFolders =
    case filename of
      (y1 : y2 : m1 : m2 : d1 : d2 : '_' : rest) | all isDigit [y1, y2, m1, m2, d1, d2] -> do
        let year = "20" ++ [y1, y2]
            month = [m1, m2]
            day = [d1, d2]
            ssdBaseDir = ssdBaseLib env ++ year ++ "/"
            folderPrefix = concat [year, "-", month, "-", day]
        withCurrentDirectory ssdBaseDir $ do
          folders <- System.Directory.listDirectory "."
          let relevant = filter (isPrefixOf folderPrefix) folders
          let fullPath = (ssdBaseDir ++) <$> relevant
          return fullPath
      _ -> do
        return [] -- todo (check both years 2021 and 2022)
  potentialFiles :: IO [FilePath]
  potentialFiles =
    concatMap
      ( \f ->
          [ f ++ "/" ++ jpgFolderName env ++ "/" ++ filename
          , f ++ "/" ++ exportFolderName env ++ "/" ++ filename
          ]
      )
      <$> relevantFolders
  matchingFiles :: IO [FilePath]
  matchingFiles = potentialFiles >>= filterM doesFileExist

-- returns map from name to tuple (toRemove, toAdd), where toRemove is titles to remove
-- and toAdd, is titles to add.
diffPhotos ::
  Map String (Set String) ->
  Map String (Set String) ->
  Map String (Set String, Set String)
diffPhotos =
  merge
    (mapMissing (\k inCurrent -> (S.empty, S.empty))) -- dont remove old folders
    (mapMissing (\k inLogged -> (S.empty, inLogged))) -- nothing to remove, add all
    ( zipWithMatched
        ( \k inCurrent inLogged ->
            let toRemove = S.difference inCurrent inLogged
                toAdd = S.difference inLogged inCurrent
             in (toRemove, toAdd)
        )
    )

gui :: Env -> IO ()
gui env = return ()

showExport :: Env -> IO ()
showExport env = do
  exportInfo <- createExportMap env
  showExportMap exportInfo

showExportMap :: Map String (Set (String, Integer)) -> IO ()
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
    . Map.toList

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

createExportMap :: Env -> IO (Map String (Set (String, Integer)))
createExportMap env = do
  setCurrentDirectory (exportLib env)
  let excludeFolders = [".DS_Store", "iPod Photo Cache"]
  folders <- System.Directory.listDirectory "."
  let relevantFolders = filter (`notElem` excludeFolders) folders
  Map.fromList <$> mapM getFolderEntry relevantFolders

getFolderEntry :: FilePath -> IO (String, Set (String, Integer))
getFolderEntry filepath = do
  let baseName = takeBaseName filepath
  infoSet <- withCurrentDirectory filepath $ getFolderInfo "."
  return (baseName, infoSet)

getFolderInfo :: FilePath -> IO (Set (String, Integer))
getFolderInfo filepath = do
  entries <- listDirectory "."
  let excludeEntries = [".DS_Store"]
      filteredEntries = filter (`notElem` excludeEntries) entries
  information <- mapM getFileInfo filteredEntries
  return $ S.fromList information

getFileInfo :: FilePath -> IO (String, Integer)
getFileInfo filepath = do
  size <- getFileSize filepath
  -- let baseName = takeBaseName filepath
  return (filepath, size)

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
transferPhotos env transfer = do
  dirs <- getSDDirectories env
  mapM_ (transferBatch env transfer) dirs

transferBatch :: Env -> Transfer -> FilePath -> IO ()
transferBatch env transfer path = do
  filenames <- fetchFilenames
  zipWithM_
    (transferSingle env path)
    [fromIntegral n / fromIntegral (length filenames) | n <- [1 ..]]
    filenames
 where
  fetchFilenames =
    case transfer of
      AllTransfer ->
        listDirectory path
      (RangeTransfer from to) ->
        filter (isBetweenRange from to . takeBaseName) <$> listDirectory path
  isBetweenRange from to filename =
    let count :: Int
        count = read $ filter isDigit filename
     in from <= count && count <= to

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
    then logEvent env ("SKIPPING EXISTING FILE (" ++ filename ++ ")")
    else
      logEvent env ("COPYING FILE " ++ filename)
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
      , ssdBaseLib = "/Volumes/EirikT5/Pictures/Fuji/"
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
getName UpdateFolders = Nothing
getName ShowExport = Nothing

event :: Env -> ApplicationEvent -> IO ()
event env = BC.writeBChan (eventChan env)

logEvent :: Env -> String -> IO ()
logEvent env = event env . AppendLogList

runCommand :: Command -> Env -> IO ()
runCommand command env =
  case command of
    Create name -> runWithGui (CreateState name) IsRunning $ do
      create env name
    Transfer transf -> runWithGui (TransferState transf 0.0) IsRunning $ do
      transfer env "" transf
    CreateAndTransfer name transf -> runWithGui (CreateAndTransferState name transf 0.0) IsRunning $ do
      createAndTransfer env name transf
    Info -> runWithGui InfoState IsQuitable $ do
      showInfo env
    GUI -> runWithGui GUIState IsQuitable $ do
      gui env
    UpdateFolders -> runWithGui UpdateFoldersState IsRunning $ do
      updateFolders env
    ShowExport -> runWithGui ShowExportState IsRunning $ do
      showExport env
 where
  runWithGui cmdState quitable program =
    runGui
      (eventChan env)
      (State env cmdState quitable [])
      (program <* event env Finished)
