module Lib where

import Cli
import Gui
import Scripts
import Types

import qualified Brick.BChan as BC
import qualified Brick.Main as M
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent as Thread
import Control.Monad.Extra (concatMapM, filterM, zipWithM_)
import Data.Char (isDigit, isSpace, toLower, toUpper)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, sort)
import Data.List.Extra (chunksOf, dropEnd, dropPrefix, splitOn, takeEnd)
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

create :: Env -> String -> IO ()
create env name = do
    setOrCreateDirectory env
    createStructure env
    createPhotosAlbum env

transfer :: Env -> String -> Transfer -> IO ()
transfer env name transf = do
    setOrCreateDirectory env
    transferPhotos env transf
    importToPhotos env transf

createAndTransfer :: Env -> String -> Transfer -> IO ()
createAndTransfer env name transf = do
    setOrCreateDirectory env
    createStructure env
    createPhotosAlbum env
    transferPhotos env transf
    importToPhotos env transf

showInfo :: Env -> IO ()
showInfo env = do
    checkExistence (sdLib env)
    checkExistence (ssdBaseLib env)
    checkExistence (exportLib env)
  where
    checkExistence filepath = do
        exists <- directoryExists filepath
        let message =
                if exists
                    then "Folder " ++ filepath ++ " exists"
                    else "Folder " ++ filepath ++ " does not exist"
        logEvent env message

directoryExists :: FilePath -> IO Bool
directoryExists fp =
    doesDirectoryExist =<< canonicalizePath fp

updateFolders :: Env -> IO ()
updateFolders env = do
    logEvent env "Running logphotos applescript. Fetching album information from Photos app"
    loggedPhotos <- runLogPhotos
    logEvent env "Fetching information from export folder on the file system"
    currentPhotos <- getPhotosFromFolder env
    logEvent env "diffing"
    let diffResult = diffPhotos currentPhotos loggedPhotos
    logEvent env "diffed"
    logEvent env "work to do:"
    updateFromDiff env diffResult

getPhotosFromFolder :: Env -> IO (Map String (Set String))
getPhotosFromFolder env = fmap (S.map fst) <$> createExportMap env

updateFromDiff :: Env -> Map String (Set String, Set String) -> IO ()
updateFromDiff env diffResult = do
    logEvent env "updating ........"
    mapM_
        ( \(name, (toRemove, toAdd)) -> do
            let toFolder = exportLib env ++ name
                removeFilePaths = (\filename -> toFolder ++ "/" ++ filename) <$> S.toList toRemove
            createDirectoryIfMissing False toFolder
            mapM_ (addToFolder env toFolder) $ S.toList toAdd
            mapM_ (\removeFilePath -> logEvent env $ "SHOULD REMOVE: " ++ removeFilePath) removeFilePaths
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
        [] -> logEvent env ("Could not find source file " ++ filename ++ ". Could not copy to folder " ++ toFolder ++ ". Try switching harddrive")
        xs -> do
            logEvent env ("Several matches for file " ++ filename ++ " to " ++ toFolder)
            let tmp = toFolder ++ "/tmp"
            createDirectoryIfMissing False tmp
            mapM_
                ( \(fromPath, i) -> do
                    let (base, extension) = span (/= '.') filename
                    let fn = base ++ "_" ++ show i ++ extension
                    let toPath = tmp ++ "/" ++ fn
                    logEvent env $ "COPYING " ++ fn ++ " to " ++ tmp
                    copyFile fromPath toPath
                )
                $ zip xs [1 ..]
  where
    relevantFolders :: IO [FilePath]
    relevantFolders =
        case filename of
            (y1 : y2 : m1 : m2 : d1 : d2 : '_' : rest) | all isDigit [y1, y2, m1, m2, d1, d2] -> do
                -- filename: "yymmdd - some name"
                let year = "20" ++ [y1, y2]
                    month = [m1, m2]
                    day = [d1, d2]
                    ssdBaseDir = ssdBaseLib env ++ "/Pictures/Fuji/" ++ year ++ "/"
                    folderPrefix = concat [year, "-", month]
                folders <- listDirectoryIfExists ssdBaseDir
                let relevant = filter (isPrefixOf folderPrefix) folders
                let fullPath = (ssdBaseDir ++) <$> relevant
                return fullPath
            _ ->
                -- filename: legacy filename or custom filename
                concatMapM
                    ( \year ->
                        let ssdBaseDir = ssdBaseLib env ++ "/Pictures/Fuji/" ++ year ++ "/"
                         in fmap (ssdBaseDir ++) <$> listDirectoryIfExists ssdBaseDir
                    )
                    ["2021", "2022", "2023", "2024", "2025", "2026"]
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
        (mapMissing (\k currentFiles -> (S.empty, S.empty))) -- dont remove old folders
        (mapMissing (\k loggedFiles -> (S.empty, loggedFiles))) -- nothing to remove, add all
        ( zipWithMatched
            ( \k currentFiles loggedFiles ->
                let
                    normalize fn =
                        -- Since duplicates can happen, we need to consider 'file_1.JPG' and 'file.JPG' the same
                        let (base, extension) = span (/= '.') fn
                         in case takeEnd 2 base of
                                ['_', d] | isDigit d -> dropEnd 2 base ++ extension
                                _ -> fn
                    toRemove = S.filter (\fn -> normalize fn `S.notMember` loggedFiles && fn `S.notMember` loggedFiles) currentFiles
                    toAdd = S.difference loggedFiles (S.union currentFiles $ S.map normalize currentFiles)
                 in
                    (toRemove, toAdd)
            )
        )

listDirectoryIfExists :: FilePath -> IO [FilePath]
listDirectoryIfExists dir = do
    exists <- doesDirectoryExist dir
    if exists
        then System.Directory.listDirectory dir
        else pure []

gui :: Env -> IO ()
gui env = do
    mapM_ transferOne [1 .. num]
  where
    num = 40
    transferOne n = do
        threadDelay 40000 -- wait 0.4s per transfer
        let progress = TransferProgress (fromIntegral n / fromIntegral num)
        logEvent env $ "transfer test #" ++ show n
        event env progress

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
    numDigitsToDisplay = ((numLength - 1) `mod` 3) + 1
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
    let includeFolders = ["idafam", "Bangers", "Backgrounds", "Venner", "Street"]
    folders <- System.Directory.listDirectory "."
    let relevantFolders = filter (`elem` includeFolders) folders
    Map.fromList <$> mapM getFolderEntry relevantFolders

getFolderEntry :: FilePath -> IO (String, Set (String, Integer))
getFolderEntry filepath = do
    let baseName = takeBaseName filepath
    infoSet <- withCurrentDirectory filepath $ getFolderInfo "."
    return (baseName, infoSet)

getFolderInfo :: FilePath -> IO (Set (String, Integer))
getFolderInfo filepath = do
    entries <- listDirectory "."
    let excludeEntries = [".DS_Store", "tmp"]
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
    hardDriveExists <- directoryExists (ssdBaseLib env)
    if hardDriveExists
        then do
            createDirectoryIfMissing True (ssdLib env)
            setCurrentDirectory (ssdLib env)
        else do
            logEvent env "No hard drive connected"
            threadDelay 1500000
            event env Exit

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

createPhotosAlbum :: Env -> IO ()
createPhotosAlbum env = case folderName env of
    Nothing -> error "no folder name. something went wrong"
    Just fn -> do
        logEvent env "creating structure in photos album"
        loggedEvents <- runCreateFolder fn
        mapM_ (logEvent env) loggedEvents
        logEvent env "done creating structure in photos album"

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

importToPhotos :: Env -> Transfer -> IO ()
importToPhotos env (RangeTransfer s e) =
    logEvent env $
        "WARNING: importing a range of photos to Photos not supported. should be done manually for range("
            ++ show s
            ++ ", "
            ++ show e
            ++ ")"
importToPhotos env AllTransfer = case folderName env of
    Nothing -> error "no folder name. something went wrong"
    Just fn -> do
        case jpgPath env of
            Nothing -> error "no jpg path, something is wrong"
            Just jpgPart -> do
                let transferFolder = ssdLib env ++ jpgPart
                loggedEvents <- runTransferPhotos fn transferFolder
                mapM_ (logEvent env) loggedEvents

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

isJpg, isRaw, isMovie :: String -> Bool
isJpg extension = fmap toLower extension `elem` ["jpg", "jpeg", "png", "tiff", "gif", "bmp", "tif", "heic"]
isRaw extension = fmap toLower extension `elem` ["raw", "raf", "cr2", "cr3", "psd", "ai"]
isMovie extension = fmap toLower extension `elem` ["mov", "mp4", "mpeg4", "avi"]

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
    hardDrive <- do
        entries <- listDirectory "/Volumes/"
        let relevant = filter (isPrefixOf "EirikT") entries
            drive = head (relevant ++ ["UnknownHardDrive"])
        return $ "/Volumes/" ++ drive

    return $
        Env
            { sdLib = "/Volumes/Untitled/DCIM/"
            , ssdLib = hardDrive ++ "/Pictures/Fuji/" ++ year ++ "/"
            , ssdBaseLib = hardDrive
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

startProgram :: Env -> IO ()
startProgram env = do
    event env . SDStatus =<< directoryExists (sdLib env)
    event env . SSDStatus =<< directoryExists (ssdBaseLib env)
    event env . ExportStatus =<< directoryExists (exportLib env)

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
        GUI -> runWithGui GUIState IsRunning $ do
            gui env
        UpdateFolders -> runWithGui UpdateFoldersState IsRunning $ do
            updateFolders env
        ShowExport -> runWithGui ShowExportState IsRunning $ do
            showExport env
  where
    fs = FolderStatus False False False
    runWithGui cmdState quitable program =
        runGui
            (eventChan env)
            (State env cmdState quitable [] fs)
            (startProgram env *> program <* event env Finished)
