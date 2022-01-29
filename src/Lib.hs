module Lib (run) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import System.Directory

create :: Env -> String -> IO ()
create env name = do
  setOrCreateDirectory env
  createStructure env

transfer :: Env -> String -> Transfer -> IO ()
transfer env name transf = do
  setOrCreateDirectory env
  transferPhotos env transf

createAndTransfer :: Env -> String -> Transfer -> IO ()
createAndTransfer env name transf = do
  setOrCreateDirectory env
  createStructure env
  transferPhotos env transf

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
      then print $ "Folder " ++ filepath ++ " exists"
      else print $ "Folder " ++ filepath ++ " does not exist"

gui :: Env -> IO ()
gui env = do
  print "not yet implemented gui"

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
  print "not yet implemented"

transferBatch :: Env -> FilePath -> IO ()
transferBatch env path = do
  filenames <- listDirectory path
  mapM_ (transferSingle env path) filenames

transferSingle :: Env -> FilePath -> String -> IO ()
transferSingle env path filename = do
  let destinationM =
        case withExtension filename of
          Jpg _ -> jpgPath env
          Raw _ -> rawPath env
          Movie _ -> moviePath env
          Export _ -> exportPath env
      destination = fromMaybe (error "Folder is not present, can't transfer") destinationM
      destinationAbsolute = ssdLib env ++ destination ++ "/" ++ filename
      originAbsolute = path ++ "/" ++ filename
  exists <- doesPathExist destinationAbsolute
  if exists
    then print ("SKIPPING EXISTING FILE (" ++ filename ++ ")")
    else
      print ("COPYING FILE " ++ filename)
        >> copyFileWithMetadata originAbsolute destinationAbsolute
        >> removeFile originAbsolute

getExtension :: String -> String
getExtension = tail . dropWhile (/= '.')

data Extension = Jpg String | Raw String | Export String | Movie String

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

data Env = Env
  { sdLib :: FilePath
  , ssdLib :: FilePath
  , exportLib :: FilePath
  , jpgFolderName :: String
  , rawFolderName :: String
  , exportFolderName :: String
  , movieFolderName :: String
  , year :: String
  , folderName :: Maybe String
  }
  deriving (Show, Eq)

jpgPath, rawPath, exportPath, moviePath :: Env -> Maybe FilePath
jpgPath env = (\base -> base ++ "/" ++ jpgFolderName env) <$> folderName env
rawPath env = (\base -> base ++ "/" ++ rawFolderName env) <$> folderName env
exportPath env = (\base -> base ++ "/" ++ exportFolderName env) <$> folderName env
moviePath env = (\base -> base ++ "/" ++ movieFolderName env) <$> folderName env

getEnv :: Maybe String -> IO Env
getEnv mName = do
  year <- getYear
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
      }

run :: IO ()
run = execute

execute :: IO ()
execute = do
  command <- execParser opts
  env <- getEnv (getName command)
  runCommand command env

getName :: Command -> Maybe String
getName (Create name) = Just name
getName (Transfer _) = Nothing
getName (CreateAndTransfer name _) = Just name
getName Info = Nothing
getName GUI = Nothing

runCommand :: Command -> Env -> IO ()
runCommand command env = case command of
  Create name -> do
    create env name
  Transfer transf -> do
    transfer env "" transf
  CreateAndTransfer name transf -> do
    createAndTransfer env name transf
  Info -> do
    showInfo env
  GUI -> do
    gui env

test :: IO ()
test = do
  let command = CreateAndTransfer " sterk loking" AllTransfer
  env <- getEnv $ getName command
  runCommand command env

data Command
  = Create String
  | Transfer Transfer
  | CreateAndTransfer String Transfer
  | Info
  | GUI
  deriving (Show, Eq)

data Transfer
  = RangeTransfer Int Int
  | AllTransfer
  deriving (Show, Eq)

------------ Parser

opts :: ParserInfo Command
opts =
  info
    ( hsubparser
        ( command "create" (info createParser (progDesc "Add a folder to the SSD"))
            <> command "transfer" (info transferParser (progDesc "Transfer files from the SD card to the specified location"))
            <> command "createAndTransfer" (info createAndTransferParser (progDesc "Create a folder on the SSD and transfer the files from the SD card"))
            <> command "info" (info informationParser (progDesc "Displays information"))
            <> command "gui" (info guiParser (progDesc "Launch the graphical user interface"))
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc "Helps create folders and transfer photos between SD card and external SSD"
        <> header "Fujifilm X-T3 Transfer Script"
    )

createParser :: Parser Command
createParser =
  Create <$> createComponentParser

transferParser :: Parser Command
transferParser =
  Transfer <$> transferComponentParser

createAndTransferParser :: Parser Command
createAndTransferParser =
  CreateAndTransfer
    <$> createComponentParser
    <*> transferComponentParser

informationParser :: Parser Command
informationParser =
  pure Info

guiParser :: Parser Command
guiParser = pure GUI

createComponentParser :: Parser String
createComponentParser =
  strOption
    ( long "name"
        <> metavar "SESSION_NAME"
        <> help "Name of the photo session"
    )

transferComponentParser :: Parser Transfer
transferComponentParser =
  transferAllParser <|> transferRangeParser

transferAllParser :: Parser Transfer
transferAllParser =
  flag' AllTransfer (long "all" <> help "transfer all photos from SD card")

transferRangeParser :: Parser Transfer
transferRangeParser =
  RangeTransfer
    <$> option
      auto
      ( long "from"
          <> short 'F'
          <> metavar "INT"
          <> help "Number to start transfering from"
      )
    <*> option
      auto
      ( long "to"
          <> short 'T'
          <> metavar "INT"
          <> help "Number to start transfering to"
      )
