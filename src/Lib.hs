module Lib (new, run) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Semigroup ((<>))
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

gui :: Env -> IO ()
gui env = do
  print "not yet implemented gui"

setOrCreateDirectory :: Env -> IO ()
setOrCreateDirectory env = do
  setCurrentDirectory (ssdLib env)
  let yearDir = "./" ++ year env
  createDirectoryIfMissing False yearDir
  setCurrentDirectory yearDir

createStructure :: Env -> IO ()
createStructure env = do
  createDirectory $ folderName env
  createDirectory $ folderName env ++ "/" ++ jpgFolderName env
  createDirectory $ folderName env ++ "/" ++ rawFolderName env
  createDirectory $ folderName env ++ "/" ++ exportFolderName env
  createDirectory $ folderName env ++ "/" ++ movieFolderName env

transferPhotos :: Env -> Transfer -> IO ()
transferPhotos env AllTransfer = do
  print "not yet implemented"
transferPhotos env (RangeTransfer from to) = do
  print "not yet implemented"

---------------
--  Helpers  --
---------------

getYear :: IO String
getYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

getFolderName :: Maybe String -> IO String
getFolderName Nothing = error "not yet implemented: not able to fetch list of folders and let the user choose an appropriate"
getFolderName (Just name) = do
  prefix <- formatTime defaultTimeLocale "%Y-%m-%d - " <$> getCurrentTime
  let cleanName = capitalize . trim $ name
  return $ prefix ++ cleanName

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
  , jpgFolderName :: String
  , rawFolderName :: String
  , exportFolderName :: String
  , movieFolderName :: String
  , year :: String
  , folderName :: String
  }
  deriving (Show, Eq)

getEnv :: Maybe String -> IO Env
getEnv mName = do
  year <- getYear
  folderName <- getFolderName mName
  return $
    Env
      { sdLib = "/Volumes/Untitled/DCIM/147_FUJI/"
      , ssdLib = "/Volumes/EirikT5/Pictures/Fuji/"
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
  print "----------OPTIONS----------"
  print command
  env <-
    getEnv
      ( case command of
          Create name -> Just name
          Transfer _ -> Nothing
          CreateAndTransfer name _ -> Just name
          GUI -> Nothing
      )
  print "----------ENV----------"
  print env
  print "----------RUNCOMMAND----------"
  runCommand command env

runCommand :: Command -> Env -> IO ()
runCommand command env = case command of
  Create name -> do
    create env name
  Transfer transf -> do
    transfer env "" transf
  CreateAndTransfer name transf -> do
    createAndTransfer env name transf
  GUI -> do
    gui env

data Command
  = Create String
  | Transfer Transfer
  | CreateAndTransfer String Transfer
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
