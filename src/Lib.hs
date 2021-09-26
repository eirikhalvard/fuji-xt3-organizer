module Lib (new, run) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Semigroup ((<>))
import Data.Time.Clock
import Data.Time.Format
import Options.Applicative
import System.Directory

-----------------
--  Constants  --
-----------------

----------------
--  Fuji New  --
----------------

new :: IO ()
new = do
  undefined

-- -- set directory to the external library
-- setCurrentDirectory t5Lib

-- -- set directory to the right year
-- year <- getYear
-- let yearDir = "./" ++ year
-- createDirectoryIfMissing False yearDir
-- setCurrentDirectory yearDir

-- -- create folder structure
-- name <- capitalize . trim <$> getLine
-- folderName <- getFolderName name
-- createDirectory folderName
-- createDirectory $ folderName ++ "/" ++ jpgFolderName
-- createDirectory $ folderName ++ "/" ++ rawFolderName
-- createDirectory $ folderName ++ "/" ++ exportFolderName

getYear :: IO String
getYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

getFolderName :: Maybe String -> IO String
getFolderName Nothing = error "not yet implemented: not able to fetch list of folders and let the user choose an appropriate"
getFolderName (Just name) = do
  prefix <- formatTime defaultTimeLocale "%Y-%m-%d - " <$> getCurrentTime
  let cleanName = capitalize . trim $ name
  return $ prefix ++ cleanName

---------------
--  Helpers  --
---------------

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : fmap toLower xs

-- Usage:
-- fuji --help
-- fuji transfer --from [from] --to [to]
-- fuji transfer --all
-- fuji createAndTransfer
-- create new folder, transfer photos to folder,

data Env = Env
  { sdLib :: FilePath,
    t5Lib :: FilePath,
    jpgFolderName :: String,
    rawFolderName :: String,
    exportFolderName :: String,
    movieFolderName :: String,
    year :: String,
    folderName :: String
  }
  deriving (Show, Eq)

getEnv :: Maybe String -> IO Env
getEnv mName = do
  year <- getYear
  folderName <- getFolderName mName
  return $
    Env
      { sdLib = "/Volumes/Untitled/DCIM/147_FUJI/",
        t5Lib = "/Volumes/EirikT5/Pictures/Fuji/",
        jpgFolderName = "01_JPG",
        rawFolderName = "02_RAW",
        exportFolderName = "03_EXPORT",
        movieFolderName = "04_MOV",
        year = year,
        folderName = folderName
      }

run :: IO ()
run = execute (Create "   checky checkeroo")

execute :: Command -> IO ()
execute (Create name) = do
  env <- getEnv (Just name)
  print "----------ENV----------"
  print env

  options <- execParser opts
  print "----------OPTIONS----------"
  print options

  error "not finished implementing"
execute (Transfer transfer) = undefined
execute (CreateAndTransfer name transfer) = undefined

data Command
  = Create String
  | Transfer Transfer
  | CreateAndTransfer String Transfer
  deriving (Show, Eq)

data Transfer
  = RangeTransfer Int Int
  | AllTransfer
  deriving (Show, Eq)

------------ Parser

opts :: ParserInfo Command
opts =
  info
    (createAndTransferParser <**> helper)
    ( fullDesc
        <> progDesc "Helps create folders and transfer photos between SD card and external SSD"
        <> header "Fujifilm X-T3 Transfer Script"
    )

createAndTransferParser :: Parser Command
createAndTransferParser =
  CreateAndTransfer
    <$> strOption
      ( long "name"
          <> metavar "SESSION_NAME"
          <> help "Name of the photo session"
      )
    <*> transferParser

transferParser :: Parser Transfer
transferParser =
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
