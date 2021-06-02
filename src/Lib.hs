module Lib (new) where

import Data.Char (isSpace, toLower, toUpper)
import Data.Time.Clock
import Data.Time.Format
import System.Directory

-----------------
--  Constants  --
-----------------

t5Lib :: FilePath
t5Lib = "/Volumes/EirikT5/Pictures/Fuji/"

jpgFolderName :: String
jpgFolderName = "01_JPG"

rawFolderName :: String
rawFolderName = "02_RAW"

exportFolderName :: String
exportFolderName = "03_EXPORT"

----------------
--  Fuji New  --
----------------

new :: IO ()
new = do
  -- set directory to the external library
  setCurrentDirectory t5Lib

  -- set directory to the right year
  year <- getYear
  let yearDir = "./" ++ year
  createDirectoryIfMissing False yearDir
  setCurrentDirectory yearDir

  -- create folder structure
  name <- capitalize . trim <$> getLine
  folderName <- getFolderName name
  createDirectory folderName
  createDirectory $ folderName ++ "/" ++ jpgFolderName
  createDirectory $ folderName ++ "/" ++ rawFolderName
  createDirectory $ folderName ++ "/" ++ exportFolderName

getYear :: IO String
getYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

getFolderName :: String -> IO String
getFolderName name = do
  prefix <- formatTime defaultTimeLocale "%Y-%m-%d | " <$> getCurrentTime
  return $ prefix ++ name

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


